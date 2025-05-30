module BuckProxy.Orchestration (
  WorkerExe (..),
  proxyServer,
  spawnGhcWorker,
) where

import BuckProxy.Util (dbg)
import qualified BuckWorker as Worker
import BuckWorker (ExecuteCommand, ExecuteResponse)
import Common.Grpc (commandEnv, streamingNotImplemented)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Exception (throwIO, try)
import Control.Monad (void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (
  Methods (..),
  fromMethods,
  mkClientStreaming,
  mkNonStreaming,
  )
import Proto.Worker (Worker (..))
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)
import Types.Args (TargetId)
import Types.BuckArgs (BuckArgs (workerTargetId), parseBuckArgs)
import Types.GhcHandler (WorkerMode (..))
import Types.Grpc (RequestArgs (..))
import Types.Orchestration (
  PrimarySocketPath (..),
  ServerSocketPath (..),
  SocketDirectory (..),
  extractTraceIdAndWorkerSpecId,
  primarySocketIn,
  projectSocketDirectory,
  )

-- | Path to the worker executable, i.e. this program.
--- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

data WorkerResource =
  WorkerResource {
    primarySocket :: PrimarySocketPath,
    processHandle :: ProcessHandle
  }

-- | Forward a request received from a client to another gRPC server and forward the response back,
-- prefixing the error messages so we know where the error originated.
forwardRequest ::
  Connection ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
forwardRequest connection req = withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
  sendFinalInput call req
  resp <- recvNextOutput call
  pure $
    resp
      & Fields.stderr
      %~ ("gRPC client error: " <>)

proxyHandler ::
  MVar (Map TargetId WorkerResource) ->
  WorkerExe ->
  WorkerMode ->
  FilePath ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
proxyHandler workerMap exe wmode basePath req = do
  let cmdEnv = commandEnv req.env
      argv = Text.unpack . decodeUtf8Lenient <$> req.argv
  buckArgs <- either (throwIO . userError) pure (parseBuckArgs cmdEnv (RequestArgs argv))
  case buckArgs.workerTargetId of
    Nothing -> throwIO (userError "No --worker-target-id passed")
    Just targetId -> do
      resource <-
        modifyMVar workerMap \wmap -> do
          case Map.lookup targetId wmap of
            Nothing -> do
              let workerSocketDir = projectSocketDirectory basePath targetId
              void $ try @IOError (createDirectoryIfMissing True workerSocketDir.path)
              resource <- spawnGhcWorker exe wmode workerSocketDir
              dbg $ "No primary socket for " ++ show targetId ++ ", so created it on " ++ resource.primarySocket.path
              pure (Map.insert targetId resource wmap, resource)
            Just resource -> do
              dbg $ "Primary socket for " ++ show targetId ++ ": " ++ resource.primarySocket.path
              pure (wmap, resource)
      withConnection def (ServerUnix resource.primarySocket.path) \connection ->
        forwardRequest connection req

grpcServerConfig :: FilePath -> ServerConfig
grpcServerConfig socketPath =
  ServerConfig
    { serverInsecure = Just (InsecureUnix socketPath)
    , serverSecure = Nothing
    }

-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to ghc-worker
proxyServer ::
  -- | mutable worker map (we spawn a new ghc-worker as a new target id arrives)
  MVar (Map TargetId WorkerResource) ->
  WorkerExe ->
  WorkerMode ->
  ServerSocketPath ->
  IO ()
proxyServer workerMap exe wmode socket = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down buck-proxy on " ++ socket.path)
    Left (err :: IOError) -> do
      dbg ("buck-proxy on" ++ socket.path ++ " crashed" ++ show err)
      exitFailure
  where
    (traceId, workerSpecId) = extractTraceIdAndWorkerSpecId socket.path
    base = traceId ++ "-" ++ workerSpecId
    methods :: Methods IO (ProtobufMethodsOf Worker)
    methods =
      Method (mkClientStreaming streamingNotImplemented) $
      Method (mkNonStreaming (proxyHandler workerMap exe wmode base)) $
      NoMoreMethods
    launch = do
      dbg ("Starting buck-proxy on " ++ socket.path)
      runServerWithHandlers def (grpcServerConfig socket.path) $ fromMethods methods

messageExecute :: Proto Worker.ExecuteCommand
messageExecute = defMessage

-- | How often the process should wait for 100ms and retry connecting to the GHC server after spawning a process.
maxRetries :: Int
maxRetries = 30

-- | Attempt to connect and send a gRPC message to the server starting up at the given socket.
waitPoll :: PrimarySocketPath -> IO ()
waitPoll socket =
  check maxRetries
  where
    check 0 = throwIO (userError "GHC server didn't respond within 3 seconds")
    check n =
      try connect >>= \case
        Right () -> pure ()
        Left (_ :: IOError) -> do
          threadDelay 100_000
          check (n - 1)

    -- The part that throws is in @withConnection@, so this has to be executed every time.
    connect =
      withConnection def (ServerUnix socket.path) \ connection ->
        withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call ->
          sendFinalInput call messageExecute <* recvNextOutput call

-- | Wait for a GHC server process to respond and check its exit code.
waitForGhcWorker :: ProcessHandle -> PrimarySocketPath -> IO ()
waitForGhcWorker ph socket = do
  dbg "Waiting for server"
  waitPoll socket
  dbg "Server is up"
  exitCode <- getProcessExitCode ph
  when (isJust exitCode) do
    dbg "Spawned process for the GHC server exited after starting up."

-- | Spawn a child process executing the worker executable (which usually is the same as this process), for the purpose
-- of running a GHC server to which all worker processes then forward their requests.
-- Afterwards, wait for the server to be responsive.
spawnGhcWorker ::
  WorkerExe ->
  WorkerMode ->
  SocketDirectory ->
  IO WorkerResource
spawnGhcWorker exe mode socketDir = do
  dbg ("Forking GHC server at " ++ primary.path)
  let workerModeFlag = case mode of
        WorkerOneshotMode -> []
        WorkerMakeMode -> ["--make"]
  proc <- spawnProcess exe.path (workerModeFlag ++ ["--serve", primary.path])
  waitForGhcWorker proc primary
  pure WorkerResource {primarySocket = primary, processHandle = proc}
  where
    primary = primarySocketIn socketDir
