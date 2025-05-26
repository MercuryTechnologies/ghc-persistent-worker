module BuckProxy.Orchestration (
  WorkerExe (..),
  proxyServer,
  spawnGhcWorker,
) where

import BuckProxy.Util (dbg)
import qualified BuckWorker as Worker
import Common.Grpc (GrpcHandler (..), fromGrpcHandler)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Exception (throwIO, try)
import Control.Monad (void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Network.GRPC.Client (Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (fromMethods)
import Proto.Worker (Worker (..))
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)
import Types.BuckArgs (BuckArgs (workerTargetId), parseBuckArgs)
import Types.GhcHandler (WorkerMode (..))
import Types.Orchestration (
  Orchestration,
  PrimarySocketPath (..),
  ServerSocketPath (..),
  SocketDirectory (..),
  primarySocketIn,
  projectSocketDirectory,
  )

-- | Path to the worker executable, i.e. this program.
--- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

proxyHandler ::
  MVar (Map String PrimarySocketPath) ->
  WorkerExe ->
  Orchestration ->
  WorkerMode ->
  ServerSocketPath ->
  GrpcHandler
proxyHandler workerMap exe _omode wmode _socket =
  GrpcHandler \ commandEnv argv -> do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs commandEnv argv)
    let mtargetId = buckArgs.workerTargetId
    case mtargetId of
      Nothing -> throwIO (userError "No --worker-target-id passed")
      Just targetId -> do
        wmap <- takeMVar workerMap
        case Map.lookup targetId wmap of
          Nothing -> do
            let workerSocketDir = projectSocketDirectory targetId
            void $ try @IOError (createDirectoryIfMissing True workerSocketDir.path)
            primary <- spawnGhcWorker exe wmode workerSocketDir
            putMVar workerMap (Map.insert targetId primary wmap)
            let msg = "No primary socket for " ++ show targetId ++ ", so created it on " ++ primary.path
            pure ([msg], 1)
          Just primary -> do
            let msg = "Primary socket for " ++ show targetId ++ ": " ++ primary.path
            pure ([msg], 1)

grpcServerConfig :: FilePath -> ServerConfig
grpcServerConfig socketPath =
  ServerConfig
    { serverInsecure = Just (InsecureUnix socketPath)
    , serverSecure = Nothing
    }

-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to ghc-worker
proxyServer ::
  -- | mutable worker map (we spawn a new ghc-worker as a new target id arrives)
  MVar (Map String PrimarySocketPath) ->
  WorkerExe ->
  Orchestration ->
  WorkerMode ->
  ServerSocketPath ->
  IO ()
proxyServer workerMap exe omode wmode socket = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down buck-proxy on " ++ socket.path)
    Left (err :: IOError) -> do
      dbg ("buck-proxy on" ++ socket.path ++ " crashed" ++ show err)
      exitFailure
  where
    methods = fromGrpcHandler (proxyHandler workerMap exe omode wmode socket)
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
  IO PrimarySocketPath
spawnGhcWorker exe mode socketDir = do
  dbg ("Forking GHC server at " ++ primary.path)
  let workerModeFlag = case mode of
        WorkerOneshotMode -> []
        WorkerMakeMode -> ["--make"]
  proc <- spawnProcess exe.path (workerModeFlag ++ ["--serve", primary.path])
  waitForGhcWorker proc primary
  pure primary
  where
    primary = primarySocketIn socketDir
