module BuckProxy.Orchestration (
  GhcWorkerCommand (..),
  WorkerExe (..),
  WorkerResource (..),
  proxyServer,
  spawnGhcWorker,
) where

import BuckProxy.Util (dbg)

import BuckWorkerProto (ExecuteCommand, ExecuteResponse)
import Common.Grpc (commandEnv, forwardRequest, runGrpcServer, streamingNotImplemented, waitPoll)
import Control.Applicative ((<|>))

import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Exception (throwIO, try)

import Control.Monad (void, when)
import Data.Map.Strict (Map, (!?))
import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.GRPC.Client (Server (..), withConnection)
import Network.GRPC.Common (def)
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (
  Methods (..),
  mkClientStreaming,
  mkNonStreaming,
  )
import Proto.Worker (Worker (..))

import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)
import Types.Args (TargetId)
import Types.BuckArgs (BuckArgs (workerTargetId), parseBuckArgs)
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Orchestration (
  PrimarySocketName (..),
  PrimarySocketPath (..),
  ServerSocketPath (..),
  SocketDirectory (..),
  extractTraceIdAndWorkerSpecId,
  primarySocketIn,
  projectSocketDirectory,
  )

-- | Path to the worker executable proxied by this app.
--- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

-- | Executable and arguments used to spawn the GHC server process.
data GhcWorkerCommand =
  GhcWorkerCommand {
    exe :: WorkerExe,
    args :: [String]
  }
  deriving stock (Eq, Show)

data WorkerResource =
  WorkerResource {
    primarySocket :: PrimarySocketPath,
    processHandle :: ProcessHandle
  }



proxyHandler ::
  MVar (Map TargetId WorkerResource) ->
  GhcWorkerCommand ->
  -- | Worker socket path determined by proxy socket path
  PrimarySocketName ->
  -- | CLI override for the socket path
  Maybe PrimarySocketName ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
proxyHandler workerMap command socketDefault socketOverride req = do
  let cmdEnv = commandEnv req.env
      argv = Text.unpack . decodeUtf8Lenient <$> req.argv
      -- Get the build ID for the primary socket path from the command environment, and fall back to the value extracted
      -- from the gRPC socket path if the key is absent from the env.
      -- If an override was specified on the command line with @--socket-name@, it has precedence over both.
      socketId = fromMaybe socketDefault (socketOverride <|> coerce (cmdEnv.values !? "BUCK_BUILD_ID"))
  buckArgs <- either (throwIO . userError) pure (parseBuckArgs cmdEnv (RequestArgs argv))
  case buckArgs.workerTargetId of
    Nothing -> throwIO (userError "No --worker-target-id passed")
    Just targetId -> do
      resource <-
        modifyMVar workerMap \wmap -> do
          case Map.lookup targetId wmap of
            Nothing -> do
              let workerSocketDir = projectSocketDirectory socketId targetId
              void $ try @IOError (createDirectoryIfMissing True workerSocketDir.path)
              resource <- spawnGhcWorker command workerSocketDir
              dbg $ "No primary socket for " ++ show targetId ++ ", so created it on " ++ resource.primarySocket.path
              pure (Map.insert targetId resource wmap, resource)
            Just resource -> do
              dbg $ "Primary socket for " ++ show targetId ++ ": " ++ resource.primarySocket.path
              pure (wmap, resource)
      withConnection def (ServerUnix resource.primarySocket.path) \connection ->
        forwardRequest connection req



-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to ghc-worker
proxyServer ::
  -- | mutable worker map (we spawn a new ghc-worker as a new target id arrives)
  MVar (Map TargetId WorkerResource) ->
  GhcWorkerCommand ->
  ServerSocketPath ->
  Maybe PrimarySocketName ->
  IO ()
proxyServer workerMap command socket workerSocketOverride = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down buck-proxy on " ++ socket.path)
    Left (err :: IOError) -> do
      dbg ("buck-proxy on" ++ socket.path ++ " crashed" ++ show err)
      exitFailure
  where
    (traceId, workerSpecId) = extractTraceIdAndWorkerSpecId socket.path
    workerSocketDefault = PrimarySocketName (traceId ++ "-" ++ workerSpecId)
    methods :: Methods IO (ProtobufMethodsOf Worker)
    methods =
      Method (mkClientStreaming streamingNotImplemented) $
      Method (mkNonStreaming (proxyHandler workerMap command workerSocketDefault workerSocketOverride)) $
      NoMoreMethods
    launch = do
      dbg ("Starting buck-proxy on " ++ socket.path)
      runGrpcServer socket.path methods



-- | Wait for a GHC server process to respond and check its exit code.
waitForGhcWorker :: ProcessHandle -> PrimarySocketPath -> IO ()
waitForGhcWorker ph socket = do
  dbg "Waiting for server"
  waitPoll socket.path
  dbg "Server is up"
  exitCode <- getProcessExitCode ph
  when (isJust exitCode) do
    dbg "Spawned process for the GHC server exited after starting up."

-- | Spawn a child process executing the worker executable, for the purpose of running a GHC server to which some or all
-- worker processes then forward their requests.
-- Afterwards, wait for the server to be responsive.
spawnGhcWorker ::
  GhcWorkerCommand ->
  SocketDirectory ->
  IO WorkerResource
spawnGhcWorker GhcWorkerCommand {exe, args} socketDir = do
  dbg ("Forking GHC server at " ++ primary.path)
  proc <- spawnProcess exe.path (args ++ ["--serve", primary.path])
  waitForGhcWorker proc primary
  pure WorkerResource {primarySocket = primary, processHandle = proc}
  where
    primary = primarySocketIn socketDir
