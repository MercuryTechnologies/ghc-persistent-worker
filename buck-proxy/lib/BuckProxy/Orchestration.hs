module BuckProxy.Orchestration (
  GhcWorkerCommand (..),
  WorkerExe (..),
  WorkerResource (..),
  proxyServer,
  spawnGhcWorker,
) where

import BuckProxy.Util (dbg)

import BuckWorkerProto (ExecuteCommand, ExecuteResponse)
import Common.Grpc (commandEnv, executeMethods, forwardRequest, runGrpcServer, waitPoll)
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
import Network.GRPC.Server.StreamType (Methods)
import Proto.Worker (Worker)

import System.Directory.OsPath (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.OsPath.Extra (fromOsPath, toOsPath)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)
import Types.Args (TargetId)
import Types.BuckArgs (BuckArgs (workerTargetId), parseBuckArgs)
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Orchestration (
  PrimarySocketName (..),
  PrimarySocketPath (..),
  ProxyInstance (..),
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
  -- | Proxy instance id
  Maybe ProxyInstance ->
  -- | CLI override for the socket path
  Maybe PrimarySocketName ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
proxyHandler workerMap command socketDefault mProxyInstance socketOverride req = do
  let cmdEnv = commandEnv req.env
      argv = Text.unpack . decodeUtf8Lenient <$> req.argv
      -- Get the build ID for the primary socket path from the command environment, and fall back to the value extracted
      -- from the gRPC socket path if the key is absent from the env.
      -- If an override was specified on the command line with @--socket-name@, it has precedence over both.
      mkSocketPathFromBuildID = do
        buildId <- cmdEnv.values !? "BUCK_BUILD_ID"
        let suffix = maybe "" (\s -> "_" ++ s.instanceId) mProxyInstance
        pure $ coerce (toOsPath (buildId ++ suffix))
      socketPath = fromMaybe socketDefault (socketOverride <|> mkSocketPathFromBuildID)

  buckArgs <- either (throwIO . userError) pure (parseBuckArgs cmdEnv (RequestArgs argv))
  case buckArgs.workerTargetId of
    Nothing -> throwIO (userError "No --worker-target-id passed")
    Just targetId -> do
      resource <-
        modifyMVar workerMap \wmap -> do
          case Map.lookup targetId wmap of
            Nothing -> do
              let workerSocketDir = projectSocketDirectory socketPath targetId
              void $ try @IOError (createDirectoryIfMissing True workerSocketDir.path)
              resource <- spawnGhcWorker command workerSocketDir
              dbg $ "No primary socket for " ++ show targetId ++ ", so created it on " ++ fromOsPath resource.primarySocket.path
              pure (Map.insert targetId resource wmap, resource)
            Just resource -> do
              dbg $ "Primary socket for " ++ show targetId ++ ": " ++ fromOsPath resource.primarySocket.path
              pure (wmap, resource)
      withConnection def (ServerUnix $ fromOsPath resource.primarySocket.path) \connection ->
        forwardRequest connection req



-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to ghc-worker
proxyServer ::
  -- | mutable worker map (we spawn a new ghc-worker as a new target id arrives)
  MVar (Map TargetId WorkerResource) ->
  GhcWorkerCommand ->
  ServerSocketPath ->
  Maybe ProxyInstance ->
  Maybe PrimarySocketName ->
  IO ()
proxyServer workerMap command buckSocket mProxyInstance workerSocketOverride = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down buck-proxy on " ++ fromOsPath buckSocket.path)
    Left (err :: IOError) -> do
      dbg ("buck-proxy on" ++ fromOsPath buckSocket.path ++ " crashed" ++ show err)
      exitFailure
  where
    (traceId, workerSpecId) = extractTraceIdAndWorkerSpecId buckSocket.path
    workerSocketDefault = PrimarySocketName (toOsPath $ traceId ++ "-" ++ workerSpecId)
    methods :: Methods IO (ProtobufMethodsOf Worker)
    methods =
      executeMethods (proxyHandler workerMap command workerSocketDefault mProxyInstance workerSocketOverride)
    launch = do
      dbg ("Starting buck-proxy on " ++ fromOsPath buckSocket.path)
      runGrpcServer buckSocket.path methods



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
  dbg ("Forking GHC server at " ++ fromOsPath primary.path)
  proc <- spawnProcess exe.path (args ++ ["--serve", fromOsPath primary.path])
  waitForGhcWorker proc primary
  pure WorkerResource {primarySocket = primary, processHandle = proc}
  where
    primary = primarySocketIn socketDir
