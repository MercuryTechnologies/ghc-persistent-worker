{-# LANGUAGE OverloadedStrings #-}

module BuckProxy.Orchestration where

import BuckProxy.Util (dbg)
import qualified BuckWorker as Worker
import BuckWorker (ExecuteCommand, ExecuteResponse)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.DeepSeq (force)
import Control.Exception (bracket_, throwIO, try)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import GHC.IO.Handle.Lock (LockMode (..), hLock, hUnlock)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (NextElem (..), Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (..), fromMethods, mkClientStreaming, mkNonStreaming)
import Proto.Worker (ExecuteEvent, Worker (..))
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (IOMode (..), hGetLine, hPutStr, withFile)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)
import Types.GhcHandler (WorkerMode (..))
import Types.Orchestration (
  Orchestration,
  PrimarySocketDiscoveryPath (..),
  PrimarySocketPath (..),
  ServerSocketPath (..),
  SocketDirectory (..),
  primarySocketDiscoveryIn,
  primarySocketIn,
  projectSocketDirectory,
  )

-- | Path to the worker executable, i.e. this program.
--- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

-- | The worker protocol is intended to support streaming events, but we're not using that yet.
streamingNotImplemented :: IO (NextElem (Proto ExecuteEvent)) -> IO (Proto ExecuteResponse)
streamingNotImplemented _ =
  pure $
    defMessage
      & Fields.exitCode
      .~ 1
      & Fields.stderr
      .~ "Streaming not implemented"

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

-- | Bracket a computation with a gRPC worker client resource, connecting to the server listening on the provided
-- socket.
withProxy ::
  PrimarySocketPath ->
  (Methods IO (ProtobufMethodsOf Worker) -> IO a) ->
  IO a
withProxy socket use = do
  withConnection def server $ \ connection -> do
    use $
      Method (mkClientStreaming streamingNotImplemented) $
      Method (mkNonStreaming (forwardRequest connection)) $
      NoMoreMethods
  where
    server = ServerUnix socket.path

grpcServerConfig :: FilePath -> ServerConfig
grpcServerConfig socketPath =
  ServerConfig
    { serverInsecure = Just (InsecureUnix socketPath)
    , serverSecure = Nothing
    }

-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to another gRPC server (here
-- our GHC primary).
proxyServer :: PrimarySocketPath -> ServerSocketPath -> IO ()
proxyServer primary socket = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down proxy on " ++ socket.path ++ " regularly")
    Left (err :: IOError) -> do
      dbg ("Proxy on " ++ socket.path ++ " crashed: " ++ show err)
      exitFailure
  where
    launch =
      withProxy primary \ methods -> do
        dbg ("Starting proxy for " ++ primary.path ++ " on " ++ socket.path)
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
waitForCentralGhc :: ProcessHandle -> PrimarySocketPath -> IO ()
waitForCentralGhc ph socket = do
  dbg "Waiting for server"
  waitPoll socket
  dbg "Server is up"
  exitCode <- getProcessExitCode ph
  when (isJust exitCode) do
    dbg "Spawned process for the GHC server exited after starting up."

-- | Spawn a child process executing the worker executable (which usually is the same as this process), for the purpose
-- of running a GHC server to which all worker processes then forward their requests.
-- Afterwards, wait for the server to be responsive.
forkCentralGhc :: WorkerExe -> WorkerMode -> SocketDirectory -> IO PrimarySocketPath
forkCentralGhc exe mode socketDir = do
  dbg ("Forking GHC server at " ++ primary.path)
  let workerModeFlag = case mode of
        WorkerOneshotMode -> []
        WorkerMakeMode -> ["--make"]
  proc <- spawnProcess exe.path (workerModeFlag ++ ["--serve", primary.path])
  waitForCentralGhc proc primary
  pure primary
  where
    primary = primarySocketIn socketDir

-- | Run a server if this process is the primary worker, otherwise return the primary's socket path.
-- Since multiple workers are started in separate processes, we negotiate using file system locks.
-- There are two major scenarios:
--
-- - When the build is started, multiple workers are spawned concurrently, and no primary exists.
--   We create a lock file in the provided socket directory.
--   The first worker that wins the lock in `withFileBlocking` gets to be primary, and writes its socket path to
--   `$socket_dir/primary`.
--   All other workers then own the lock in sequence and proceed with the second scenario.
--
-- - When the build is running and a primary exists, either because the worker lost the inital lock race or was started
--   later in the build due to dependencies and/or parallelism limits, the contents of the `primary` file are read to
--   obtain the primary's socket path.
--   A gRPC server is started that resends all requests to that socket.
runOrProxyCentralGhc ::
  SocketDirectory ->
  (PrimarySocketDiscoveryPath -> IO (PrimarySocketPath, a)) ->
  IO (Either PrimarySocketPath (PrimarySocketPath, a))
runOrProxyCentralGhc socketDir runServer = do
  void $ try @IOError (createDirectoryIfMissing True socketDir.path)
  withFile primaryFile.path ReadWriteMode \ handle -> do
    bracket_ (hLock handle ExclusiveLock) (hUnlock handle) do
      try @IOError (hGetLine handle) >>= \case
        -- If the file didn't exist, `hGetLine` will still return the empty string in some GHC versions.
        -- File IO is buffered/lazy, so we have to force the string to avoid read after close.
        Right !primary | not (null (force primary)) -> do
          pure (Left (PrimarySocketPath primary))
        _ -> do
          (primary, resource) <- runServer primaryFile
          hPutStr handle primary.path
          pure (Right (primary, resource))
  where
    primaryFile = primarySocketDiscoveryIn socketDir

-- | Start a proxy gRPC server that forwards requests to the central GHC server.
-- If that server isn't running, spawn a process and wait for it to boot up.
spawnOrProxyCentralGhc :: WorkerExe -> Orchestration -> WorkerMode -> ServerSocketPath -> MVar (IO ()) -> IO ()
spawnOrProxyCentralGhc exe omode wmode socket refHandler = do
  let socketDir = projectSocketDirectory omode socket
  eprimary <- runOrProxyCentralGhc socketDir \ _ -> do
    primary <- forkCentralGhc exe wmode socketDir
    pure (primary, ())
  let primary = either id fst eprimary
  putMVar refHandler $ do
    dbg (show primary)
    withConnection def (ServerUnix primary.path) \ connection ->
      withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
        let req = defMessage & Fields.argv .~ ["--worker-mode", "close"]
        sendFinalInput call req
        resp <- recvNextOutput call
        dbg (show resp)

  proxyServer primary socket
