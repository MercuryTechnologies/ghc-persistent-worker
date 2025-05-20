{-# LANGUAGE NoFieldSelectors #-}

module Orchestration where

import qualified BuckWorker as Worker
import BuckWorker (ExecuteCommand, ExecuteResponse)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.DeepSeq (force)
import Control.Exception (bracket_, finally, onException, throwIO, try)
import Control.Monad (void, when)
import Data.Hashable (hash)
import Data.List (dropWhileEnd)
import Data.Maybe (isJust)
import Data.Traversable (for)
import GHC.Debug.Stub (withGhcDebug)
import GHC.IO.Handle.Lock (LockMode (..), hLock, hUnlock)
import Grpc (streamingNotImplemented)
import Internal.Log (dbg)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (..), fromMethods, mkClientStreaming, mkNonStreaming)
import Proto.Instrument (Instrument (..))
import Proto.Worker (Worker (..))
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hGetLine, hPutStr, withFile)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)

-- | Determine how GHC servers should be started in relation to Buck worker processes.
data Orchestration =
  -- | Each worker process starts its own GHC server.
  Multi
  |
  -- | One worker process starts a GHC server, the others start a proxy server that forwards requests to the central
  -- GHC.
  Single
  |
  -- | One worker process spawns a new child process that runs the GHC server, and all workers then proxy this GHC.
  -- GHC.
  Spawn
  deriving stock (Eq, Show)

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
newtype ServerSocketPath =
  ServerSocketPath { path :: FilePath }
  deriving stock (Eq, Show)

-- | This environment variable is usually set by Buck before starting the worker process.
envServerSocket :: IO ServerSocketPath
envServerSocket = ServerSocketPath <$> getEnv "WORKER_SOCKET"

-- | The base dir for sockets, usually a dir in @/tmp@ created by Buck or ourselves.
newtype SocketDirectory =
  SocketDirectory { path :: FilePath }
  deriving stock (Eq, Show)

-- | Derive the socket base dir from the socket path provided by Buck.
spawnedSocketDirectory :: ServerSocketPath -> SocketDirectory
spawnedSocketDirectory (ServerSocketPath path) =
  SocketDirectory (takeDirectory path)

-- | Use the current directory's hash to create a socket directory independent of Buck.
-- This is a hack that should eventually be replaced by a proper system.
projectSocketDirectory :: IO SocketDirectory
projectSocketDirectory = do
  cwd <- getCurrentDirectory
  pure (SocketDirectory ("/tmp/ghc-persistent-worker/" ++ show (hash cwd)))

-- | The file system path of the socket on which the primary worker running the GHC server is listening.
newtype PrimarySocketPath =
  PrimarySocketPath { path :: FilePath }
  deriving stock (Eq, Show)

-- | For the case where the primary server is spawned, rather than reusing the socket on which communication with Buck
-- is happening.
primarySocketIn :: SocketDirectory -> PrimarySocketPath
primarySocketIn dir = PrimarySocketPath (dir.path </> "server")

-- | The file system path of the socket on which the primary worker outputs instrumentation information.
newtype InstrumentSocketPath =
  InstrumentSocketPath { path :: FilePath }
  deriving stock (Eq, Show)

instrumentSocketIn :: SocketDirectory -> InstrumentSocketPath
instrumentSocketIn dir = InstrumentSocketPath (dir.path </> "instrument")

-- | The file system path in which the primary worker running the GHC server stores its socket path for clients to
-- discover.
newtype PrimarySocketDiscoveryPath =
  PrimarySocketDiscoveryPath { path :: FilePath }
  deriving stock (Eq, Show)

primarySocketDiscoveryIn :: SocketDirectory -> PrimarySocketDiscoveryPath
primarySocketDiscoveryIn dir = PrimarySocketDiscoveryPath (dir.path </> "primary")

-- | Path to the worker executable, i.e. this program.
-- Used to spawn the GHC server process.
newtype WorkerExe =
  WorkerExe { path :: FilePath }
  deriving stock (Eq, Show)

-- | The implementation of an app consisting of two gRPC servers, implementing the protocols 'Worker' and 'Instrument'.
-- The 'Instrument' component is intended to be optional.
data CreateMethods where
  CreateMethods :: {
    createInstrumentation :: IO (instrumentSocket, Methods IO (ProtobufMethodsOf Instrument)),
    createGhc :: Maybe instrumentSocket -> IO (Methods IO (ProtobufMethodsOf Worker))
  } -> CreateMethods

newtype FeatureInstrument =
  FeatureInstrument { flag :: Bool }
  deriving stock (Eq, Show)

-- | Start a gRPC server that dispatches requests to GHC handlers.
runLocalGhc ::
  CreateMethods ->
  ServerSocketPath ->
  Maybe InstrumentSocketPath ->
  IO ()
runLocalGhc CreateMethods {..} socket minstr = do
  dbg ("Starting ghc server on " ++ socket.path)
  instrResource <- for minstr \instrumentSocket -> do
    dbg ("Instrumentation info available on " ++ instrumentSocket.path)
    (resource, methods) <- createInstrumentation
    _instrThread <- async $ runServerWithHandlers def (grpcServerConfig instrumentSocket.path) (fromMethods methods)
    pure resource
  methods <- createGhc instrResource
  withGhcDebug do
    runServerWithHandlers def (grpcServerConfig socket.path) (fromMethods methods)

-- | Start a gRPC server that runs GHC for client proxies, deleting the discovery file on shutdown.
runCentralGhc ::
  CreateMethods ->
  PrimarySocketDiscoveryPath ->
  ServerSocketPath ->
  Maybe InstrumentSocketPath ->
  IO ()
runCentralGhc mode discovery socket instrumentSocket =
  finally (runLocalGhc mode socket instrumentSocket) do
    dbg ("Shutting down ghc server on " ++ socket.path)
    removeFile discovery.path

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
waitForCentralGhc proc socket = do
  dbg "Waiting for server"
  waitPoll socket
  dbg "Server is up"
  exitCode <- getProcessExitCode proc
  when (isJust exitCode) do
    dbg "Spawned process for the GHC server exited after starting up."

-- | Spawn a child process executing the worker executable (which usually is the same as this process), for the purpose
-- of running a GHC server to which all worker processes then forward their requests.
-- Afterwards, wait for the server to be responsive.
forkCentralGhc :: WorkerExe -> SocketDirectory -> IO PrimarySocketPath
forkCentralGhc exe socketDir = do
  dbg ("Forking GHC server at " ++ primary.path)
  proc <- spawnProcess exe.path ["--make", "--serve", primary.path, "+RTS", "-hT", "-i1", "-l", "-ol/home/tek/profile.eventlog", "-RTS"]
  -- proc <- spawnProcess exe.path ["--make", "--serve", primary.path]
  waitForCentralGhc proc primary
  pure primary
  where
    primary = primarySocketIn socketDir

-- | Run a GHC server synchronously.
runCentralGhcSpawned :: CreateMethods -> FeatureInstrument -> ServerSocketPath -> IO ()
runCentralGhcSpawned methods instrument socket =
  runCentralGhc methods primaryFile socket instrumentSocket
  where
    instrumentSocket =
      if instrument.flag
      then Just (instrumentSocketIn dir)
      else Nothing

    primaryFile = primarySocketDiscoveryIn dir

    dir = spawnedSocketDirectory socket

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

-- | Start a gRPC server that either runs GHC (primary server) or a proxy that forwards requests to the primary.
serveOrProxyCentralGhc :: CreateMethods -> ServerSocketPath -> IO ()
serveOrProxyCentralGhc mode socket = do
  runOrProxyCentralGhc socketDir run >>= \case
    Right (_, thread) -> onException (wait thread) (cancel thread)
    Left primary -> proxyServer primary socket
  where
    run primaryFile = do
      let primary = PrimarySocketPath socket.path
      thread <- async (runCentralGhc mode primaryFile socket instrumentSocket)
      waitPoll primary
      pure (primary, thread)

    instrumentSocket = Just (instrumentSocketIn socketDir)

    socketDir = SocketDirectory (init (dropWhileEnd ('-' /=) (takeDirectory socket.path)))

-- | Start a proxy gRPC server that forwards requests to the central GHC server.
-- If that server isn't running, spawn a process and wait for it to boot up.
spawnOrProxyCentralGhc :: WorkerExe -> ServerSocketPath -> IO ()
spawnOrProxyCentralGhc exe socket = do
  socketDir <- projectSocketDirectory
  primary <- runOrProxyCentralGhc socketDir \ _ -> do
    primary <- forkCentralGhc exe socketDir
    pure (primary, ())
  proxyServer (either id fst primary) socket
