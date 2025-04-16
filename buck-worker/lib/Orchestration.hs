{-# LANGUAGE NoFieldSelectors #-}

module Orchestration where

import BuckWorker (ExecuteCommand, ExecuteResponse)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Exception (bracket_, finally, try)
import Control.Monad (void, when)
import Data.Hashable (hash)
import Data.Maybe (isJust)
import Data.Traversable (for)
import GHC.IO.Handle (LockMode (..), hLock)
import GHC.IO.Handle.Lock (hUnlock)
import Grpc (streamingNotImplemented)
import Internal.Log (dbg)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, (%~), (&))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (..), fromMethods, mkClientStreaming, mkNonStreaming)
import Proto.Instrument (Instrument (..))
import Proto.Worker (Worker (..))
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectory, getCurrentDirectory, removeFile)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hGetLine, hPutStr, withFile)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess)

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
newtype ServerSocketPath =
  ServerSocketPath { path :: FilePath }
  deriving stock (Eq, Show)

-- | This environment variable is usually set by Buck before starting the worker process.
envServerSocket :: IO ServerSocketPath
envServerSocket = ServerSocketPath <$> getEnv "WORKER_SOCKET"

-- | The file system path of the socket on which the primary worker running the GHC server is listening.
newtype PrimarySocketPath =
  PrimarySocketPath { path :: FilePath }
  deriving stock (Eq, Show)

-- | The file system path of the socket on which the primary worker outputs instrumentation information.
newtype InstrumentSocketPath =
  InstrumentSocketPath { path :: FilePath }
  deriving stock (Eq, Show)

instrumentSocketIn :: FilePath -> InstrumentSocketPath
instrumentSocketIn dir = InstrumentSocketPath (dir </> "instrument")

-- | The file system path in which the primary worker running the GHC server stores its socket path for clients to
-- discover.
newtype PrimarySocketDiscoveryPath =
  PrimarySocketDiscoveryPath { path :: FilePath }
  deriving stock (Eq, Show)

primarySocketDiscoveryIn :: FilePath -> PrimarySocketDiscoveryPath
primarySocketDiscoveryIn dir = PrimarySocketDiscoveryPath (dir </> "primary")

-- | The implementation of an app consisting of two gRPC servers, implementing the protocols 'Worker' and 'Instrument'.
-- The 'Instrument' component is intended to be optional.
data CreateMethods where
  CreateMethods :: {
    createInstrumentation :: IO (instr, Methods IO (ProtobufMethodsOf Instrument)),
    createGhc :: Maybe instr -> IO (Methods IO (ProtobufMethodsOf Worker))
  } -> CreateMethods

-- | Start a gRPC server that dispatches requests to GHC handlers.
runLocalGhc ::
  CreateMethods ->
  ServerSocketPath ->
  Maybe InstrumentSocketPath ->
  IO ()
runLocalGhc CreateMethods {..} socket minstr = do
  dbg ("Starting ghc server on " ++ socket.path)
  instrResource <- for minstr \instr -> do
    dbg ("Instrumentation info available on " ++ instr.path)
    (resource, methods) <- createInstrumentation
    _instrThread <- async $ runServerWithHandlers def (grpcServerConfig instr.path) (fromMethods methods)
    pure resource
  methods <- createGhc instrResource
  runServerWithHandlers def (grpcServerConfig socket.path) (fromMethods methods)

-- | Start a gRPC server that runs GHC for client proxies, deleting the discovery file on shutdown.
runCentralGhc ::
  CreateMethods ->
  PrimarySocketDiscoveryPath ->
  ServerSocketPath ->
  Maybe InstrumentSocketPath ->
  IO ()
runCentralGhc mode discovery socket instr =
  finally (runLocalGhc mode socket instr) do
    dbg ("Shutting down ghc server on " ++ socket.path)
    when False do
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

waitForCentralGhc :: ProcessHandle -> PrimarySocketPath -> IO ()
waitForCentralGhc proc _ = do
  dbg "Waiting for server"
  threadDelay 2_000_000
  exitCode <- getProcessExitCode proc
  when (isJust exitCode) do
    dbg "Central GHC fork failed."

forkCentralGhc :: FilePath -> FilePath -> IO PrimarySocketPath
forkCentralGhc exe dir = do
  dbg ("Forking GHC server at " ++ socket)
  proc <- spawnProcess exe ["--make", "--serve", socket]
  let primary = PrimarySocketPath socket
  waitForCentralGhc proc primary
  pure primary
  where
    socket = dir </> "server"

runCentralGhcForked :: CreateMethods -> FilePath -> IO ()
runCentralGhcForked methods socket =
  runCentralGhc methods primaryFile (ServerSocketPath socket) instr
  where
    instr = Just (instrumentSocketIn dir)

    primaryFile = primarySocketDiscoveryIn dir

    dir = takeDirectory socket

-- | Start a gRPC server that either runs GHC (primary server) or a proxy that forwards requests to the primary.
-- Since multiple workers are started in separate processes, we negotiate using file system locks.
-- There are two major scenarios:
--
-- - When the build is started, multiple workers are spawned concurrently, and no primary exists.
--   We use the common prefix of the worker sockets (something like `/tmp/buck2_worker/<hash>`) to create a lock file.
--   The first worker that wins the lock in `withFileBlocking` gets to be primary, and writes its socket path to
--   `/tmp/buck2_worker/<hash>/primary`.
--   All other workers then own the lock in sequence and proceed with the second scenario.
--
-- - When the build is running and a primary exists, either because the worker lost the inital lock race or was started
--   later in the build due to dependencies and/or parallelism limits, the contents of the `primary` file are read to
--   obtain the primary's socket path.
--   A gRPC server is started that resends all requests to that socket.
runOrProxyCentralGhc :: FilePath -> ServerSocketPath -> IO ()
runOrProxyCentralGhc exe socket = do
  cwd <- getCurrentDirectory
  let
    projectId = hash cwd
    dir = "/tmp/buck2_worker/" ++ show projectId
    primaryFile = primarySocketDiscoveryIn dir
  void $ try @IOError (createDirectory dir)
  primary <- withFile primaryFile.path ReadWriteMode \ handle -> do
    bracket_ (hLock handle ExclusiveLock) (hUnlock handle) do
      let
        fork = do
          primary <- forkCentralGhc exe dir
          hPutStr handle primary.path
          pure primary
      try @IOError (hGetLine handle) >>= \case
        -- If the file didn't exist, `hGetLine` will still return the empty string.
        -- File IO is buffered/lazy, so we have to force the pattern to avoid read after close (though this is already
        -- achieved by calling `null`).
        Right !primary | not (null primary) ->
          pure (PrimarySocketPath primary)
        Right _ -> do
          dbg "empty primary file"
          fork
        Left err -> do
          dbg ("primary file error: " ++ show err)
          fork
  proxyServer primary socket
