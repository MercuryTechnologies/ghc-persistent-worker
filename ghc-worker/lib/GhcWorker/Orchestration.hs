{-# LANGUAGE RecursiveDo #-}
module GhcWorker.Orchestration where

import Common.Grpc (forwardRequest, runGrpcServer, streamingNotImplemented, waitPoll)
import Control.Concurrent.Async (async, cancel, wait)
import Control.DeepSeq (force)
import Control.Exception (bracket_, finally, onException, try)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Data.Traversable (for)
import GHC.IO.Handle.Lock (LockMode (..), hLock, hUnlock)
import Internal.Log (dbg)
import Network.GRPC.Client (Server (..), withConnection)
import Network.GRPC.Common (def)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..), mkClientStreaming, mkNonStreaming)
import Proto.Instrument (Instrument (..))
import Proto.Worker (Worker (..))
import System.Directory.OsPath (createDirectoryIfMissing, removeFile)
import System.Exit (exitFailure)
import System.File.OsPath (withFile)
import System.IO (IOMode (..), hGetLine, hPutStr)
import System.OsPath.Extra (encodeUtf, fromOsPath, takeDirectory)
import System.OsString (dropWhileEnd, unsafeFromChar)
import qualified System.OsString as OsString
import System.Process (ProcessHandle, getProcessExitCode)
import Types.Grpc (CommandEnv, RequestArgs (..))
import Types.Orchestration (
  InstrumentSocketPath (..),
  PrimarySocketDiscoveryPath (..),
  PrimarySocketPath (..),
  ServerSocketPath (..),
  SocketDirectory (..),
  instrumentSocketIn,
  primarySocketDiscoveryIn,
  spawnedSocketDirectory,
  )

-- | The implementation of an app consisting of two gRPC servers, implementing the protocols 'Worker' and 'Instrument'.
-- The 'Instrument' component is intended to be optional.
data CreateMethods where
  CreateMethods :: {
    createInstrumentation :: (CommandEnv -> RequestArgs -> IO ()) -> IO (instrumentSocket, Methods IO (ProtobufMethodsOf Instrument)),
    createGhc :: Maybe instrumentSocket -> IO (CommandEnv -> RequestArgs -> IO (), Methods IO (ProtobufMethodsOf Worker))
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
runLocalGhc CreateMethods {..} socket minstr = mdo
  dbg ("Starting ghc server on " ++ fromOsPath socket.path)
  instrResource <- for minstr \instrumentSocket -> do
    dbg ("Instrumentation info available on " ++ fromOsPath instrumentSocket.path)
    (resource, instrMethods) <- createInstrumentation (\ ce (RequestArgs args) -> recompile ce (RequestArgs (args ++ ["-fforce-recomp"])))
    _instrThread <- async $ runGrpcServer instrumentSocket.path instrMethods
    pure resource
  (recompile, methods) <- createGhc instrResource
  runGrpcServer socket.path methods

-- | Start a gRPC server that runs GHC for client proxies, deleting the discovery file on shutdown.
runCentralGhc ::
  CreateMethods ->
  PrimarySocketDiscoveryPath ->
  ServerSocketPath ->
  Maybe InstrumentSocketPath ->
  IO ()
runCentralGhc mode discovery socket instrumentSocket =
  finally (runLocalGhc mode socket instrumentSocket) do
    dbg ("Shutting down ghc server on " ++ fromOsPath socket.path)
    removeFile discovery.path



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
    server = ServerUnix (fromOsPath socket.path)



-- | Start a worker gRPC server that forwards requests received from a client (here Buck) to another gRPC server (here
-- our GHC primary).
proxyServer :: PrimarySocketPath -> ServerSocketPath -> IO ()
proxyServer primary socket = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down proxy on " ++ fromOsPath socket.path ++ " regularly")
    Left (err :: IOError) -> do
      dbg ("Proxy on " ++ fromOsPath socket.path ++ " crashed: " ++ show err)
      exitFailure
  where
    launch =
      withProxy primary \ methods -> do
        dbg ("Starting proxy for " ++ fromOsPath primary.path ++ " on " ++ fromOsPath socket.path)
        runGrpcServer socket.path methods



-- | Wait for a GHC server process to respond and check its exit code.
waitForCentralGhc :: ProcessHandle -> PrimarySocketPath -> IO ()
waitForCentralGhc proc socket = do
  dbg "Waiting for server"
  waitPoll socket.path
  dbg "Server is up"
  exitCode <- getProcessExitCode proc
  when (isJust exitCode) do
    dbg "Spawned process for the GHC server exited after starting up."

-- | Run a GHC server synchronously.
runCentralGhcSpawned :: CreateMethods -> FeatureInstrument -> ServerSocketPath -> IO ()
runCentralGhcSpawned methods featureInstrument socket =
  runCentralGhc methods primaryFile socket instrumentSocket
  where
    instrumentSocket =
      if featureInstrument.flag
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
          primaryPath <- encodeUtf primary
          pure (Left (PrimarySocketPath primaryPath))
        _ -> do
          (primary, resource) <- runServer primaryFile
          hPutStr handle (fromOsPath primary.path)
          pure (Right (primary, resource))
  where
    primaryFile = primarySocketDiscoveryIn socketDir

-- | Start a gRPC server that either runs GHC (primary server) or a proxy that forwards requests to the primary.
serveOrProxyCentralGhc :: CreateMethods -> ServerSocketPath -> IO ()
serveOrProxyCentralGhc methods socket = do
  runOrProxyCentralGhc socketDir run >>= \case
    Right (_, thread) -> onException (wait thread) (cancel thread)
    Left primary -> proxyServer primary socket
  where
    run primaryFile = do
      let primary = PrimarySocketPath socket.path
      thread <- async (runCentralGhc methods primaryFile socket instrumentSocket)
      waitPoll primary.path
      pure (primary, thread)

    instrumentSocket = Just (instrumentSocketIn socketDir)

    socketDir = SocketDirectory (OsString.init $ dropWhileEnd (unsafeFromChar '-' /=) $ takeDirectory socket.path)
