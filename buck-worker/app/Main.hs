{-# LANGUAGE DataKinds, GADTs, MultiWayIf #-}

module Main where

import BuckArgs (BuckArgs (..), CompileResult (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (
  ExecuteCommand (..),
  ExecuteCommand_EnvironmentEntry (..),
  ExecuteEvent (..),
  ExecuteResponse (..),
  Worker (..),
  workerClient,
  workerServer,
  )
import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Exception (
  Exception (displayException),
  SomeException (SomeException),
  bracket,
  finally,
  onException,
  throwIO,
  try,
  )
import Control.Monad (foldM, void, when)
import Data.Functor ((<&>))
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import GHC (Ghc, getSession)
import GHC.IO.Handle.FD (withFileBlocking)
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), ModuleArtifacts (..), Target, emptyCache)
import Internal.CompileHpt (compileHpt)
import Internal.Log (dbg, logFlush, newLog)
import Internal.Session (Env (..), withGhc)
import Network.GRPC.HighLevel.Generated (
  ClientConfig (..),
  ClientError,
  ClientRequest (ClientNormalRequest),
  ClientResult (..),
  GRPCMethodType (..),
  ServerRequest (..),
  ServerResponse (..),
  ServiceOptions (..),
  StatusCode (..),
  defaultServiceOptions,
  withGRPCClient,
  )
import Prelude hiding (log)
import System.Directory (createDirectory, removeFile)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), IOMode (..), hGetLine, hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

data WorkerStatus =
  WorkerStatus {
    active :: Int
  }
  deriving stock (Eq, Show)

commandEnv :: Vector.Vector ExecuteCommand_EnvironmentEntry -> Map String String
commandEnv =
  Map.fromList .
  fmap (\ (ExecuteCommand_EnvironmentEntry key value) -> (fromBs key, fromBs value)) .
  Vector.toList
  where
    fromBs = Text.unpack . decodeUtf8Lenient

compileAndReadAbiHash :: BuckArgs -> Target -> Ghc (Maybe CompileResult)
compileAndReadAbiHash args target = do
  compileHpt target >>= traverse \ artifacts -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash {path, hash = showAbiHash hsc_env artifacts.iface}
    pure CompileResult {artifacts, abiHash}

startJob ::
  MVar WorkerStatus ->
  IO ()
startJob var =
  modifyMVar_ var \ WorkerStatus {active} -> do
    let new = active + 1
    dbg ("Starting job, now " ++ show new ++ " active")
    pure WorkerStatus {active = new}

finishJob ::
  MVar WorkerStatus ->
  a ->
  IO ()
finishJob var _ = do
  modifyMVar_ var \ WorkerStatus {active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure WorkerStatus {active = new}

debugRequestArgs :: Bool
debugRequestArgs = True

executeHandler ::
  MVar WorkerStatus ->
  MVar Cache ->
  ServerRequest 'Normal ExecuteCommand ExecuteResponse ->
  IO (ServerResponse 'Normal ExecuteResponse)
executeHandler status cache (ServerNormalRequest _ ExecuteCommand {executeCommandArgv, executeCommandEnv}) = do
  when debugRequestArgs do
    hPutStrLn stderr (unlines argv)
  response <- either exceptionResponse successResponse =<< try run
  pure (ServerNormalResponse response [] StatusOk "")
  where
    run = do
      buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv executeCommandEnv) argv)
      args <- toGhcArgs buckArgs
      bracket (startJob status) (finishJob status) \ _ -> do
        log <- newLog True
        let env = Env {log, cache, args}
        result <- withGhc env (compileAndReadAbiHash buckArgs)
        pure (env, buckArgs, result)

    successResponse (env, buckArgs, result) = do
      executeResponseExitCode <- writeResult buckArgs result
      output <- logFlush env.log
      pure ExecuteResponse {
        executeResponseExitCode,
        executeResponseStderr = LazyText.unlines (LazyText.pack <$> output)
      }

    exceptionResponse (SomeException e) =
      pure ExecuteResponse {
        executeResponseExitCode = 1,
        executeResponseStderr = "Uncaught exception: " <> LazyText.pack (show e)
      }

    argv = Text.unpack . decodeUtf8Lenient <$> Vector.toList executeCommandArgv

execHandler ::
  ServerRequest 'ClientStreaming ExecuteEvent ExecuteResponse ->
  IO (ServerResponse 'ClientStreaming ExecuteResponse)
execHandler (ServerReaderRequest _metadata _recv) = do
  hPutStrLn stderr "Received Exec"
  error "not implemented"

ghcServerHandlers ::
  MVar WorkerStatus ->
  MVar Cache ->
  Worker ServerRequest ServerResponse
ghcServerHandlers status cache =
  Worker {
    workerExecute = executeHandler status cache,
    workerExec = execHandler
  }

setSocket :: String -> ServiceOptions -> ServiceOptions
setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
newtype ServerSocketPath =
  ServerSocketPath { path :: FilePath }
  deriving stock (Eq, Show)

envServerSocket :: IO ServerSocketPath
envServerSocket = ServerSocketPath <$> getEnv "WORKER_SOCKET"

-- | The file system path of the socket on which the primary worker running the GHC server is listening.
newtype PrimarySocketPath =
  PrimarySocketPath { path :: FilePath }
  deriving stock (Eq, Show)

-- | The file system path in which the primary worker running the GHC server stores its socket path for clients to
-- discover.
newtype PrimarySocketDiscoveryPath =
  PrimarySocketDiscoveryPath { path :: FilePath }
  deriving stock (Eq, Show)

primarySocketDiscoveryIn :: FilePath -> PrimarySocketDiscoveryPath
primarySocketDiscoveryIn dir = PrimarySocketDiscoveryPath (dir </> "primary")

-- | Start a gRPC server that dispatches requests to GHC handlers.
runLocalGhc :: ServerSocketPath -> IO ()
runLocalGhc socket = do
  dbg ("Starting ghc server on " ++ socket.path)
  cache <- emptyCache False
  status <- newMVar WorkerStatus {active = 0}
  workerServer (ghcServerHandlers status cache) (setSocket socket.path defaultServiceOptions)

-- | Start a gRPC server that runs GHC for client proxies, deleting the discovery file on shutdown.
runCentralGhc :: PrimarySocketDiscoveryPath -> ServerSocketPath -> IO ()
runCentralGhc discovery socket =
  finally (runLocalGhc socket) do
    dbg ("Shutting down ghc server on " ++ socket.path)
    removeFile discovery.path

-- | Send a request received from a client to another gRPC server by converting between client and server data types and
-- calling the provided request callback.
forwardRequest ::
  (ClientError -> a) ->
  (ClientRequest 'Normal c a -> IO (ClientResult 'Normal a)) ->
  ServerRequest 'Normal c a ->
  IO (ServerResponse 'Normal a)
forwardRequest errorResponse send (ServerNormalRequest _ cmd) =
  send (ClientNormalRequest cmd 3600 []) <&> \case
    ClientNormalResponse response _ _ status _ ->
      ServerNormalResponse response [] status ""
    ClientErrorResponse err ->
      ServerNormalResponse (errorResponse err) [] StatusOk ""

-- | Bracket a computation with a gRPC worker client resource, connecting to the server listening on the provided
-- socket.
withProxy ::
  PrimarySocketPath ->
  (Worker ServerRequest ServerResponse -> IO a) ->
  IO a
withProxy socket use = do
  withGRPCClient cfg \ client -> do
    Worker {workerExecute = send} <- workerClient client
    use Worker
      { workerExecute = forwardRequest errorResponse send,
        workerExec = execHandler
      }
  where
    cfg = ClientConfig {
      clientServerEndpoint = fromString ("unix://" <> socket.path <> "\x00"),
      clientSSLConfig = Nothing,
      clientAuthority = Nothing,
      clientArgs = []
    }

    errorResponse err =
      ExecuteResponse {
        executeResponseExitCode = 1,
        executeResponseStderr = "gRPC client error: " <> LazyText.pack (show err)
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
      withProxy primary \ worker -> do
        dbg ("Starting proxy for " ++ primary.path ++ " on " ++ socket.path)
        workerServer worker (setSocket socket.path defaultServiceOptions)

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
runOrProxyCentralGhc :: ServerSocketPath -> IO ()
runOrProxyCentralGhc socket = do
  void $ try @IOError (createDirectory dir)
  result <- withFileBlocking primaryFile.path ReadWriteMode \ handle -> do
    try @IOError (hGetLine handle) >>= \case
      -- If the file didn't exist, `hGetLine` will still return the empty string.
      -- File IO is buffered/lazy, so we have to force the pattern to avoid read after close (though this is already
      -- achieved by calling `null`).
      Right !primary | not (null primary) -> do
        pure (Left (PrimarySocketPath primary))
      _ -> do
        thread <- async (runCentralGhc primaryFile socket)
        hPutStr handle socket.path
        pure (Right thread)
  case result of
    Right thread -> onException (wait thread) (cancel thread)
    Left primary -> proxyServer primary socket
  where
    primaryFile = primarySocketDiscoveryIn dir
    dir = init (dropWhileEnd ('-' /=) (takeDirectory socket.path))

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | Should only a single central GHC server be run, with all other worker processes proxying it?
    single :: Bool
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {single = False}

parseOptions :: [String] -> IO CliOptions
parseOptions =
  flip foldM defaultCliOptions \ z -> \case
    "--single" -> pure z {single = True}
    arg -> throwIO (userError ("Invalid worker CLI arg: " ++ arg))

runWorker :: ServerSocketPath -> CliOptions -> IO ()
runWorker socket CliOptions {single} =
  if single
  then runOrProxyCentralGhc socket
  else runLocalGhc socket

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseOptions =<< getArgs
  socket <- envServerSocket
  hPutStrLn stderr $ "using worker socket: " <> show socket
  try (runWorker socket options) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) -> do
      dbg ("Worker terminated with exception: " ++ displayException err)
