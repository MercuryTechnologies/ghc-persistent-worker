{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Applicative (asum)
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
import Control.Monad (foldM, when)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC (Ghc, getSession)
import GHC.IO.Handle.FD (withFileBlocking)
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), ModuleArtifacts (..), Target, emptyCache)
import Internal.Compile (compile)
import Internal.Log (dbg, logFlush, newLog)
import Internal.Session (Env (..), withGhc)
import Network.GRPC.Client (Address (Address), Connection, Server (ServerInsecure), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (
  NextElem,
  Proxy (..),
  def,
 )
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (InsecureConfig), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (Methods (Method, NoMoreMethods), fromMethods, mkClientStreaming, mkNonStreaming)
import Network.HTTP2.TLS.Server (PortNumber)
import System.Directory (removeFile)
import System.Environment (getArgs, getEnv, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), IOMode (..), hGetLine, hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)
import Prelude hiding (log)

import BuckArgs (BuckArgs (..), CompileResult (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (ExecuteCommand, ExecuteCommand'EnvironmentEntry, ExecuteEvent, ExecuteResponse, Worker (..))
import Proto.Worker_Fields qualified as Fields

data WorkerStatus
  = WorkerStatus
  { active :: Int
  }
  deriving stock (Eq, Show)

commandEnv :: [Proto ExecuteCommand'EnvironmentEntry] -> Map String String
commandEnv =
  Map.fromList
    . fmap \kv -> (fromBs kv.key, fromBs kv.value)
 where
  fromBs = Text.unpack . decodeUtf8Lenient

compileAndReadAbiHash :: BuckArgs -> Target -> Ghc (Maybe CompileResult)
compileAndReadAbiHash args target = do
  compile target >>= traverse \artifacts -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash{path, hash = showAbiHash hsc_env artifacts.iface}
    pure CompileResult{artifacts, abiHash}

startJob ::
  MVar WorkerStatus ->
  IO ()
startJob var =
  modifyMVar_ var \WorkerStatus{active} -> do
    let new = active + 1
    dbg ("Starting job, now " ++ show new ++ " active")
    pure WorkerStatus{active = new}

finishJob ::
  MVar WorkerStatus ->
  a ->
  IO ()
finishJob var _ = do
  modifyMVar_ var \WorkerStatus{active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure WorkerStatus{active = new}

debugRequestArgs :: Bool
debugRequestArgs = False

execute ::
  MVar WorkerStatus ->
  MVar Cache ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
execute status cache req = do
  when debugRequestArgs do
    hPutStrLn stderr (unlines argv)
  either exceptionResponse successResponse =<< try run
 where
  run = do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv req.env) argv)
    args <- toGhcArgs buckArgs
    bracket (startJob status) (finishJob status) \_ -> do
      log <- newLog False
      let env = Env{log, cache, args}
      result <- withGhc env (compileAndReadAbiHash buckArgs)
      pure (env, buckArgs, result)

  successResponse (env, buckArgs, result) = do
    exitCode <- writeResult buckArgs result
    output <- logFlush env.log
    pure $
      defMessage
        & Fields.exitCode
        .~ exitCode
        & Fields.stderr
        .~ Text.unlines (Text.pack <$> output)

  exceptionResponse (SomeException e) =
    pure $
      defMessage
        & Fields.exitCode
        .~ 1
        & Fields.stderr
        .~ "Uncaught exception: "
        <> Text.pack (show e)

  argv = Text.unpack . decodeUtf8Lenient <$> req.argv

exec :: IO (NextElem (Proto ExecuteEvent)) -> IO (Proto ExecuteResponse)
exec _ =
  pure $
    defMessage
      & Fields.exitCode
      .~ 1
      & Fields.stderr
      .~ "Streaming not implemented"

ghcServerMethods ::
  MVar WorkerStatus ->
  MVar Cache ->
  Methods IO (ProtobufMethodsOf Worker)
ghcServerMethods status cache =
  Method (mkClientStreaming exec) $
    Method (mkNonStreaming (execute status cache)) $
      NoMoreMethods

-- | The file system nr of the port on which the worker running in this process is supposed to listen.
newtype ServerPortNumber
  = ServerPortNumber {nr :: PortNumber}
  deriving stock (Eq, Show)

envServerPortNumber :: IO ServerPortNumber
envServerPortNumber = ServerPortNumber . read <$> getEnv "WORKER_PORT"

-- | The file system nr of the port on which the primary worker running the GHC server is listening.
newtype PrimaryPortNumber
  = PrimaryPortNumber {nr :: PortNumber}
  deriving stock (Eq, Show)

{- | The file system nr in which the primary worker running the GHC server stores its port nr for clients to
discover.
-}
newtype PrimaryPortDiscoveryPath
  = PrimaryPortDiscoveryPath {path :: FilePath}
  deriving stock (Eq, Show)

primaryPortDiscoveryIn :: FilePath -> PrimaryPortDiscoveryPath
primaryPortDiscoveryIn dir = PrimaryPortDiscoveryPath (dir </> "primary")

-- | Start a gRPC server that dispatches requests to GHC handlers.
runLocalGhc :: ServerPortNumber -> IO ()
runLocalGhc port = do
  dbg ("Starting ghc server on " ++ show port.nr)
  cache <- emptyCache True
  status <- newMVar WorkerStatus{active = 0}
  runServerWithHandlers def config $ fromMethods (ghcServerMethods status cache)
 where
  config :: ServerConfig
  config =
    ServerConfig
      { serverInsecure = Just (InsecureConfig Nothing port.nr)
      , serverSecure = Nothing
      }

-- | Start a gRPC server that runs GHC for client proxies, deleting the discovery file on shutdown.
runCentralGhc :: PrimaryPortDiscoveryPath -> ServerPortNumber -> IO ()
runCentralGhc discovery port =
  finally (runLocalGhc port) do
    dbg ("Shutting down ghc server on " ++ show port.nr)
    removeFile discovery.path

{- | Send a request received from a client to another gRPC server by converting between client and server data types and
calling the provided request callback.
-}
forwardRequest ::
  Connection ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
forwardRequest connection req = withRPC connection def (Proxy @(Protobuf Worker "execute")) \call -> do
  sendFinalInput call req
  resp <- recvNextOutput call
  pure $
    resp
      & Fields.stderr
      %~ ("gRPC client error: " <>)

{- | Bracket a computation with a gRPC worker client resource, connecting to the server listening on the provided
port.
-}
withProxy ::
  PrimaryPortNumber ->
  (Methods IO (ProtobufMethodsOf Worker) -> IO a) ->
  IO a
withProxy port use = do
  withConnection def server $ \connection -> do
    use $
      Method (mkClientStreaming exec) $
        Method (mkNonStreaming (forwardRequest connection)) $
          NoMoreMethods
 where
  server = ServerInsecure (Address "localhost" port.nr Nothing)

{- | Start a worker gRPC server that forwards requests received from a client (here Buck) to another gRPC server (here
our GHC primary).
-}
proxyServer :: PrimaryPortNumber -> ServerPortNumber -> IO ()
proxyServer primary port = do
  try launch >>= \case
    Right () ->
      dbg ("Shutting down proxy on " ++ show port.nr ++ " regularly")
    Left (err :: IOError) -> do
      dbg ("Proxy on " ++ show port.nr ++ " crashed: " ++ show err)
      exitFailure
 where
  launch =
    withProxy primary \methods -> do
      dbg ("Starting proxy for " ++ show primary.nr ++ " on " ++ show port.nr)
      runServerWithHandlers def config $ fromMethods methods
  config :: ServerConfig
  config =
    ServerConfig
      { serverInsecure = Just (InsecureConfig Nothing port.nr)
      , serverSecure = Nothing
      }

{- | Start a gRPC server that either runs GHC (primary server) or a proxy that forwards requests to the primary.
Since multiple workers are started in separate processes, we negotiate using file system locks.
There are two major scenarios:

- When the build is started, multiple workers are spawned concurrently, and no primary exists.
  We use the temp dir to create a lock file.
  The first worker that wins the lock in `withFileBlocking` gets to be primary, and writes its port number to
  `/tmp/buck2_worker/<hash>/primary`.
  All other workers then own the lock in sequence and proceed with the second scenario.

- When the build is running and a primary exists, either because the worker lost the inital lock race or was started
  later in the build due to dependencies and/or parallelism limits, the contents of the `primary` file are read to
  obtain the primary's port number.
  A gRPC server is started that resends all requests to that port.
-}
runOrProxyCentralGhc :: ServerPortNumber -> IO ()
runOrProxyCentralGhc port = do
  Just primaryFile <- fmap primaryPortDiscoveryIn . asum <$> traverse @[] lookupEnv ["TMPDIR", "TEMP", "TMP"]
  result <- withFileBlocking primaryFile.path ReadWriteMode \handle -> do
    try @IOError (hGetLine handle) >>= \case
      -- If the file didn't exist, `hGetLine` will still return the empty string.
      -- File IO is buffered/lazy, so we have to force the pattern to avoid read after close (though this is already
      -- achieved by calling `null`).
      Right !primary | not (null primary) -> do
        pure (Left (PrimaryPortNumber $ read primary))
      _ -> do
        thread <- async (runCentralGhc primaryFile port)
        hPutStr handle $ show port.nr
        pure (Right thread)
  case result of
    Right thread -> onException (wait thread) (cancel thread)
    Left primary -> proxyServer primary port

{- | Global options for the worker, passed when the process is started, in contrast to request options stored in
'BuckArgs'.
-}
data CliOptions
  = CliOptions
  { single :: Bool
  -- ^ Should only a single central GHC server be run, with all other worker processes proxying it?
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions{single = False}

parseOptions :: [String] -> IO CliOptions
parseOptions =
  flip foldM defaultCliOptions \z -> \case
    "--single" -> pure z{single = True}
    arg -> throwIO (userError ("Invalid worker CLI arg: " ++ arg))

runWorker :: ServerPortNumber -> CliOptions -> IO ()
runWorker port CliOptions{single} =
  if single
    then runOrProxyCentralGhc port
    else runLocalGhc port

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options <- parseOptions =<< getArgs
  port <- envServerPortNumber
  hPutStrLn stderr $ "using worker port: " <> show port
  try (runWorker port options) >>= \case
    Right () ->
      dbg "Worker terminated without cancellation."
    Left (err :: SomeException) -> do
      dbg ("Worker terminated with exception: " ++ displayException err)
