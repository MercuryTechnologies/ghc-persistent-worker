{-# LANGUAGE DataKinds, GADTs, MultiWayIf #-}

module Main where

import qualified BuckArgs as BuckArgs
import BuckArgs (BuckArgs, CompileResult (..), Mode (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (
  ExecuteCommand,
  ExecuteCommand'EnvironmentEntry,
  ExecuteEvent,
  ExecuteResponse,
  Instrument (..),
  Worker (..),
  )
import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import Control.Exception (Exception (..), SomeException (..), bracket, finally, onException, throwIO, try)
import Control.Monad (foldM, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC (DynFlags (..), Ghc, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GHC.IO.Handle.FD (withFileBlocking)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), CacheFeatures (..), ModuleArtifacts (..), Target (..), emptyCache, emptyCacheWith)
import Internal.Compile (compileModuleWithDepsInEps)
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (dbg, logFlush, newLog)
import Internal.Metadata (computeMetadata)
import Internal.Session (Env (..), withGhc, withGhcMhu)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (NextElem (..), Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&), (.~), (^.))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.Run (InsecureConfig (..), ServerConfig (..), runServerWithHandlers)
import Network.GRPC.Server.StreamType (
  Methods (..),
  fromMethods,
  mkClientStreaming,
  mkNonStreaming,
  mkServerStreaming,
  simpleMethods,
  )
import Prelude hiding (log)
import qualified Proto.Instrument as Instr
import Proto.Instrument_Fields qualified as Instr
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectory, removeFile)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), IOMode (..), hGetLine, hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

data WorkerStatus =
  WorkerStatus {
    active :: Int
  }

data WorkerMode =
  WorkerMakeMode
  |
  WorkerOneshotMode
  deriving stock (Eq, Show)

commandEnv :: [Proto ExecuteCommand'EnvironmentEntry] -> Map String String
commandEnv =
  Map.fromList .
  fmap (\kv -> (fromBs kv.key, fromBs kv.value))
  where
    fromBs = Text.unpack . decodeUtf8Lenient

-- | Compile a single module.
-- Depending on @mode@ this will either use the old EPS-based oneshot-style compilation logic or the HPT-based
-- make-style implementation.
compileAndReadAbiHash ::
  GhcMode ->
  (Target -> Ghc (Maybe ModuleArtifacts)) ->
  Chan (Proto Instr.Event) ->
  BuckArgs ->
  [String] ->
  Target ->
  Ghc (Maybe CompileResult)
compileAndReadAbiHash ghcMode compile instrChan args target = do

  liftIO $ writeChan instrChan $
    defMessage &
      Instr.compileStart .~
        compileStart target.get

  modifySession $ hscUpdateFlags \ d -> d {ghcMode}
  compile target >>= traverse \ artifacts -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash {path, hash = showAbiHash hsc_env artifacts.iface}
    pure CompileResult {artifacts, abiHash}
  where
    (ghcMode, compile) = case mode of
      WorkerOneshotMode -> (OneShot, compileModuleWithDepsInEps)
      WorkerMakeMode -> (CompManager, compileModuleWithDepsInHpt specific)

-- | Process a worker request based on the operational mode specified in the request arguments, either compiling a
-- single module for 'ModeCompile' (@-c@), or computing and writing the module graph to a JSON file for 'ModeMetadata'
-- (@-M@).
dispatch ::
  WorkerMode ->
  Chan (Proto Instr.Event) ->
  Env ->
  BuckArgs ->
  IO Int32
dispatch workerMode instrChan env args =
  case args.mode of
    Just ModeCompile -> do
      result <- withGhcMhu env (compileAndReadAbiHash workerMode instrChan args)
      writeResult args result
    Just m -> error ("worker: mode not implemented: " ++ show m)
    Nothing -> error "worker: no mode specified"

-- | Process a worker request based on the operational mode specified in the request arguments, either compiling a
-- single module for 'ModeCompile' (@-c@), or computing and writing the module graph to a JSON file for 'ModeMetadata'
-- (@-M@).
dispatch ::
  WorkerMode ->
  Chan (Proto Instr.Event) ->
  Env ->
  BuckArgs ->
  IO Int32
dispatch workerMode instrChan env args =
  case args.mode of
    Just ModeCompile -> do
      result <- compile
      writeResult args result
    Just ModeMetadata ->
      computeMetadata env <&> \case
        True -> 0
        False -> 1
    Just m -> error ("worker: mode not implemented: " ++ show m)
    Nothing -> error "worker: no mode specified"
  where
    compile = case workerMode of
      WorkerOneshotMode ->
        withGhc env (compileAndReadAbiHash OneShot compileModuleWithDepsInEps instrChan args)
      WorkerMakeMode ->
        withGhcMhu env \ specific ->
          compileAndReadAbiHash CompManager (compileModuleWithDepsInHpt specific) instrChan args

compileStart :: String -> Proto Instr.CompileStart
compileStart target =
  defMessage
    & Instr.target .~ Text.pack target

compileEnd :: String -> Int -> String -> Proto Instr.CompileEnd
compileEnd target exitCode err =
  defMessage
    & Instr.target .~ Text.pack target
    & Instr.exitCode .~ fromIntegral exitCode
    & Instr.stderr .~ Text.pack err

mkStats :: IO (Proto Instr.Stats)
mkStats = do
  s <- getRTSStats
  pure $
    defMessage
      & Instr.memory .~ fromIntegral s.gc.gcdetails_mem_in_use_bytes
      & Instr.gcCpuNs .~ s.gc_cpu_ns
      & Instr.cpuNs .~ s.cpu_ns

startJob ::
  MVar WorkerStatus ->
  IO ()
startJob var =
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active + 1
    dbg ("Starting job, now " ++ show new ++ " active")
    pure ws {active = new}

finishJob ::
  MVar WorkerStatus ->
  a ->
  IO ()
finishJob var _ = do
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure ws {active = new}

debugRequestArgs :: Bool
debugRequestArgs = False

execute ::
  MVar WorkerStatus ->
  MVar Cache ->
  WorkerMode ->
  Chan (Proto Instr.Event) ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
execute status cache mode instrChan req = do
  when debugRequestArgs do
    hPutStrLn stderr (unlines argv)
  msg <- either exceptionResponse successResponse =<< try run

  writeChan instrChan $
    defMessage &
      Instr.compileEnd .~
        compileEnd
          ""
          (fromIntegral $ msg ^. Fields.exitCode)
          (Text.unpack (msg ^. Fields.stderr))

  stats <- mkStats
  writeChan instrChan $
    defMessage &
      Instr.stats .~ stats

  pure msg
  where
    run = do
      buckArgs <- either (throwIO . userError) pure (parseBuckArgs (commandEnv req.env) argv)
      args <- toGhcArgs buckArgs
      bracket (startJob status) (finishJob status) \ _ -> do
        log <- newLog True
        let env = Env {log, cache, args}
        result <- dispatch mode instrChan env buckArgs
        pure (env, result)

    successResponse (env, exitCode) = do
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
  WorkerMode ->
  Chan (Proto Instr.Event) ->
  Methods IO (ProtobufMethodsOf Worker)
ghcServerMethods status cache mode instrChan =
  simpleMethods
    (mkClientStreaming exec)
    (mkNonStreaming (execute status cache mode instrChan))

notifyMe ::
  Chan (Proto Instr.Event) ->
  (NextElem (Proto Instr.Event) -> IO ()) ->
  IO ()
notifyMe chan callback = do
  myChan <- dupChan chan
  stats <- mkStats
  callback $ NextElem $
    defMessage
      & Instr.stats .~ stats
  forever $ do
    msg <- readChan myChan
    callback $ NextElem msg

instrumentMethods ::
  Chan (Proto Instr.Event) ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan =
  simpleMethods
    (mkServerStreaming (const (notifyMe chan)))

grpcServerConfig :: FilePath -> ServerConfig
grpcServerConfig socketPath =
  ServerConfig
    { serverInsecure = Just (InsecureUnix socketPath)
    , serverSecure = Nothing
    }

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

-- | Start a gRPC server that dispatches requests to GHC handlers.
runLocalGhc :: WorkerMode -> ServerSocketPath -> Maybe InstrumentSocketPath -> IO ()
runLocalGhc mode socket minstr = do
  dbg ("Starting ghc server on " ++ socket.path)
  cache <-
    case mode of
      WorkerMakeMode ->
        emptyCacheWith CacheFeatures {
          hpt = True,
          loader = False,
          enable = True,
          names = False,
          finder = True,
          eps = False
        }
      WorkerOneshotMode -> emptyCache True
  status <- newMVar WorkerStatus {active = 0}
  instrChan <- newChan

  for_ minstr $ \instr -> do
    dbg ("Instrumentation info available on " ++ instr.path)
    async $ runServerWithHandlers def (grpcServerConfig instr.path) $ fromMethods (instrumentMethods instrChan)

  runServerWithHandlers def (grpcServerConfig socket.path) $ fromMethods (ghcServerMethods status cache mode instrChan)

-- | Start a gRPC server that runs GHC for client proxies, deleting the discovery file on shutdown.
runCentralGhc :: WorkerMode -> PrimarySocketDiscoveryPath -> ServerSocketPath -> Maybe InstrumentSocketPath -> IO ()
runCentralGhc mode discovery socket instr =
  finally (runLocalGhc mode socket instr) do
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
      Method (mkClientStreaming exec) $
      Method (mkNonStreaming (forwardRequest connection)) $
      NoMoreMethods
  where
    server = ServerUnix socket.path

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
runOrProxyCentralGhc :: WorkerMode -> ServerSocketPath -> IO ()
runOrProxyCentralGhc mode socket = do
  void $ try @IOError (createDirectory dir)
  result <- withFileBlocking primaryFile.path ReadWriteMode \ handle -> do
    try @IOError (hGetLine handle) >>= \case
      -- If the file didn't exist, `hGetLine` will still return the empty string.
      -- File IO is buffered/lazy, so we have to force the pattern to avoid read after close (though this is already
      -- achieved by calling `null`).
      Right !primary | not (null primary) -> do
        pure (Left (PrimarySocketPath primary))
      _ -> do
        thread <- async (runCentralGhc mode primaryFile socket instr)
        hPutStr handle socket.path
        pure (Right thread)
  case result of
    Right thread -> onException (wait thread) (cancel thread)
    Left primary -> proxyServer primary socket
  where
    primaryFile = primarySocketDiscoveryIn dir
    instr = Just (instrumentSocketIn dir)
    dir = init (dropWhileEnd ('-' /=) (takeDirectory socket.path))

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | Should only a single central GHC server be run, with all other worker processes proxying it?
    single :: Bool,

    -- | The worker implementation: Make mode or oneshot mode.
    mode :: WorkerMode
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {single = False, mode = WorkerOneshotMode}

parseOptions :: [String] -> IO CliOptions
parseOptions =
  flip foldM defaultCliOptions \ z -> \case
    "--single" -> pure z {single = True}
    "--make" -> pure z {mode = WorkerMakeMode}
    arg -> throwIO (userError ("Invalid worker CLI arg: " ++ arg))

runWorker :: ServerSocketPath -> CliOptions -> IO ()
runWorker socket CliOptions {single, mode} =
  if single
  then runOrProxyCentralGhc mode socket
  else runLocalGhc mode socket Nothing

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
    Left (err :: SomeException) ->
      dbg ("Worker terminated with exception: " ++ displayException err)
