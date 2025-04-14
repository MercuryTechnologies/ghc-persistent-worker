module Main where

import qualified BuckArgs as BuckArgs
import BuckArgs (BuckArgs, CompileResult (..), Mode (..), parseBuckArgs, toGhcArgs, writeResult)
import BuckWorker (ExecuteCommand, ExecuteResponse, Instrument, Worker)
import Control.Concurrent (MVar, modifyMVar_, newChan, newMVar, writeChan)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Exception (Exception (..), SomeException (..), bracket_, finally, throwIO, try)
import Control.Monad (foldM, forever, void, when)
import Control.Monad.Catch (onException)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Traversable (for)
import GHC (DynFlags (..), Ghc, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GHC.IO.Handle.FD (withFileBlocking)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Grpc (CommandEnv (..), RequestArgs (..))
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Cache (Cache (..), CacheFeatures (..), ModuleArtifacts (..), Target (..), emptyCache, emptyCacheWith)
import Internal.Compile (compileModuleWithDepsInEps)
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (dbg, logFlush, newLog)
import Internal.Metadata (computeMetadata)
import Internal.Session (Env (..), withGhc, withGhcMhu)
import Network.GRPC.Client (Connection, Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (NextElem (..), Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (%~), (&), (.~))
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
import Proto.Worker (ExecuteCommand'EnvironmentEntry, ExecuteEvent)
import Proto.Worker_Fields qualified as Fields
import System.Directory (createDirectory, removeFile)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), IOMode (..), hGetLine, hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

-- | A handler for gRPC requests takes a 'Map' of environment variables and a list of command line arguments and returns
-- a list of output message lines and an exit code.
newtype GrpcHandler =
  GrpcHandler {
    run ::
      CommandEnv ->
      RequestArgs ->
      IO ([String], Int32)
  }

data Hooks =
  Hooks {
    compileStart :: Maybe Target -> IO (),
    compileFinish :: Maybe ([String], Int32) -> IO ()
  }

hooksNoop :: Hooks
hooksNoop =
  Hooks {
    compileStart = const (pure ()),
    compileFinish = const (pure ())
  }

newtype InstrumentedHandler =
  InstrumentedHandler { create :: Hooks -> GrpcHandler }

data WorkerStatus =
  WorkerStatus {
    active :: Int
  }

-- | Selects the worker implementation.
data WorkerMode =
  WorkerMakeMode
  |
  WorkerOneshotMode
  deriving stock (Eq, Show)

commandEnv :: [Proto ExecuteCommand'EnvironmentEntry] -> CommandEnv
commandEnv =
  CommandEnv .
  Map.fromList .
  fmap \kv -> (fromBs kv.key, fromBs kv.value)
  where
    fromBs = Text.unpack . decodeUtf8Lenient

-- | Compile a single module.
-- Depending on @mode@ this will either use the old EPS-based oneshot-style compilation logic or the HPT-based
-- make-style implementation.
compileAndReadAbiHash ::
  GhcMode ->
  (Target -> Ghc (Maybe ModuleArtifacts)) ->
  Hooks ->
  BuckArgs ->
  Target ->
  Ghc (Maybe CompileResult)
compileAndReadAbiHash ghcMode compile hooks args target = do
  liftIO $ hooks.compileStart (Just target)
  modifySession $ hscUpdateFlags \ d -> d {ghcMode}
  compile target >>= traverse \ artifacts -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash {path, hash = showAbiHash hsc_env artifacts.iface}
    pure CompileResult {artifacts, abiHash}

-- | Process a worker request based on the operational mode specified in the request arguments, either compiling a
-- single module for 'ModeCompile' (@-c@), or computing and writing the module graph to a JSON file for 'ModeMetadata'
-- (@-M@).
dispatch ::
  WorkerMode ->
  Hooks ->
  Env ->
  BuckArgs ->
  IO Int32
dispatch workerMode hooks env args =
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
        withGhc env (compileAndReadAbiHash OneShot compileModuleWithDepsInEps hooks args)
      WorkerMakeMode ->
        withGhcMhu env \ specific ->
          compileAndReadAbiHash CompManager (compileModuleWithDepsInHpt specific) hooks args

messageCompileStart :: String -> Proto Instr.CompileStart
messageCompileStart target =
  defMessage
    & Instr.target .~ Text.pack target

messageCompileEnd :: String -> Int -> String -> Proto Instr.CompileEnd
messageCompileEnd target exitCode err =
  defMessage
    & Instr.target .~ Text.pack target
    & Instr.exitCode .~ fromIntegral exitCode
    & Instr.stderr .~ Text.pack err

-- | Fetch statistics about the current state of the RTS for instrumentation.
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
  IO ()
finishJob var = do
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure ws {active = new}

-- | Generic wrapper for a handler of the 'Worker' message 'ExecuteCommand', taking care of input data conversions and
-- response construction.

debugRequestArgs :: Bool
debugRequestArgs = False
execute ::
  GrpcHandler ->
  Proto ExecuteCommand ->
  IO (Proto ExecuteResponse)
execute handler req = do
  when debugRequestArgs do
    hPutStrLn stderr (unlines argv)
  (output, exitCode) <- either exceptionResult id <$> try (handler.run (commandEnv req.env) (RequestArgs argv))
  pure $
    defMessage
      & Fields.exitCode
      .~ exitCode
      & Fields.stderr
      .~ Text.unlines (Text.pack <$> output)
  where
    exceptionResult (SomeException e) =
      (["Uncaught exception: " ++ show e], 1)

    argv = Text.unpack . decodeUtf8Lenient <$> req.argv

-- | The worker protocol is intended to support streaming events, but we're not using that yet.
streamingNotImplemented :: IO (NextElem (Proto ExecuteEvent)) -> IO (Proto ExecuteResponse)
streamingNotImplemented _ =
  pure $
    defMessage
      & Fields.exitCode
      .~ 1
      & Fields.stderr
      .~ "Streaming not implemented"

-- | Wrap a 'GrpcHandler' in a grapesy handler data type.
ghcServerMethods ::
  GrpcHandler ->
  Methods IO (ProtobufMethodsOf Worker)
ghcServerMethods handler =
  simpleMethods
    (mkClientStreaming streamingNotImplemented)
    (mkNonStreaming (execute handler))

-- | Implementation of a streaming grapesy handler that sends instrumentation statistics pulled from the provided
-- channel to the client.
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

-- | A grapesy server that streams instrumentation data from the provided channel.
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
runCentralGhc :: CreateMethods -> PrimarySocketDiscoveryPath -> ServerSocketPath -> Maybe InstrumentSocketPath -> IO ()
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
      Method (mkClientStreaming streamingNotImplemented) $
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
runOrProxyCentralGhc :: CreateMethods -> ServerSocketPath -> IO ()
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

withInstrumentation ::
  Chan (Proto Instr.Event) ->
  MVar WorkerStatus ->
  InstrumentedHandler ->
  GrpcHandler
withInstrumentation instrChan status handler =
  GrpcHandler \ cmdEnv argv ->
    bracket_ (startJob status) (finishJob status) do
      result <- (handler.create hooks).run cmdEnv argv
      stats <- mkStats
      writeChan instrChan (defMessage & Instr.stats .~ stats)
      pure result
  where
    hooks = Hooks {
      compileStart,
      compileFinish
    }

    compileStart =
      traverse_ \ target ->
        writeChan instrChan $
          defMessage &
            Instr.compileStart .~
              messageCompileStart target.get

    compileFinish =
      traverse_ \ (output, exitCode) ->
        writeChan instrChan $
          defMessage &
            Instr.compileEnd .~
              messageCompileEnd "" (fromIntegral exitCode) (unlines output)

toGrpcHandler ::
  InstrumentedHandler ->
  MVar WorkerStatus ->
  Maybe (Chan (Proto Instr.Event)) ->
  GrpcHandler
toGrpcHandler createHandler status = \case
  Nothing -> createHandler.create hooksNoop
  Just instrChan -> withInstrumentation instrChan status createHandler

-- | Allocate a communication channel for instrumentation events and construct a gRPC server handler that streams said
-- events to a client.
--
-- Returns the channel so that a GHC server can use it to send events.
createInstrumentMethods :: IO (Chan (Proto Instr.Event), Methods IO (ProtobufMethodsOf Instrument))
createInstrumentMethods = do
  instrChan <- newChan
  pure (instrChan, instrumentMethods instrChan)

-- | Default implementation of an 'InstrumentedHandler' using our custom persistent worker GHC mode, either using HPT or
-- EPS for local dependency lookup.
--
-- Parses the request args from Buck, creates a new 'Log' and 'Env', and passes all of that to 'dispatch'.
-- Afterwards, extracts the log messages that GHC wrote to 'Log' and calls the instrumentation hook 'compileFinish',
-- providing the log and exit code.
--
-- If an exception was thrown, the hook is called without data.
ghcHandler ::
  MVar Cache ->
  WorkerMode ->
  InstrumentedHandler
ghcHandler cache workerMode =
  InstrumentedHandler \ hooks -> GrpcHandler \ cmdEnv argv -> do
    buckArgs <- either (throwIO . userError) pure (parseBuckArgs cmdEnv argv)
    args <- toGhcArgs buckArgs
    log <- newLog True
    let env = Env {log, cache, args}
    onException
      do
        result <- dispatch workerMode hooks env buckArgs
        output <- logFlush env.log
        liftIO $ hooks.compileFinish (Just (output, result))
        pure (output, result)
      do
        liftIO $ hooks.compileFinish Nothing

-- | Construct a gRPC server handler for the main part of the persistent worker.
createGhcMethods ::
  MVar Cache ->
  WorkerMode ->
  MVar WorkerStatus ->
  Maybe (Chan (Proto Instr.Event)) ->
  IO (Methods IO (ProtobufMethodsOf Worker))
createGhcMethods cache mode status instrChan =
  pure (ghcServerMethods (toGrpcHandler (ghcHandler cache mode) status instrChan))

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: ServerSocketPath -> CliOptions -> IO ()
runWorker socket CliOptions {single, mode} = do
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
  let
    methods = CreateMethods {
      createInstrumentation = createInstrumentMethods,
      createGhc = createGhcMethods cache mode status
    }
  if single
  then runOrProxyCentralGhc methods socket
  else runLocalGhc methods socket Nothing

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
