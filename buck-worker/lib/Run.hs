module Run where

import qualified BuckArgs as BuckArgs
import BuckArgs (Mode (..), emptyBuckArgs)
import BuckWorker (Instrument, Worker)
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Control.Exception (throwIO)
import Data.Foldable (for_)
import GhcHandler (WorkerMode (..), dispatch, ghcHandler)
import Grpc (ghcServerMethods, instrumentMethods)
import Instrumentation (WorkerStatus (..), hooksNoop, toGrpcHandler)
import Internal.Args (Args (..), emptyArgs)
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCache, emptyCacheWith, logMemStats)
import Internal.Log (dbg, newLog)
import Internal.Session (Env (..))
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import Orchestration (
  CreateMethods (..),
  FeatureInstrument (..),
  Orchestration (..),
  ServerSocketPath (..),
  WorkerExe (..),
  runCentralGhcSpawned,
  runLocalGhc,
  serveOrProxyCentralGhc,
  spawnOrProxyCentralGhc,
  )
import qualified Proto.Instrument as Instr
import System.Directory (createDirectoryIfMissing)
import System.FilePath (addExtension, replaceExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | Should only a single central GHC server be run, with all other worker processes proxying it?
    orchestration :: Orchestration,

    -- | The worker implementation: Make mode or oneshot mode.
    workerMode :: WorkerMode,

    -- | The path to the @buck-worker@ executable.
    -- Usually this is the same executable that started the process, but we cannot access it reliably.
    -- Used to spawn the GHC server, provided by Buck.
    workerExe :: Maybe WorkerExe,

    -- | If this is given, the app should start a GHC server synchronously, listening on the given path.
    serve :: Maybe ServerSocketPath,

    instrument :: FeatureInstrument
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions {
    orchestration = Multi,
    workerMode = WorkerOneshotMode,
    workerExe = Nothing,
    serve = Nothing,
    instrument = FeatureInstrument False
  }

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--single" : rest -> spin z {orchestration = Single} rest
      "--spawn" : rest -> spin z {orchestration = Spawn} rest
      "--make" : rest -> spin z {workerMode = WorkerMakeMode} rest
      "--exe" : exe : rest -> spin z {workerExe = Just (WorkerExe exe)} rest
      "--serve" : socket : rest -> spin z {serve = Just (ServerSocketPath socket)} rest
      "--instrument" : rest -> spin z {instrument = FeatureInstrument True} rest
      arg -> throwIO (userError ("Invalid worker CLI args: " ++ unwords arg))

-- | Allocate a communication channel for instrumentation events and construct a gRPC server handler that streams said
-- events to a client.
--
-- Returns the channel so that a GHC server can use it to send events.
createInstrumentMethods :: MVar Cache -> IO (Chan (Proto Instr.Event), Methods IO (ProtobufMethodsOf Instrument))
createInstrumentMethods cacheVar = do
  instrChan <- newChan
  pure (instrChan, instrumentMethods instrChan cacheVar)

-- | Construct a gRPC server handler for the main part of the persistent worker.
createGhcMethods ::
  MVar Cache ->
  WorkerMode ->
  MVar WorkerStatus ->
  Maybe (Chan (Proto Instr.Event)) ->
  IO (Methods IO (ProtobufMethodsOf Worker))
createGhcMethods cache workerMode status instrChan = do
  counter <- newMVar 0
  pure (ghcServerMethods (toGrpcHandler (ghcHandler counter cache workerMode) status cache instrChan))

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: ServerSocketPath -> CliOptions -> IO ()
runWorker socket CliOptions {orchestration, workerMode, workerExe, serve, instrument} = do
  cache <-
    case workerMode of
      WorkerMakeMode ->
        emptyCacheWith CacheFeatures {
          hpt = True,
          loader = False,
          enable = True,
          names = False,
          finder = False,
          eps = False
        }
      WorkerOneshotMode -> emptyCache True
  status <- newMVar WorkerStatus {active = 0}
  let
    methods = CreateMethods {
      createInstrumentation = createInstrumentMethods cache,
      createGhc = createGhcMethods cache workerMode status
    }
    runSpawn = do
      exe <- case workerExe of
        Just exe -> pure exe
        Nothing -> throwIO (userError "Spawn mode requires specifying the worker executable with '--exe'")
      spawnOrProxyCentralGhc exe socket
  case serve of
    Just serverSocket -> runCentralGhcSpawned methods instrument serverSocket
    Nothing ->
      case orchestration of
        Single -> serveOrProxyCentralGhc methods socket
        Multi -> runLocalGhc methods socket Nothing
        Spawn -> runSpawn

batchCompileWith ::
  FilePath ->
  Env ->
  [FilePath] ->
  IO ()
batchCompileWith tmp env paths = do
  createDirectoryIfMissing True out
  _ <- dispatch WorkerMakeMode hooksNoop (withOptions (metadataOptions ++ paths)) buckArgs {BuckArgs.mode = Just ModeMetadata}
  for_ paths \ path -> do
    let name = takeFileName path
    dbg name
    dispatch WorkerMakeMode hooksNoop (withOptions (compileOptions name ++ [path])) buckArgs {BuckArgs.mode = Just ModeCompile, BuckArgs.abiOut = Just (out </> addExtension name "hash")}
  where
    out = tmp </> "out"

    withOptions opts = env {args = env.args {ghcOptions = env.args.ghcOptions ++ opts}}

    compileOptions name =
      [
        "-fwrite-ide-info",
        "-no-link",
        "-i",
        "-hide-all-packages",
        "-dynamic",
        "-fPIC",
        "-osuf", "dyn_o",
        "-hisuf", "dyn_hi",
        "-o", out </> replaceExtension name "dyn_o",
        "-ohi", out </> replaceExtension name "dyn_hi",
        "-odir", out,
        "-hiedir", out,
        "-dumpdir", out,
        "-stubdir", out,
        "-i" ++ out,
        "-package", "base",
        "-package", "template-haskell",
        "-fbyte-code-and-object-code"
      ]

    metadataOptions = [
      "-i",
      "-hide-all-packages",
      "-include-pkg-deps",
      "-package", "base",
      "-package", "template-haskell",
      "-dep-json", out </> "depends.json",
      "-dep-makefile", out </> "depends.make",
      "-outputdir", "."
      ]

    buckArgs = emptyBuckArgs mempty


batchCompile :: [FilePath] -> IO ()
batchCompile paths =
  withSystemTempDirectory "batch-worker" \ tmp -> do
  logVar <- newLog True
  cache <- emptyCacheWith CacheFeatures {
          hpt = True,
          loader = False,
          enable = True,
          names = False,
          finder = False,
          eps = False
        }
  let ghcTemp = tmp </> "ghc-tmp"
      args = (emptyArgs mempty) {
        -- This is what Buck uses
        -- topdir = Just "/nix/store/v6n3b9gc39apd7hh9cxs7rz3xi9vif3g-ghc-9.10.1/lib/ghc-9.10.1/lib/",

        -- This is the vanilla GHC directory
        topdir = Just "/nix/store/zlw55w44mid9acbhkqqm8frjsd6mjwhg-ghc-9.10.1-with-packages/lib/ghc-9.10.1/lib/",
        tempDir = Just ghcTemp,
        ghcOptions = [
          "-this-unit-id", "test"
        ]
      }
      env = Env {log = logVar, cache, args}
  createDirectoryIfMissing True ghcTemp
  batchCompileWith tmp env paths
  logMemStats "final" logVar
