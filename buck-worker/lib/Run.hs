module Run where

import BuckWorker (Instrument, Worker)
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Control.Exception (throwIO)
import GhcHandler (WorkerMode (..), ghcHandler)
import Grpc (ghcServerMethods, instrumentMethods)
import Instrumentation (WorkerStatus (..), toGrpcHandler)
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCache, emptyCacheWith)
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import Orchestration (
  CreateMethods (..),
  Orchestration (..),
  ServerSocketPath (..),
  WorkerExe (..),
  runCentralGhcSpawned,
  serverSocketFromPath,
  spawnOrProxyCentralGhc,
  )
import qualified Proto.Instrument as Instr

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
    serve :: Maybe ServerSocketPath
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions {
    orchestration = Multi,
    workerMode = WorkerOneshotMode,
    workerExe = Nothing,
    serve = Nothing
  }

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--single" : rest -> spin z {orchestration = Single} rest
      "--make" : rest -> spin z {workerMode = WorkerMakeMode} rest
      "--exe" : exe : rest -> spin z {workerExe = Just (WorkerExe exe)} rest
      "--serve" : socket : rest -> spin z {serve = Just (serverSocketFromPath socket)} rest
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
createGhcMethods cache workerMode status instrChan =
  pure (ghcServerMethods (toGrpcHandler (ghcHandler cache workerMode) status cache instrChan))

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: ServerSocketPath -> CliOptions -> IO ()
runWorker socket CliOptions {orchestration, workerMode, workerExe, serve} = do
  cache <-
    case workerMode of
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
      createInstrumentation = createInstrumentMethods cache,
      createGhc = createGhcMethods cache workerMode status
    }
    runSpawn = do
      exe <- case workerExe of
        Just exe -> pure exe
        Nothing -> throwIO (userError "Spawn mode requires specifying the worker executable with '--exe'")
      spawnOrProxyCentralGhc exe orchestration workerMode socket
  case serve of
    Just serverSocket -> runCentralGhcSpawned methods serverSocket
    Nothing -> runSpawn
