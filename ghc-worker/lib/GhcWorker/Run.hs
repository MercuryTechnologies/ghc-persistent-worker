module GhcWorker.Run where

import BuckWorker (Instrument, Worker)
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Control.Exception (throwIO)
import GhcWorker.GhcHandler (ghcHandler)
import GhcWorker.Grpc (ghcServerMethods, instrumentMethods)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (
  CreateMethods (..),
  runCentralGhcSpawned,
  )
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCache, emptyCacheWith)
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import qualified Proto.Instrument as Instr
import Types.GhcHandler (WorkerMode (..))
import Types.Orchestration (
  ServerSocketPath (..),
  serverSocketFromPath,
  )

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | The worker implementation: Make mode or oneshot mode.
    workerMode :: WorkerMode,

    -- | listening on the given path.
    serve :: ServerSocketPath
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions {
    workerMode = WorkerOneshotMode,
    serve = ServerSocketPath "" "" ""
  }

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--make" : rest -> spin z {workerMode = WorkerMakeMode} rest
      "--serve" : socket : rest -> spin z {serve = serverSocketFromPath socket} rest
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
runWorker :: CliOptions -> IO ()
runWorker CliOptions {workerMode, serve} = do
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
  runCentralGhcSpawned methods serve
