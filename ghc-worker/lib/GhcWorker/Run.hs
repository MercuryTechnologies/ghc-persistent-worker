module GhcWorker.Run where

import BuckWorker (Instrument, Worker)
import Common.Grpc (fromGrpcHandler, GrpcHandler(..))
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (throwIO)
import Data.Functor (void)
import GhcWorker.GhcHandler (LockState (..), ghcHandler)
import GhcWorker.Grpc (instrumentMethods)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (CreateMethods (..), FeatureInstrument (..), runCentralGhcSpawned)
import Internal.Log (TraceId (..))
import Internal.State (WorkerState (..), newState, newStateWith)
import Internal.State.Oneshot (OneshotCacheFeatures (..))
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import qualified Proto.Instrument as Instr
import Types.GhcHandler (WorkerMode (..))
import Types.Orchestration (
  ServerSocketPath (..),
  serverSocketFromPath,
  )
import Types.Grpc (CommandEnv, RequestArgs)

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | The worker implementation: Make mode or oneshot mode.
    workerMode :: WorkerMode,

    -- | If this is given, the app should start a GHC server synchronously, listening on the given path.
    serve :: ServerSocketPath,

    instrument :: FeatureInstrument
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions {
    workerMode = WorkerOneshotMode,
    serve = ServerSocketPath "" "" "",
    instrument = FeatureInstrument False
  }

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--make" : rest -> spin z {workerMode = WorkerMakeMode} rest
      "--serve" : socket : rest -> spin z {serve = serverSocketFromPath socket} rest
      "--instrument" : rest -> spin z {instrument = FeatureInstrument True} rest
      arg -> throwIO (userError ("Invalid worker CLI args: " ++ unwords arg))

-- | Allocate a communication channel for instrumentation events and construct a gRPC server handler that streams said
-- events to a client.
--
-- Returns the channel so that a GHC server can use it to send events.
createInstrumentMethods ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  IO (Chan (Proto Instr.Event), Methods IO (ProtobufMethodsOf Instrument))
createInstrumentMethods stateVar recompile = do
  instrChan <- newChan
  pure (instrChan, instrumentMethods instrChan stateVar recompile)

-- | Construct a gRPC server handler for the main part of the persistent worker.
createGhcMethods ::
  TVar LockState ->
  MVar WorkerState ->
  WorkerMode ->
  FeatureInstrument ->
  MVar WorkerStatus ->
  Maybe TraceId ->
  Maybe (Chan (Proto Instr.Event)) ->
  IO (CommandEnv -> RequestArgs -> IO (), Methods IO (ProtobufMethodsOf Worker))
createGhcMethods lock state workerMode instrument status traceId instrChan =
  let handler = toGrpcHandler (ghcHandler lock state workerMode instrument traceId) status state instrChan
      voidRun commandEnv requestArgs =
        void $ handler.run commandEnv requestArgs
  in pure (voidRun, fromGrpcHandler handler)

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: CliOptions -> IO ()
runWorker CliOptions {workerMode, serve, instrument} = do
  state <-
    case workerMode of
      WorkerMakeMode ->
        newStateWith OneshotCacheFeatures {
          loader = False,
          enable = True,
          names = False,
          finder = False,
          eps = False
        }
      WorkerOneshotMode -> newState True
  lock <- newTVarIO LockStart
  status <- newMVar WorkerStatus {active = 0}
  let
    methods = CreateMethods {
      createInstrumentation = createInstrumentMethods state,
      createGhc = createGhcMethods lock state workerMode instrument status traceId
    }
  runCentralGhcSpawned methods instrument serve
  where
    traceId = if null serve.traceId then Nothing else Just (TraceId serve.traceId)
