module Run where

import BuckWorker (Instrument, Worker)
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Control.Exception (throwIO)
import Control.Monad (foldM)
import GhcHandler (WorkerMode (..), ghcHandler)
import Grpc (ghcServerMethods, instrumentMethods)
import Instrumentation (WorkerStatus (..), toGrpcHandler)
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCache, emptyCacheWith)
import Network.GRPC.Common.Protobuf (Proto)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import Orchestration (CreateMethods (..), ServerSocketPath, runLocalGhc, runOrProxyCentralGhc)
import qualified Proto.Instrument as Instr

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | Should only a single central GHC server be run, with all other worker processes proxying it?
    single :: Bool,

    -- | The worker implementation: Make mode or oneshot mode.
    workerMode :: WorkerMode
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {single = False, workerMode = WorkerOneshotMode}

parseOptions :: [String] -> IO CliOptions
parseOptions =
  flip foldM defaultCliOptions \ z -> \case
    "--single" -> pure z {single = True}
    "--make" -> pure z {workerMode = WorkerMakeMode}
    arg -> throwIO (userError ("Invalid worker CLI arg: " ++ arg))

-- | Allocate a communication channel for instrumentation events and construct a gRPC server handler that streams said
-- events to a client.
--
-- Returns the channel so that a GHC server can use it to send events.
createInstrumentMethods :: IO (Chan (Proto Instr.Event), Methods IO (ProtobufMethodsOf Instrument))
createInstrumentMethods = do
  instrChan <- newChan
  pure (instrChan, instrumentMethods instrChan)

-- | Construct a gRPC server handler for the main part of the persistent worker.
createGhcMethods ::
  MVar Cache ->
  WorkerMode ->
  MVar WorkerStatus ->
  Maybe (Chan (Proto Instr.Event)) ->
  IO (Methods IO (ProtobufMethodsOf Worker))
createGhcMethods cache workerMode status instrChan =
  pure (ghcServerMethods (toGrpcHandler (ghcHandler cache workerMode) status instrChan))

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: ServerSocketPath -> CliOptions -> IO ()
runWorker socket CliOptions {single, workerMode} = do
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
      createInstrumentation = createInstrumentMethods,
      createGhc = createGhcMethods cache workerMode status
    }
  if single
  then runOrProxyCentralGhc methods socket
  else runLocalGhc methods socket Nothing
