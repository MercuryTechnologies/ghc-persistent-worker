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
import Orchestration (CreateMethods (..), ServerSocketPath, runCentralGhcForked, runLocalGhc, runOrProxyCentralGhc)
import qualified Proto.Instrument as Instr

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | Should only a single central GHC server be run, with all other worker processes proxying it?
    single :: Bool,

    -- | The worker implementation: Make mode or oneshot mode.
    workerMode :: WorkerMode,

    workerExe :: Maybe FilePath,

    serve :: Maybe FilePath

    -- TODO
    -- instrument :: Bool
  }
  deriving stock (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions {single = False, workerMode = WorkerOneshotMode, workerExe = Nothing, serve = Nothing}

parseOptions :: [String] -> IO CliOptions
parseOptions =
  spin defaultCliOptions
  where
    spin z = \case
      [] -> pure z
      "--single" : rest -> spin z {single = True} rest
      "--make" : rest -> spin z {workerMode = WorkerMakeMode} rest
      "--exe" : exe : rest -> spin z {workerExe = Just exe} rest
      "--serve" : socket : rest -> spin z {serve = Just socket} rest
      arg -> throwIO (userError ("Invalid worker CLI args: " ++ unwords arg))

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
runWorker socket CliOptions {single, workerMode, workerExe, serve} = do
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
    runSingle = do
      exe <- case workerExe of
        Just exe -> pure exe
        Nothing -> throwIO (userError "Single mode requires specifying the worker executable with '--exe'")
      runOrProxyCentralGhc exe socket
  case serve of
    Just serverSocket -> runCentralGhcForked methods serverSocket
    Nothing ->
      if single
      then runSingle
      else runLocalGhc methods socket Nothing
