module GhcWorker.Grpc where

import BuckWorkerProto ()
import Common.Grpc (GrpcHandler, execute, workerMethods)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad (forever)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Binary qualified as Binary
import Data.ByteString (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text (pack)
import Data.Text qualified as Text
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..), mkNonStreaming, mkServerStreaming, simpleMethods)
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr
import Proto.Worker (Encoded, Worker)
import Proto.Worker_Fields qualified as Fields
import Types.Api (ApiRequest (..), ApiResponse (..), Event (..))
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.State (WorkerState (..))
import Types.Target (TargetSpec (..))

-- | Fetch statistics about the current state of the RTS for instrumentation.
mkStats :: WorkerState -> IO Event
mkStats _ = do
  s <- getRTSStats
  pure $
    Stats
      { memory = Map.fromList
          [ ("Total", fromIntegral s.gc.gcdetails_mem_in_use_bytes)
          ]
      , gcCpuNs = fromIntegral s.gc_cpu_ns
      , cpuNs = fromIntegral s.cpu_ns
      }

-- | Implementation of a streaming grapesy handler that sends instrumentation statistics pulled from the provided
-- channel to the client.
notifyMe ::
  MVar WorkerState ->
  Chan Event ->
  (NextElem (Proto Instr.Event) -> IO ()) ->
  IO ()
notifyMe stateVar chan callback = do
  state <- readMVar stateVar
  myChan <- dupChan chan
  stats <- mkStats state
  callback $ NextElem $
    defMessage
      & Instr.encoded .~ toStrict (Binary.encode stats)
  forever $ do
    msg <- readChan myChan
    callback $ NextElem $
      defMessage
        & Instr.encoded .~ toStrict (Binary.encode msg)

-- | Trigger a rebuild for the given target.
triggerRebuild ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerRebuild stateVar recompile target = do
  state <- readMVar stateVar
  let margs = Map.lookup (TargetUnknown (Text.unpack target.target)) state.targetArgs
  for_ margs (uncurry recompile)
  pure defMessage

-- | A grapesy server that streams instrumentation data from the provided channel.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar recompile =
  simpleMethods
    (mkServerStreaming (const (notifyMe stateVar chan)))
    (mkNonStreaming (triggerRebuild stateVar recompile))

-- | Implementation of the streaming @Events@ method, forwarding the 'Chan' to each consumer.
events ::
  MVar WorkerState ->
  Maybe (Chan Event) ->
  (NextElem (Proto Encoded) -> IO ()) ->
  IO ()
events _ Nothing callback = callback NoNextElem
events stateVar (Just chan) callback = do
  state <- readMVar stateVar
  myChan <- dupChan chan
  stats <- mkStats state
  callback $ NextElem $
    defMessage
      & Fields.payload .~ toStrict (Binary.encode stats)
  forever $ do
    msg <- readChan myChan
    callback $ NextElem $
      defMessage
        & Fields.payload .~ toStrict (Binary.encode msg)

-- | Dispatch a single decoded 'ApiRequest' to the appropriate handler.
-- The worker doesn't support any requests yet.
apiRequest :: ApiRequest -> IO ApiResponse
apiRequest = \case
  TriggerTask _ ->
    pure (ApiFailure "Cannot trigger tasks")
  EvictBytecode _ ->
    pure (ApiFailure "Evicting bytecode not supported")
  Clean _ ->
    pure (ApiFailure "Cleaning not supported")

-- | Implementation of the @Api@ method.
api ::
  (ApiRequest -> IO ApiResponse) ->
  Proto Encoded ->
  IO (Proto Encoded)
api run req = do
  resp <- case eitherDecodeStrict req.payload of
    Left err -> do
      pure (encode (ApiFailure ("api: failed to decode payload: " <> pack err)))
    Right cmd -> encode <$> run cmd
  pure (defMessage & Fields.payload .~ LBS.toStrict resp)

-- | Server methods for the @worker@ proto.
ghcMethods ::
  MVar WorkerState ->
  Maybe (Chan Event) ->
  GrpcHandler ->
  Methods IO (ProtobufMethodsOf Worker)
ghcMethods stateVar chan handler =
  workerMethods (api apiRequest) (events stateVar chan) (execute handler)
