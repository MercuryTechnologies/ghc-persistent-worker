module GhcWorker.Grpc where

import Common.Grpc ()
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Internal.State (WorkerState (..), Options (..))
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (
  Methods (..),
  mkNonStreaming,
  mkServerStreaming,
  simpleMethods,
  )
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr

-- | Fetch statistics about the current state of the RTS for instrumentation.
mkStats :: WorkerState -> IO (Proto Instr.Stats)
mkStats _ = do
  s <- getRTSStats
  pure $
    defMessage
      & Instr.memory .~ Map.fromList
          [ ("Total", fromIntegral s.gc.gcdetails_mem_in_use_bytes)
          ]
      & Instr.gcCpuNs .~ s.gc_cpu_ns
      & Instr.cpuNs .~ s.cpu_ns

-- | Implementation of a streaming grapesy handler that sends instrumentation statistics pulled from the provided
-- channel to the client.
notifyMe ::
  MVar WorkerState ->
  Chan (Proto Instr.Event) ->
  (NextElem (Proto Instr.Event) -> IO ()) ->
  IO ()
notifyMe stateVar chan callback = do
  state <- readMVar stateVar
  myChan <- dupChan chan
  stats <- mkStats state
  callback $ NextElem $
    defMessage
      & Instr.stats .~ stats
  forever $ do
    msg <- readChan myChan
    callback $ NextElem msg

-- | Set the options for the server.
setOptions ::
  MVar WorkerState ->
  Proto  Instr.Options ->
  IO (Proto Instr.Empty)
setOptions stateVar opts = do
  modifyMVar_ stateVar $ \state ->
    pure state {
      options = Options {
        extraGhcOptions = Text.unpack opts.extraGhcOptions
      }
    }
  pure defMessage

-- | A grapesy server that streams instrumentation data from the provided channel.
instrumentMethods ::
  Chan (Proto Instr.Event) ->
  MVar WorkerState ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar =
  simpleMethods
    (mkServerStreaming (const (notifyMe stateVar chan)))
    (mkNonStreaming (setOptions stateVar))
