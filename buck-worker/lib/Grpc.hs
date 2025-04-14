module Grpc where

import BuckWorker (ExecuteCommand, ExecuteResponse)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Exception (SomeException (..), try)
import Control.Monad (forever, when)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (
  Methods (..),
  mkClientStreaming,
  mkNonStreaming,
  mkServerStreaming,
  simpleMethods,
  )
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr
import Proto.Worker (ExecuteCommand'EnvironmentEntry, ExecuteEvent, Worker (..))
import Proto.Worker_Fields qualified as Fields
import System.IO (hPutStrLn, stderr)

debugRequestArgs :: Bool
debugRequestArgs = False

-- | The environment variables sent by Buck.
newtype CommandEnv =
  CommandEnv (Map String String)
  deriving stock (Eq, Show)

-- | The command line arguments sent by Buck.
newtype RequestArgs =
  RequestArgs [String]
  deriving stock (Eq, Show)

-- | A handler for gRPC requests takes a 'Map' of environment variables and a list of command line arguments and returns
-- a list of output message lines and an exit code.
newtype GrpcHandler =
  GrpcHandler {
    run ::
      CommandEnv ->
      RequestArgs ->
      IO ([String], Int32)
  }

commandEnv :: [Proto ExecuteCommand'EnvironmentEntry] -> CommandEnv
commandEnv =
  CommandEnv .
  Map.fromList .
  fmap \kv -> (fromBs kv.key, fromBs kv.value)
  where
    fromBs = Text.unpack . decodeUtf8Lenient

-- | Generic wrapper for a handler of the 'Worker' message 'ExecuteCommand', taking care of input data conversions and
-- response construction.
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

-- | Fetch statistics about the current state of the RTS for instrumentation.
mkStats :: IO (Proto Instr.Stats)
mkStats = do
  s <- getRTSStats
  pure $
    defMessage
      & Instr.memory .~ fromIntegral s.gc.gcdetails_mem_in_use_bytes
      & Instr.gcCpuNs .~ s.gc_cpu_ns
      & Instr.cpuNs .~ s.cpu_ns

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
