module Grpc where

import BuckWorkerProto (Instrument)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Text qualified as Text
import Internal.State (Options (..))
import Network.GRPC.Client (rpc, Connection)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Fields
import Types.State (Target (..))

sendOptions :: Connection -> Options -> IO ()
sendOptions conn options =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "setOptions")) $
      mkOptions options

mkOptions :: Options -> Proto Instr.Options
mkOptions Options{..} =
  defMessage
    & Fields.extraGhcOptions
    .~ Text.pack extraGhcOptions

triggerRebuild :: Connection -> Target -> IO ()
triggerRebuild conn (Target target) =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "triggerRebuild")) $
      defMessage
        & Fields.target
        .~ Text.pack target