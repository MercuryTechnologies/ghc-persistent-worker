module Ghc.Ui.Grpc where

import BuckWorkerProto (Instrument)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Ghc.Ui.Data.ServerApi (ServerApi (..))
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Instrument_Fields qualified as Fields
import Types.Target (TargetSpec, renderTargetSpec)

triggerRebuild :: Connection -> TargetSpec -> IO ()
triggerRebuild conn target =
  void $ forkIO $ void $
  nonStreaming conn (rpc @(Protobuf Instrument "triggerRebuild")) $
    defMessage
    & Fields.target
    .~ renderTargetSpec target

serverApi :: ServerApi
serverApi =
  ServerApi {triggerRebuild}
