module Ghc.Ui.Grpc where

import Brick.BChan (BChan, writeBChan)
import BuckWorkerProto (Worker)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (toStrict)
import qualified Data.Text as Text
import Ghc.Ui.Data.Main (MainEvent (..))
import Ghc.Ui.Data.ServerApi (ServerApi (..))
import qualified Ghc.Ui.Data.Session as Session
import qualified Ghc.Ui.Data.Sessions as Sessions
import Ghc.Ui.Data.WorkerId (WorkerId)
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Worker_Fields qualified as Fields
import Types.Api (ApiRequest (..), ApiResponse (..), Target, TaskKind (..), TaskTrigger (..))

-- | Send a request to the worker's @Api@ method and decode the response.
-- A failed call, or a response that does not decode, is returned as 'ApiFailure'.
apiRequest :: Connection -> ApiRequest -> IO ApiResponse
apiRequest conn request = do
  result <- try @SomeException $
    nonStreaming conn (rpc @(Protobuf Worker "api")) $
      defMessage & Fields.payload .~ toStrict (encode request)
  pure case result of
    Left err -> ApiFailure ("api: " <> Text.pack (show err))
    Right response ->
      either (ApiFailure . ("api: failed to decode response: " <>) . Text.pack) id $
        eitherDecodeStrict response.payload

triggerRebuild :: BChan MainEvent -> Session.Id -> WorkerId -> Connection -> Target -> IO ()
triggerRebuild chan sid wid conn target =
  void $ forkIO do
    apiRequest conn (TriggerTask TaskTrigger {target, task = Build {rebuild = True}}) >>= \case
      ApiSuccess -> pure ()
      ApiFailure {message} ->
        writeBChan chan $ SessionSelectorEvent $ Sessions.Session sid $ Session.RebuildFailed wid target message

serverApi :: BChan MainEvent -> ServerApi
serverApi chan =
  ServerApi {triggerRebuild = triggerRebuild chan}
