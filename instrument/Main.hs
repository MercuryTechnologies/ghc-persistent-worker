module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import BuckWorker (Instrument)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, void, when, forever)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Graphics.Vty (Vty (shutdown))
import Internal.Cache (Options (..))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming, serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Fields
import System.Directory (doesPathExist, getModificationTime, listDirectory)
import System.Environment (lookupEnv)
import System.FSNotify (Event(..), EventIsDirectory(..), watchDir, withManager)
import UI qualified
import UI.SessionSelector qualified as SS
import UI.Session qualified as Session

newtype WorkerPath
  = WorkerPath {path :: FilePath}
  deriving stock (Eq, Show)

envWorkerPath :: IO WorkerPath
envWorkerPath = WorkerPath . (++ "/") . fromMaybe "/tmp/ghc-persistent-worker" <$> lookupEnv "WORKER_PATH"

listen :: BChan UI.Event -> FilePath -> IO ()
listen eventChan instrPath = do
  void $ forkIO $ go 5
 where
  -- TODO: This is a hack, ids should be sent over grpc
  (sessionId', workerId') = break (== '_') instrPath
  sessionId = Session.Id $ Text.pack sessionId'
  workerId = Session.WorkerId $ Text.pack workerId'
  go :: Int -> IO ()
  go 0 = writeBChan eventChan $ UI.SessionSelectorEvent $ SS.RemoveWorker sessionId workerId
  go n =
    catch @SomeException
      ( withConnection def (ServerUnix instrPath) $ \conn -> do
          let sendOptions options =
                void $
                  nonStreaming conn (rpc @(Protobuf Instrument "setOptions")) $
                    mkOptions options
          serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
            time <- getModificationTime instrPath
            writeBChan eventChan $ UI.SessionSelectorEvent $ SS.AddWorker sessionId workerId time sendOptions
            writeBChan eventChan (UI.SendOptions (Just workerId))
            whileNext_ recv $ writeBChan eventChan . UI.SessionSelectorEvent . SS.SessionEvent sessionId . Session.InstrEvent workerId
      )
      (const $ threadDelay 100_000 >> go (n - 1))
  mkOptions :: Options -> Proto Instr.Options
  mkOptions Options{..} =
    defMessage
      & Fields.extraGhcOptions
      .~ Text.pack extraGhcOptions

main :: IO ()
main = do
  workers <- envWorkerPath
  workerPathExists <- doesPathExist workers.path
  eventChan <- newBChan 10

  -- Update time every 100ms
  _ <- forkIO $ forever $ do
    time <- getCurrentTime
    writeBChan eventChan (UI.SetTime time)
    threadDelay 100_000

  -- Find already running workers
  when workerPathExists do
    primaryDirs <- do
      dirs <- listDirectory workers.path
      filterM (\dir -> doesPathExist (workers.path ++ dir ++ "/instrument")) dirs
    mapM_ (listen eventChan . (++ "/instrument") . (workers.path ++)) primaryDirs

  -- Detect new workers
  withManager $ \mgr -> do
    void $ watchDir mgr workers.path (const True) $ \case
      Added dir _ IsDirectory | not ("/log" `isInfixOf` dir) -> do
        listen eventChan $ dir ++ "instrument"
      _ -> pure ()

    (_, vty) <- UI.customMainWithDefaultVty (Just eventChan) UI.app UI.initialState
    vty.shutdown
