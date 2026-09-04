module Ghc.Ui.Run where

import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (customMainWithDefaultVty)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (filterM, forever, void, when)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Ghc.Ui.App (app)
import Ghc.Ui.Event.Main (MainEvent (..), initialState)
import Ghc.Ui.Session qualified as Session
import Ghc.Ui.SessionSelector qualified as SessionSelector
import Ghc.Ui.Types (WorkerId (WorkerId))
import Graphics.Vty (Vty (shutdown))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import Proto.Instrument (Instrument)
import System.Directory (createDirectoryIfMissing, doesPathExist, getModificationTime, listDirectory)
import System.Environment (lookupEnv)
import qualified System.FSNotify as FSNotify
import System.FSNotify (EventIsDirectory (..), watchDir, withManager)
import System.FilePath ((</>))

newtype WorkerPath =
  WorkerPath { path :: FilePath }
  deriving stock (Eq, Show)

envWorkerPath :: IO WorkerPath
envWorkerPath = WorkerPath . (++ "/") . fromMaybe "/tmp/ghc-persistent-worker" <$> lookupEnv "WORKER_PATH"

listen :: BChan MainEvent -> FilePath -> IO ()
listen eventChan instrPath = do
 void $ forkIO $ go 5
 where
  -- TODO: This is a hack, ids should be sent over grpc
  (sessionId', workerId') = break (== '_') instrPath
  sessionId = Session.Id $ Text.pack sessionId'
  workerId = WorkerId $ Text.pack workerId'
  go :: Int -> IO ()
  go 0 = writeBChan eventChan $ SessionSelectorEvent $ SessionSelector.RemoveWorker sessionId workerId
  go n =
    catch @SomeException
      ( withConnection def (ServerUnix instrPath) $ \conn -> do
          serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
            time <- getModificationTime instrPath
            writeBChan eventChan $ SessionSelectorEvent $ SessionSelector.AddWorker sessionId workerId time conn
            writeBChan eventChan (SendOptions (Just workerId))
            whileNext_ recv
              $ writeBChan eventChan
              . SessionSelectorEvent
              . SessionSelector.SessionEvent sessionId
              . Session.InstrEvent workerId
              . decode
              . fromStrict
              . (.encoded)
      )
      (const $ threadDelay 100_000 >> go (n - 1))

main :: IO ()
main = do
  workers <- envWorkerPath
  workerPathExists <- doesPathExist workers.path
  eventChan <- newBChan 10

  -- Update time every 100ms
  _ <- forkIO $ forever $ do
    time <- getCurrentTime
    writeBChan eventChan (SetTime time)
    threadDelay 100_000

  -- Find already running workers
  when workerPathExists do
    primaryDirs <- do
      dirs <- listDirectory workers.path
      filterM (\dir -> doesPathExist (workers.path ++ dir ++ "/instrument")) dirs
    mapM_ (listen eventChan . (++ "/instrument") . (workers.path ++)) primaryDirs

  void $ try @IOException do
    createDirectoryIfMissing True workers.path

  -- Detect new workers
  withManager $ \mgr -> do
    void $ watchDir mgr workers.path (const True) $ \case
      FSNotify.Added dir _ IsDirectory | not ("/log" `isInfixOf` dir) -> do
        listen eventChan $ dir </> "instrument"
      _ -> pure ()

    (_, vty) <- customMainWithDefaultVty (Just eventChan) app initialState
    vty.shutdown
