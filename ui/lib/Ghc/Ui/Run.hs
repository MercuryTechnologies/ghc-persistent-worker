module Ghc.Ui.Run where

import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (customMainWithDefaultVty)
import BuckWorkerProto (Worker)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (filterM, forever, void, when)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import Ghc.Ui.App (app)
import Ghc.Ui.Data.Main (MainEvent (..))
import qualified Ghc.Ui.Data.Session as Session
import qualified Ghc.Ui.Data.Sessions as Sessions
import Ghc.Ui.Data.WorkerId (WorkerId (WorkerId))
import Ghc.Ui.Event.Main (initialState)
import Ghc.Ui.Grpc (serverApi)
import Graphics.Vty (Vty (shutdown))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
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

-- | The socket of the GHC server whose files are in the directory.
-- A server that workers forward their requests to writes its socket path to @primary@.
-- A server spawned by @buck-proxy@ listens on @server@ instead.
workerSocket :: FilePath -> IO FilePath
workerSocket dir =
  try @IOException (Text.readFile (dir </> "primary")) >>= \case
    Right primary | not (Text.null primary) -> pure (Text.unpack primary)
    _ -> pure (dir </> "server")

-- | Whether the directory belongs to a GHC server.
isWorkerDir :: FilePath -> IO Bool
isWorkerDir dir =
  (||) <$> doesPathExist (dir </> "primary") <*> doesPathExist (dir </> "server")

listen :: BChan MainEvent -> FilePath -> IO ()
listen eventChan dir = do
 void $ forkIO $ go 5
 where
  -- TODO: This is a hack, ids should be sent over grpc
  (sessionId', workerId') = break (== '_') dir
  sessionId = Session.Id $ Text.pack sessionId'
  workerId = WorkerId $ Text.pack workerId'
  removeWorker = writeBChan eventChan $ SessionSelectorEvent $ Sessions.RemoveWorker sessionId workerId
  go :: Int -> IO ()
  go 0 = removeWorker
  go n =
    catch @SomeException
      ( do
          socket <- workerSocket dir
          withConnection def (ServerUnix socket) $ \conn -> do
            serverStreaming conn (rpc @(Protobuf Worker "events")) defMessage $ \recv -> do
              time <- getModificationTime socket
              writeBChan eventChan $ SessionSelectorEvent $ Sessions.AddWorker sessionId workerId time conn
              whileNext_ recv
                $ writeBChan eventChan
                . SessionSelectorEvent
                . Sessions.Session sessionId
                . Session.InstrEvent workerId
                . decode
                . fromStrict
                . (.payload)
          -- The stream ends right away if the worker runs without instrumentation.
          removeWorker
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
      filterM (isWorkerDir . (workers.path ++)) dirs
    mapM_ (listen eventChan . (workers.path ++)) primaryDirs

  void $ try @IOException do
    createDirectoryIfMissing True workers.path

  -- Detect new workers
  withManager $ \mgr -> do
    void $ watchDir mgr workers.path (const True) $ \case
      FSNotify.Added dir _ IsDirectory | not ("/log" `isInfixOf` dir) -> do
        listen eventChan dir
      _ -> pure ()

    (_, vty) <- customMainWithDefaultVty (Just eventChan) (app (serverApi eventChan)) initialState
    vty.shutdown
