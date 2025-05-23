module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import BuckWorker (Instrument)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, void, when, forever)
import Data.List (dropWhileEnd, isSuffixOf)
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
import System.FSNotify (Event(..), watchTree, withManager)
import UI qualified

newtype WorkerPath
  = WorkerPath {path :: FilePath}
  deriving stock (Eq, Show)

envWorkerPath :: IO WorkerPath
envWorkerPath = WorkerPath . (++ "/") . fromMaybe "/tmp/ghc-persistent-worker" <$> lookupEnv "WORKER_PATH"

listen :: BChan UI.Event -> FilePath -> IO ()
listen eventChan instrPath = do
  void $ forkIO $ go 5
 where
  go :: Int -> IO ()
  go 0 = getCurrentTime >>= writeBChan eventChan . UI.SessionEvent instrPath . UI.EndSession
  go n =
    catch @SomeException
      ( withConnection def (ServerUnix instrPath) $ \conn -> do
          let sendOptions options =
                void $
                  nonStreaming conn (rpc @(Protobuf Instrument "setOptions")) $
                    mkOptions options
          serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
            time <- getModificationTime instrPath
            writeBChan eventChan $ UI.SessionEvent instrPath $ UI.StartSession time sendOptions
            whileNext_ recv $ writeBChan eventChan . UI.SessionEvent instrPath . UI.InstrEvent
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
      filterM (\dir -> doesPathExist (workers.path ++ dir ++ "/primary")) dirs
    mapM_ (listen eventChan . (++ "/instrument") . (workers.path ++)) primaryDirs

  -- Detect new workers
  withManager $ \mgr -> do
    void $ watchTree mgr workers.path (const True) $ \case
      Added file _ _ | "/primary" `isSuffixOf` file -> do
        listen eventChan $ dropWhileEnd (/= '/') file ++ "instrument"
      Modified file _ _ | "/primary" `isSuffixOf` file -> do
        listen eventChan $ dropWhileEnd (/= '/') file ++ "instrument"
      _ -> pure ()

    (_, vty) <- UI.customMainWithDefaultVty (Just eventChan) UI.app UI.initialState
    vty.shutdown
