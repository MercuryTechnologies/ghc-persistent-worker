module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException)
import Control.Monad (void, filterM)
import Data.List (dropWhileEnd, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Graphics.Vty (Vty (shutdown))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import System.Directory (doesFileExist, listDirectory, getModificationTime)
import System.Environment (lookupEnv)
import System.FSNotify (Event (..), watchTree, withManager)

import BuckWorker (Instrument)

import UI

newtype WorkerPath =
  WorkerPath { path :: FilePath }
  deriving stock (Eq, Show)

envWorkerPath :: IO WorkerPath
envWorkerPath = WorkerPath . (++ "/") . fromMaybe "/tmp/buck2_worker" <$> lookupEnv "WORKER_PATH"

listen :: BChan (SessionId, CustomEvent) -> FilePath -> IO ()
listen eventChan instrPath = do
  void $ forkIO $ withConnection def (ServerUnix instrPath) $ \conn -> do
    catch @SomeException
      (serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
        time <- getModificationTime instrPath
        writeBChan eventChan $ (instrPath, StartSession time)
        whileNext_ recv $ writeBChan eventChan . (,) instrPath . InstrEvent)
      (const $ getCurrentTime >>= writeBChan eventChan . (,) instrPath . EndSession)

main :: IO ()
main = do
  workers <- envWorkerPath
  eventChan <- newBChan 10

  -- Find already running workers
  primaryDirs <- do
    dirs <- listDirectory workers.path
    filterM (\dir -> doesFileExist (workers.path ++ dir ++ "/primary")) dirs
  mapM_ (listen eventChan . (++ "/instrument") . (workers.path ++)) primaryDirs

  -- Detect new workers
  withManager $ \mgr -> do
    void $ watchTree mgr workers.path (const True) $ \case
      Modified file _ _ | "/primary" `isSuffixOf` file -> do
        listen eventChan $ dropWhileEnd (/= '/') file ++ "instrument"
      _ -> pure ()

    (_, vty) <- customMainWithDefaultVty (Just eventChan) app initialState
    vty.shutdown
