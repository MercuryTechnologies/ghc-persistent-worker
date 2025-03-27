module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException)
import Control.Monad (void)
import Data.List (dropWhileEnd, isSuffixOf)
import Data.Time (getCurrentTime)
import Graphics.Vty (Vty (shutdown))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import System.FSNotify (Event (..), watchTree, withManager)

import BuckWorker (Instrument)

import UI

listen :: BChan (SessionId, CustomEvent) -> FilePath -> IO ()
listen eventChan instrPath = do
  void $ forkIO $ withConnection def (ServerUnix instrPath) $ \conn -> do
    time <- getCurrentTime
    writeBChan eventChan $ (instrPath, StartSession time)
    catch @SomeException
      (serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
        whileNext_ recv $ writeBChan eventChan . (,) instrPath . InstrEvent)
      (const $ getCurrentTime >>= writeBChan eventChan . (,) instrPath . EndSession)

main :: IO ()
main = do
  eventChan <- newBChan 10

  withManager $ \mgr -> do
    void $ watchTree mgr "/tmp/buck2_worker/" (const True) $ \case
      Modified file _ _ | "/primary" `isSuffixOf` file -> do
        listen eventChan $ dropWhileEnd (/= '/') file ++ "instrument"
      _ -> pure ()

    (_, vty) <- customMainWithDefaultVty (Just eventChan) app initialState
    vty.shutdown
