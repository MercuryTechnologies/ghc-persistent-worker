{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forM_, void)
import Data.String (fromString)
import Data.Text qualified as Text
import Graphics.Vty (Vty(shutdown))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~), (^.))
import Proto.Worker_Fields qualified as Fields
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (log)

import BuckWorker (Worker (..))

import UI

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
newtype ServerSocketPath
  = ServerSocketPath {path :: FilePath}
  deriving stock (Eq, Show)

envServerSocket :: IO ServerSocketPath
envServerSocket = ServerSocketPath <$> getEnv "WORKER_SOCKET"

main :: IO ()
main = do
  eventChan <- newBChan 10

  serverSocket <- envServerSocket
  hPutStrLn stderr $ "using worker socket: " <> show serverSocket
  let server = ServerUnix serverSocket.path
      buckArgs = ["-fprefer-byte-code", "-B/Users/sjoerdvisscher/.ghcup/ghc/9.10.1/lib/ghc-9.10.1/lib"]
  void $ forkIO $ withConnection def server $ \conn -> do
    forM_ @[] ["test/A.hs", "test/B.hs", "test/C.hs"] $ \file -> do
      writeBChan eventChan $ AddContent file
      let req = defMessage & Fields.argv .~ (fromString file : buckArgs)
      resp <- nonStreaming conn (rpc @(Protobuf Worker "execute")) req
      writeBChan eventChan $ AddContent $ Text.unpack $ resp ^. Fields.stderr

  (_, vty) <- customMainWithDefaultVty (Just eventChan) app initialState
  vty.shutdown
