{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import BuckWorker (Worker (..))
import Control.Monad (forM_)
import Data.Text qualified as Text
import Network.GRPC.Client (withConnection, Server(ServerUnix), rpc)
import Network.GRPC.Common (def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~), (^.))
import Proto.Worker_Fields qualified as Fields
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (log)
import Network.GRPC.Client.StreamType.IO (nonStreaming)

-- | The file system path of the socket on which the worker running in this process is supposed to listen.
newtype ServerSocketPath
  = ServerSocketPath {path :: FilePath}
  deriving stock (Eq, Show)

envServerSocket :: IO ServerSocketPath
envServerSocket = ServerSocketPath <$> getEnv "WORKER_SOCKET"

main :: IO ()
main = do
  serverSocket <- envServerSocket
  hPutStrLn stderr $ "using worker socket: " <> show serverSocket
  let server = ServerUnix serverSocket.path
      buckArgs = ["-fprefer-byte-code", "-B/Users/sjoerdvisscher/.ghcup/ghc/9.10.1/lib/ghc-9.10.1/lib"]
  withConnection def server $ \conn -> do
    forM_ @[] ["test/A.hs", "test/B.hs", "test/C.hs"] $ \file -> do
      print file
      let req = defMessage & Fields.argv .~ (file : buckArgs)
      resp <- nonStreaming conn (rpc @(Protobuf Worker "execute")) req
      putStrLn $ Text.unpack $ resp ^. Fields.stderr
