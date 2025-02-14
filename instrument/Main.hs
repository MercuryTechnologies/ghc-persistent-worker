{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad (forM_)
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (log)

import BuckWorker (Worker (..))
import Data.Text qualified as Text
import Network.GRPC.Client (Address (Address), Server (ServerInsecure), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~), (^.))
import Proto.Worker_Fields qualified as Fields

main :: IO ()
main = do
  port <- read <$> getEnv "WORKER_PORT"
  hPutStrLn stderr $ "using worker port: " <> show port
  let server = ServerInsecure (Address "localhost" port Nothing)
      buckArgs = ["-fprefer-byte-code", "-B/Users/sjoerdvisscher/.ghcup/ghc/9.10.1/lib/ghc-9.10.1/lib"]
  withConnection def server $ \connection -> do
    forM_ @[] ["test/A.hs", "test/B.hs", "test/C.hs"] $ \file -> do
      withRPC connection def (Proxy @(Protobuf Worker "execute")) \call -> do
        sendFinalInput call (defMessage & Fields.argv .~ (file : buckArgs))
        resp <- recvNextOutput call
        putStr $ Text.unpack $ resp ^. Fields.stderr
        pure ()
