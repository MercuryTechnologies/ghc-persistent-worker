{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Int (Int32)
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runClient "/tmp/mytest.ipc" $ \s -> do
    let xs :: [String]
        xs = ["a", "b", "abc", "def"]
        msg = wrapMsg xs
    sendMsg s msg
    --
    msg' <- recvMsg s
    let n :: Int = unwrapMsg msg'
    print n

runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient fp client = withSocketsDo $ E.bracket (open fp) close client
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        connect sock (SockAddrUnix fp)
        return sock
