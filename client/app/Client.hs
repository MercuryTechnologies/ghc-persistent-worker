{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Int (Int32)
import Message (ConsoleOutput (..), Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)

main :: IO ()
main = runClient "/tmp/mytest.ipc" $ \s -> do
    args <- getArgs
    let msg = wrapMsg args
    sendMsg s msg
    --
    msg' <- recvMsg s
    let ConsoleOutput ss = unwrapMsg msg'
    mapM_ putStrLn ss

runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient fp client = withSocketsDo $ E.bracket (open fp) close client
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        connect sock (SockAddrUnix fp)
        return sock
