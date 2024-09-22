{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Int (Int32)
import Message
  ( Msg (..),
    Request (..),
    Response (..),
    recvMsg,
    sendMsg,
    unwrapMsg,
    wrapMsg
  )
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
import System.Environment (getArgs, getEnvironment)
import System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = runClient "/tmp/mytest.ipc" $ \s -> do
    args <- getArgs
    env <- getEnvironment
    let req = Request
          { requestEnv = env,
            requestArgs = args
          }
    let msg = wrapMsg req
    sendMsg s msg
    --
    msg' <- recvMsg s
    let Response res ss_out ss_err = unwrapMsg msg'
    mapM_ (hPutStrLn stdout) ss_out
    mapM_ (hPutStrLn stderr) ss_err

runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient fp client = withSocketsDo $ E.bracket (open fp) close client
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        connect sock (SockAddrUnix fp)
        return sock
