{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Int (Int32)
import Data.List (lookup)
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
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  args <- getArgs
  env <- getEnvironment
  let mSocketPath = lookup "GHC_PERSISTENT_WORKER_SOCKET" env
  case mSocketPath of
    Nothing -> do
      hPutStrLn stderr "ghc-persistent-worker-client: Please set GHC_PERSISTENT_WORKER_SOCKET env variable with the socket file path."
      exitFailure
    Just sockPath -> process sockPath env args

process :: FilePath -> [(String, String)] -> [String] -> IO ()
process socketPath env args = runClient socketPath $ \s -> do
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
  hFlush stdout
  mapM_ (hPutStrLn stderr) ss_err
  hFlush stderr

runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient fp client = withSocketsDo $ E.bracket (open fp) close client
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        connect sock (SockAddrUnix fp)
        return sock
