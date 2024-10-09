{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Foldable (foldMap)
import Data.Int (Int32)
import Data.List (isPrefixOf, lookup, partition, stripPrefix)
import Data.Monoid (First (..))
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

splitArgs :: [String] -> ([String], [String])
splitArgs = partition ("--worker-" `isPrefixOf`)

main :: IO ()
main = do
  args <- getArgs
  let (workerArgs, ghcArgs) = splitArgs args
      mSocketPath = getFirst $ foldMap (First . stripPrefix "--worker-socket=") workerArgs
  case mSocketPath of
    Nothing -> do
      hPutStrLn stderr "ghc-persistent-worker-client: Please set GHC_PERSISTENT_WORKER_SOCKET env variable with the socket file path."
      exitFailure
    Just sockPath -> do
      env <- getEnvironment
      process sockPath env ghcArgs

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
