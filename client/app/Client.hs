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
  ( TargetId (..),
    Msg (..),
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

data WorkerConfig = WorkerConfig
  { workerConfigSocket :: String,
    workerConfigTargetId :: Maybe TargetId,
    workerConfigClose :: Bool
  }
  deriving (Show)

splitArgs :: [String] -> ([String], [String])
splitArgs = partition ("--worker-" `isPrefixOf`)

getWorkerConfig :: [String] -> Maybe WorkerConfig
getWorkerConfig args = do
  socket <- getFirst $ foldMap (First . stripPrefix "--worker-socket=") args
  let mid = getFirst $ foldMap (First . stripPrefix "--worker-target-id=") args
      willClose = any ("--worker-close" `isPrefixOf`) args
  pure WorkerConfig
    { workerConfigSocket = socket,
      workerConfigTargetId = TargetId <$> mid,
      workerConfigClose = willClose
    }

main :: IO ()
main = do
  args <- getArgs
  let (workerArgs, ghcArgs) = splitArgs args
      mConf = getWorkerConfig workerArgs
  hPutStrLn stderr (show mConf)
  hPutStrLn stderr (show args)
  hFlush stderr
  case mConf of
    Nothing -> do
      hPutStrLn stderr "ghc-persistent-worker-client: Please pass --worker-socket=(socket file path)."
      exitFailure
    Just conf -> do
      let sockPath = workerConfigSocket conf
          mid = workerConfigTargetId conf
          willClose = workerConfigClose conf
      env <- getEnvironment
      process sockPath mid willClose env ghcArgs

process :: FilePath -> Maybe TargetId -> Bool -> [(String, String)] -> [String] -> IO ()
process socketPath mid willClose env args = runClient socketPath $ \s -> do
  let req = Request
        { requestWorkerTargetId = mid,
          requestWorkerClose = willClose,
          requestEnv = env,
          requestArgs = args
        }
  let msg = wrapMsg req
  sendMsg s msg
  --
  msg' <- recvMsg s
  let Response jobid res ss_out ss_err = unwrapMsg msg'
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
