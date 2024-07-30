{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Data.Binary (decode)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as List
import Message (Msg (..), recvMsg, sendMsg, unwrapMsg, wrapMsg)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

newtype Pool = Pool
  { poolWorkers :: IntMap Bool
  }

runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer fp server = withSocketsDo $ E.bracket (open fp) close loop
  where
    open fp = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
        bind sock (SockAddrUnix fp)
        listen sock 5
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)

assignJob :: TVar Pool -> STM Int
assignJob ref = do
  workers <- poolWorkers <$> readTVar ref
  let m = List.find ((== False) . snd) (IM.toAscList workers)
  case m of
    Nothing -> retry
    Just (i, _) -> do
      let !workers' = IM.update (\_ -> Just True) i workers
      writeTVar ref (Pool workers')
      pure i

finishJob :: TVar Pool -> Int -> STM ()
finishJob ref i = do
  workers <- poolWorkers <$> readTVar ref
  let !workers' = IM.update (\_ -> Just False) i workers
  writeTVar ref (Pool workers')


work :: TVar Pool -> Socket -> IO ()
work ref s = do
    msg <- recvMsg s
    i <- atomically $ assignJob ref
    let xs :: [String] = unwrapMsg msg
    putStrLn $ "worker = " ++ show i ++ ": " ++ show xs
    let n' = length xs
        msg' = wrapMsg n'
    -- simulate a worker
    threadDelay 5_000_000
    atomically $ finishJob ref i
    sendMsg s msg'
    work ref s


main :: IO ()
main = do
  let thePool = Pool (IM.fromList [(1, False), (2, False), (3, False), (4, False)])
  ref <- newTVarIO thePool
  runServer "/tmp/mytest.ipc" (work ref)
