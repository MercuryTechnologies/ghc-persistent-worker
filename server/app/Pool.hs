{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Pool
( HandleSet (..),
  Pool (..),
  dumpStatus,
  assignJob,
  finishJob,
) where

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, retry, writeTVar)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as List
import Message (Id)
import System.IO (Handle)

data HandleSet = HandleSet
  { handleArgIn :: Handle,
    handleMsgOut :: Handle
  }

data Pool = Pool
  { poolStatus :: IntMap (Maybe Id),
    poolHandles :: [(Int, HandleSet)]
  }

dumpStatus :: TVar Pool -> IO ()
dumpStatus ref = do
  pool <- atomically (readTVar ref)
  print $ poolStatus pool

assignJob :: TVar Pool -> Id -> STM (Int, HandleSet)
assignJob ref id' = do
  pool <- readTVar ref
  let workers = poolStatus pool
  let m = List.find ((== Nothing) . snd) (IM.toAscList workers)
  case m of
    Nothing -> retry
    Just (i, _) -> do
      let !workers' = IM.update (\_ -> Just (Just id')) i workers
          Just hset = List.lookup i (poolHandles pool)
      writeTVar ref (pool {poolStatus = workers'})
      pure (i, hset)

finishJob :: TVar Pool -> Int -> STM ()
finishJob ref i = do
  pool <- readTVar ref
  let workers = poolStatus pool
      !workers' = IM.update (\_ -> Just Nothing) i workers
  writeTVar ref (pool {poolStatus = workers'})

