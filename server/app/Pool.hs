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
  { poolStatus :: IntMap (Bool, Maybe Id),
    poolHandles :: [(Int, HandleSet)]
  }

dumpStatus :: TVar Pool -> IO ()
dumpStatus ref = do
  pool <- atomically (readTVar ref)
  mapM_ print $ IM.toAscList (poolStatus pool)

getAssignableWorker :: IntMap (Bool, Maybe Id) -> Id -> Maybe (Int, (Bool, Maybe Id))
getAssignableWorker workers id' = List.find (isAssignable . snd) . IM.toAscList $ workers
  where
    isAssignable (b, mid)
      | not b = case mid of
                  Nothing -> True
                  Just id'' -> id' == id''
      | otherwise = False

assignJob :: TVar Pool -> Id -> STM (Int, HandleSet)
assignJob ref id' = do
  pool <- readTVar ref
  let workers = poolStatus pool
  let m = getAssignableWorker workers id'
  case m of
    Nothing -> retry
    Just (i, _) -> do
      let !workers' = IM.update (\_ -> Just (True, Just id')) i workers
          Just hset = List.lookup i (poolHandles pool)
      writeTVar ref (pool {poolStatus = workers'})
      pure (i, hset)

finishJob :: TVar Pool -> Int -> STM ()
finishJob ref i = do
  pool <- readTVar ref
  let workers = poolStatus pool
      !workers' = IM.update (\(_, m)  -> Just (False, m)) i workers
  writeTVar ref (pool {poolStatus = workers'})
