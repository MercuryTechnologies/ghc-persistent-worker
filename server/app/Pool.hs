{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Pool
( HandleSet (..),
  Pool (..),
  dumpStatus,
  assignJob,
  finishJob,
  removeWorker,
) where

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, retry, writeTVar)
import qualified Data.Foldable as F
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as List
import Message (Id)
import System.IO (Handle, hFlush, hPrint, stdout)

data HandleSet = HandleSet
  { handleArgIn :: Handle,
    handleMsgOut :: Handle
  }

data Pool = Pool
  { poolLimit :: Int,
    poolStatus :: IntMap (Bool, Maybe Id),
    poolHandles :: [(Int, HandleSet)]
  }

dumpStatus :: TVar Pool -> IO ()
dumpStatus ref = do
  pool <- atomically (readTVar ref)
  mapM_ (hPrint stdout) $ IM.toAscList (poolStatus pool)
  hFlush stdout

getAssignableWorker :: IntMap (Bool, Maybe Id) -> Maybe Id -> Maybe (Int, (Bool, Maybe Id))
getAssignableWorker workers mid' = List.find (isAssignable . snd) . IM.toAscList $ workers
  where
    isAssignable (b, mid)
      | not b = case (mid, mid') of
                  (Nothing, _) -> True
                  (Just _, Nothing) -> True
                  (Just id'', Just id') -> id' == id''
      | otherwise = False

assignJob :: TVar Pool -> Maybe Id -> STM (Either Int (Int, HandleSet))
assignJob ref mid' = do
  pool <- readTVar ref
  let workers = poolStatus pool
  let m = getAssignableWorker workers mid'
  case m of
    Nothing -> do
      let nRunningJobs = length $ filter (\(b, _) -> b) $ F.toList workers
      if (nRunningJobs >= poolLimit pool)
        then retry
        else pure (Left nRunningJobs)
    Just (i, _) -> do
      let upd (_, Nothing) = Just (True, mid')
          upd (_, Just id'') = Just (True, Just id'')
          !workers' = IM.update upd i workers
          Just hset = List.lookup i (poolHandles pool)
      writeTVar ref (pool {poolStatus = workers'})
      pure $ Right (i, hset)

finishJob :: TVar Pool -> Int -> STM ()
finishJob ref i = do
  pool <- readTVar ref
  let workers = poolStatus pool
      !workers' = IM.update (\(_, m)  -> Just (False, m)) i workers
  writeTVar ref (pool {poolStatus = workers'})

removeWorker :: TVar Pool -> Id -> STM ()
removeWorker ref id' = do
  pool <- readTVar ref
  let workers = poolStatus pool
      upd (b, m)
        | m == Just id' = (False, Nothing)
        | otherwise = (b, m)
      !workers' = fmap upd workers
  writeTVar ref (pool {poolStatus = workers'})
