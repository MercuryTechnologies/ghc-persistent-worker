{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Pool
( WorkerId,
  JobId (..),
  HandleSet (..),
  Pool (..),
  JobStatus (..),
  newWorkerId,
  newJobId,
  dumpStatus,
  assignJob,
  finishJob,
  removeWorker,
) where

import Control.Concurrent.STM (STM, TChan, TVar, atomically, readTVar, retry, writeTVar)
import Control.Exception (mask)
import qualified Data.Foldable as F
import Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IM
import qualified Data.List as List
import Message (Response, TargetId)
import System.IO (Handle, hFlush, hPrint, hPutStrLn, stdout)
import System.Process (ProcessHandle, terminateProcess)

type WorkerStatus = IntMap (Bool, Maybe TargetId)
type WorkerId = Key

newtype JobId = JobId Int
  deriving (Eq, Num, Ord, Show)

data HandleSet = HandleSet
  { handleProcess :: ProcessHandle,
    handleArgIn :: Handle,
    handleMsgOut :: Handle
  }

data Pool = Pool
  { poolLimit :: Int,
    poolNewWorkerId :: WorkerId,
    poolNewJobId :: JobId,
    poolStatus :: WorkerStatus,
    poolHandles :: [(WorkerId, HandleSet)]
  }

data JobStatus = JobStatus
  { jobStatusChan :: [(JobId, TChan Response)]
  }

newWorkerId :: TVar Pool -> STM WorkerId
newWorkerId ref = do
  pool <- readTVar ref
  let i = poolNewWorkerId pool
  writeTVar ref (pool {poolNewWorkerId = i + 1})
  pure i

newJobId :: TVar Pool -> STM JobId
newJobId ref = do
  pool <- readTVar ref
  let JobId i = poolNewJobId pool
  writeTVar ref (pool {poolNewJobId = JobId (i + 1)})
  pure (JobId i)

dumpStatus :: TVar Pool -> IO ()
dumpStatus ref = do
  pool <- atomically (readTVar ref)
  hPutStrLn stdout $ "poolLimit = " ++ show (poolLimit pool)
  mapM_ (hPrint stdout) $ IM.toAscList (poolStatus pool)
  hFlush stdout

getAssignableWorker ::
  IntMap (Bool, Maybe TargetId) ->
  Maybe TargetId ->
  Maybe (WorkerId, (Bool, Maybe TargetId))
getAssignableWorker workers mid' = List.find (isAssignable . snd) . IM.toAscList $ workers
  where
    isAssignable (b, mid)
      | not b = case (mid, mid') of
                  (Nothing, _) -> True
                  (Just _, Nothing) -> True
                  (Just id'', Just id') -> id' == id''
      | otherwise = False

assignJob ::
  TVar Pool ->
  Maybe TargetId ->
  -- | Right assigned, Left new id that will be used for new spawned worker process.
  STM (Either Int (JobId, WorkerId, HandleSet))
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
    Just (wid, _) -> do
      let upd (_, Nothing) = Just (True, mid')
          upd (_, Just id'') = Just (True, Just id'')
          !workers' = IM.update upd wid workers
          Just hset = List.lookup wid (poolHandles pool)
      writeTVar ref (pool {poolStatus = workers'})
      jid <- newJobId ref
      pure $ Right (jid, wid, hset)

finishJob :: TVar Pool -> WorkerId -> STM ()
finishJob ref i = do
  pool <- readTVar ref
  let workers = poolStatus pool
      !workers' = IM.update (\(_, m)  -> Just (False, m)) i workers
  writeTVar ref (pool {poolStatus = workers'})

removeWorker :: TVar Pool -> TargetId -> IO ()
removeWorker ref id' = mask $ \_restore -> do
  dismissedHandles <-
    atomically $ do
      pool <- readTVar ref
      let workers = poolStatus pool
          (dismissed, remained) = IM.partition (\(_, m) -> m == Just id') workers
      let ks = IM.keys dismissed
          (dismissedHandles, remainedHandles) = List.partition ((`elem` ks) . fst) $ poolHandles pool
      writeTVar ref (pool {poolStatus = remained, poolHandles = remainedHandles})
      pure (fmap (handleProcess . snd) dismissedHandles)
  mapM_ terminateProcess dismissedHandles
