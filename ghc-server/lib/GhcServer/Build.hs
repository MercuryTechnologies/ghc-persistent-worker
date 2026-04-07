-- | Scheduler orchestration for the standalone GHC server.
--
-- This module manages the scheduler lifecycle: creating builds, submitting
-- batches, awaiting results.  It delegates build-system logic to
-- 'GhcServer.Build.Classify' and worker\/GHC adaptation to
-- 'GhcServer.Build.Propagate'.
module GhcServer.Build (
  -- * Build lifecycle
  Build (..),
  newBuild,
  scheduleBatch,
  awaitBuild,
  stopBuild,
  runBuild,
  newBuildState,
  -- * Re-exports
  BuildResult (..),
) where

import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (atomically, readTVar)
import Data.Set (Set)
import Data.Void (Void)
import GhcServer.Build.Classify (BuildResult (..), classifyBuildRequest, collectBuildResult)
import GhcServer.Build.Propagate (
  BuildExt (..),
  dispatchTask,
  emptyBuildExt,
  propagateCompletion,
  )
import GhcServer.Build.Schedule (BuildStatus, TaskKey (..))
import GhcServer.Cache (mkBuildCache)
import GhcServer.Data.BuildCache (BuildCache (..))
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Request (ScheduleRequest)
import GhcServer.Data.Unit (UnitName)
import GhcServer.Scheduler (
  Handlers (..),
  SchedulerEnv (..),
  SchedulerResources (..),
  SchedulerState (..),
  awaitIdle,
  newSchedulerState,
  runScheduler,
  submitRequest,
  )
import Internal.State (newState)
import Prelude hiding (log)
import Types.State (WorkerState)

-- | Shared state for a build session.
-- Created once, supports multiple 'scheduleBatch' calls.
data Build =
  Build {
    scheduler :: SchedulerResources ScheduleRequest TaskKey BuildStatus BuildExt,
    thread :: Async Void,
    -- | Units that were already cached at build start (metadata skipped for these).
    cachedUnits :: Set UnitName
  }

-- | Create a new build session.
--
-- Starts the scheduler loop in a background thread.  The loop classifies requests
-- and dispatches tasks.  Metadata completion triggers resolution and promotion
-- of pending compile tasks via the 'propagate' callback.
newBuild :: Int -> Int -> BuildEnv -> IO Build
newBuild maxJobs taskTimeout buildEnv = do
  let cache = mkBuildCache buildEnv.outputDir buildEnv.project
  cachedUnits <- cache.cachedUnits
  scheduler <- newSchedulerState emptyBuildExt
  let
    env = SchedulerEnv {
      maxJobs,
      handlers = Handlers {
        dispatch = dispatchTask cache buildEnv,
        classify = classifyBuildRequest cachedUnits buildEnv,
        propagate = propagateCompletion cache buildEnv
      },
      taskTimeout
    }
  thread <- runScheduler env scheduler
  pure Build {cachedUnits, ..}

-- | Submit a batch of build requests to the scheduler.  Non-blocking.
scheduleBatch :: Build -> ScheduleRequest -> IO ()
scheduleBatch cb request =
  submitRequest cb.scheduler request

-- | Wait for all submitted tasks to complete, then collect results.
awaitBuild :: Build -> IO BuildResult
awaitBuild cb = do
  SchedulerState {completed, failures} <- atomically do
    awaitIdle cb.scheduler
    readTVar cb.scheduler.state
  pure (collectBuildResult completed failures)

-- | Wait for all submitted tasks, collect results, and cancel the scheduler thread.
--
-- Use this for one-shot builds and tests to avoid leaking the background thread.
stopBuild :: Build -> IO BuildResult
stopBuild cb = do
  result <- awaitBuild cb
  cancel cb.thread
  pure result

-- | Dispatch a build using the concurrent scheduler.
--
-- Creates a scheduler, submits one batch, waits for completion, then cancels
-- the scheduler thread.  For persistent schedulers, use 'newBuild',
-- 'scheduleBatch', and 'awaitBuild' directly.
runBuild :: Int -> Int -> BuildEnv -> ScheduleRequest -> IO BuildResult
runBuild maxJobs taskTimeout env schedule = do
  cb <- newBuild maxJobs taskTimeout env
  scheduleBatch cb schedule
  stopBuild cb

-- | Create a fresh 'WorkerState' for use with 'runBuild'.
newBuildState :: IO (MVar WorkerState)
newBuildState = newState
