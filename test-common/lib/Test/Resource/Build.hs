-- | Sequential build runner for the resource consumption test.
module Test.Resource.Build where

import Data.Foldable (fold)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Set (Set)
import Test.Build (initialStrategy, runSchedule)
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Env (MaxJobs (..), SessionEnv (..))
import Test.Data.Project (BuildModule (..), Component (..), GenUnit (..), ModuleKey (..), TaskKey)
import Test.Data.Scheduler (Dispatch (..), Schedule, runDispatch)
import Test.ExtDep (createExtDepPackageDbs)
import Test.Path (showUnit)
import Test.Resource.Project (allModuleSources, schedule)
import Test.Resource.Stats (PhaseResult, measurePhase)
import Test.Source (writeProjectSources)

-- | Phase name for a build task, e.g. @unit_0_metadata@ or @unit_1_compile_2@.
phaseName :: Component -> String
phaseName = \case
  ComponentUnit unit -> "unit_" ++ showUnit unit.key ++ "_metadata"
  ComponentModule key -> "unit_" ++ showUnit key.unit ++ "_compile_" ++ show key.number

-- | Wrap 'initialStrategy' to measure allocations per task and accumulate results.
measuredStrategy ::
  Dispatch task ->
  (task -> String) ->
  IORef [PhaseResult] ->
  Dispatch task
measuredStrategy inner name ref =
  Dispatch \ component -> do
    (result, phase) <- measurePhase (name component) (runDispatch inner component)
    modifyIORef' ref (phase :)
    pure result

-- | Run the full build sequentially, measuring allocations per task.
-- This uses the scheduler for convenience, even though there's no concurrency involved.
withMeasuredBuild ::
  Dispatch task ->
  (task -> String) ->
  Set TaskKey ->
  Schedule TaskKey task ->
  IO (BuildResult, [PhaseResult])
withMeasuredBuild build name unmodified sched = do
  phasesRef <- newIORef []
  buildResult <- runSchedule (MaxJobs 1) (measuredStrategy build name phasesRef) unmodified sched
  taskPhases <- reverse <$> readIORef phasesRef
  pure (buildResult, taskPhases)

-- | Run the full build sequentially, measuring allocations per task.
-- This uses the scheduler for convenience, even though there's no concurrency involved.
runResourceBuild :: [GenUnit BuildModule] -> SessionEnv -> IO (BuildResult, [PhaseResult])
runResourceBuild units env = do
  extDepDbs <- createExtDepPackageDbs env.tempDir allExtDeps
  let envWithExtDeps = env {extDepDbs, extDeps = allExtDeps}
  writeProjectSources envWithExtDeps.sourceDir (allModuleSources units)
  withMeasuredBuild (initialStrategy envWithExtDeps False) phaseName [] (schedule units)
  where
    allExtDeps = fold [bm.extDeps | u <- units, bm <- u.modules]
