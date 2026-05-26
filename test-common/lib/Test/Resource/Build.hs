-- | Sequential build runner for the resource consumption test.
module Test.Resource.Build where

import Data.Foldable (fold)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Test.Build (initialStrategy, runSchedule)
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Env (MaxJobs (..), SessionEnv (..))
import Test.Data.Project (BuildModule (..), Component (..), GenUnit (..), ModuleKey (..))
import Test.Data.Scheduler (RequestResult)
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
measuredStrategy :: IORef [PhaseResult] -> SessionEnv -> Component -> IO RequestResult
measuredStrategy ref env component = do
  (result, phase) <- measurePhase (phaseName component) (initialStrategy env False component)
  modifyIORef' ref (phase :)
  pure result

-- | Run the full build sequentially, measuring allocations per task.
-- This uses the scheduler for convenience, even though there's no concurrency involved.
runResourceBuild :: [GenUnit BuildModule] -> SessionEnv -> IO (BuildResult, [PhaseResult])
runResourceBuild units env = do
  extDepDbs <- createExtDepPackageDbs env.tempDir allExtDeps
  let envWithExtDeps = env {extDepDbs, extDeps = allExtDeps}
  writeProjectSources envWithExtDeps.sourceDir (allModuleSources units)
  phasesRef <- newIORef []
  buildResult <- runSchedule (MaxJobs 1) (measuredStrategy phasesRef envWithExtDeps) Set.empty (schedule units)
  taskPhases <- reverse <$> readIORef phasesRef
  pure (buildResult, taskPhases)
  where
    allExtDeps = fold [bm.extDeps | u <- units, bm <- u.modules]
