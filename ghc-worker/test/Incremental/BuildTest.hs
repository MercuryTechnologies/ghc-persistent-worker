module Incremental.BuildTest where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog (TestT, assert, diff, (===))
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, osp, (</>))
import Test.BuckHashes (writeUnitHashes)
import Test.Build (initialStrategy, resumeStrategy, runSchedule)
import Test.Cache (writeResumeCache)
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Env (MaxJobs (..), SessionEnv (..))
import Test.Data.Project (
  BuildModule (..),
  Component (..),
  GenUnit (..),
  ModuleKey (..),
  ResumeComponent (..),
  TaskKey (..),
  UnitKey (..),
  )
import Test.Data.Scheduler (Dispatch (..), Schedule (..), Task (..), runDispatch)
import Test.Data.SourceMode (ModuleSource (..), SourceMode (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Path (moduleSourcePath, unitOutputDir)
import Test.Run (unitTest)
import Test.Source (moduleSource, writeProjectSources)
import Test.Tasty (TestTree)

modulesPerUnit :: Int
modulesPerUnit = 2

-- | Module keys for a unit.
testModuleKeys :: UnitKey -> [ModuleKey]
testModuleKeys unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [0 .. modulesPerUnit - 1]]

intraUnitDeps :: ModuleKey -> Set.Set ModuleKey
intraUnitDeps ModuleKey {unit, number} =
  Set.fromList [ModuleKey {unit, number = n, errorVariant = Nothing} | n <- [0 .. number - 1]]

crossUnitDeps :: Set.Set UnitKey -> Set.Set ModuleKey
crossUnitDeps depUnits =
  Set.fromList [mk | u <- Set.toList depUnits, mk <- testModuleKeys u]

testModuleDeps :: Set.Set UnitKey -> ModuleKey -> Set.Set ModuleKey
testModuleDeps depUnits key =
  Set.union (intraUnitDeps key) (crossUnitDeps depUnits)

mkTestUnit :: UnitKey -> Set.Set UnitKey -> GenUnit BuildModule
mkTestUnit unitKey depUnits =
  GenUnit {
    key = unitKey,
    depUnits,
    modules = [
      BuildModule {key, deps = testModuleDeps depUnits key, th = False, bindings = 1, extDeps = mempty}
      | key <- testModuleKeys unitKey
    ]
  }

metaTask :: GenUnit BuildModule -> Task TaskKey Component
metaTask unit =
  Task {
    key = TaskMeta unit.key,
    deps = Set.map TaskMeta unit.depUnits,
    value = ComponentUnit unit
  }

moduleTask :: UnitKey -> BuildModule -> Task TaskKey Component
moduleTask unitKey BuildModule {key, deps} =
  Task {
    key = TaskCompile key,
    deps = Set.insert (TaskMeta unitKey) (Set.map TaskCompile deps),
    value = ComponentModule key
  }

unitTasks :: GenUnit BuildModule -> [Task TaskKey Component]
unitTasks unit =
  metaTask unit : [moduleTask unit.key bm | bm <- unit.modules]

testSchedule :: [GenUnit BuildModule] -> Schedule TaskKey Component
testSchedule units =
  Schedule {tasks = concatMap unitTasks units}

-- | The test project: 2 units, 2 modules each. No TH, 1 binding, no ext deps.
-- Unit 0: leaf, Unit 1: depends on Unit 0.
allUnits :: [GenUnit BuildModule]
allUnits =
  [
    mkTestUnit 0 Set.empty,
    mkTestUnit 1 (Set.singleton 0)
  ]

-- | Source map for writing project sources.
sourcesMap :: Map ModuleKey ModuleSource
sourcesMap =
  Map.fromList
    [(m.key, ModuleSource {deps = Set.toList m.deps, th = m.th, bindings = m.bindings, extDeps = m.extDeps})
    | u <- allUnits, m <- u.modules]

-- | All source file paths for the project.
allSourcePaths :: SessionEnv -> [OsPath]
allSourcePaths env =
  [env.sourceDir </> moduleSourcePath m.key | u <- allUnits, m <- u.modules]

-- | Write per-unit buck_source_hashes files for the test project.
writeProjectHashes :: SessionEnv -> IO [LazyByteString]
writeProjectHashes env =
  traverse (writeUnitHashes env.tempDir env.sourceDir) allUnits

-- | Write per-unit buck_source_hashes for just the modified unit after a source change.
writeModifiedUnitHashes :: SessionEnv -> IO (Maybe LazyByteString)
writeModifiedUnitHashes env =
  case [u | u <- allUnits, u.key == modifiedModule.unit] of
    unit : _ -> Just <$> writeUnitHashes env.tempDir env.sourceDir unit
    [] -> pure Nothing

-- | The module key we modify between builds.
modifiedModule :: ModuleKey
modifiedModule = ModuleKey {unit = 1, number = 1, errorVariant = Nothing}

-- | Rewrite one module's source to simulate a change.
modifySource :: SessionEnv -> IO (Maybe LazyByteString)
modifySource env = do
  let path = env.sourceDir </> moduleSourcePath modifiedModule
      deps = Set.toList (testModuleDeps (Set.singleton 0) modifiedModule)
  OsPath.writeFile path (moduleSource 1 False mempty SourceModified modifiedModule deps)
  writeModifiedUnitHashes env

-- | Read the build plan JSON written by the metadata step for a unit.
readBuildPlan :: SessionEnv -> UnitKey -> IO Aeson.Value
readBuildPlan env unit = do
  let path = env.tempDir </> unitOutputDir unit </> [osp|build-plan.json|]
  content <- OsPath.readFile path
  case Aeson.decode content of
    Just v -> pure v
    Nothing -> fail ("Failed to decode build plan for unit " ++ show unit)

-- | Read build plans for all units, keyed by unit number.
readAllBuildPlans :: SessionEnv -> IO (Map Int Aeson.Value)
readAllBuildPlans env =
  Map.fromList <$> traverse readOne [0, 1]
  where
    readOne n = (n,) <$> readBuildPlan env (UnitKey n)

-- | The set of tasks belonging to unit 0, used as pre-completed in rebuild schedules so that unit 1's compile tasks
-- (which depend on unit 0's compile tasks) have their dependencies satisfied.
unit0PreCompleted :: Set.Set TaskKey
unit0PreCompleted =
  Set.fromList $ TaskMeta (UnitKey 0) : [TaskCompile mk | mk <- testModuleKeys (UnitKey 0)]

-- | Filter a resume schedule to only include unit 1's tasks.
-- Unit 0's tasks are excluded from dispatch; they are only used as pre-completed dependencies.
unit1Schedule :: Schedule TaskKey ResumeComponent -> Schedule TaskKey ResumeComponent
unit1Schedule (Schedule tasks) = Schedule (filter isUnit1 tasks)
  where
    isUnit1 task = case task.key of
      TaskMeta (UnitKey 1) -> True
      TaskCompile ModuleKey {unit = UnitKey 1} -> True
      _ -> False

-- | Run the initial build, using incremental metadata when 'useIncremental' is 'True'.
runInitialBuild :: SessionEnv -> Bool -> IO BuildResult
runInitialBuild env useIncremental =
  runSchedule (MaxJobs 1) (initialStrategy env useIncremental) Set.empty (testSchedule allUnits)

-- | Run a rebuild using cache-restored unit 0 state.
--
-- Only unit 1's tasks are dispatched; unit 0's tasks are excluded from the schedule and listed as pre-completed so
-- that unit 1's compile tasks (which transitively depend on unit 0's) have their dep sets satisfied.
-- @loadCachedUnits@ is thereby forced to restore unit 0 from the cache files written by 'writeResumeCache'.
--
-- Also asserts that exactly one metadata task was dispatched (unit 1's), confirming that unit 0's metadata
-- was not re-run.
runRebuild :: SessionEnv -> Bool -> Schedule TaskKey ResumeComponent -> TestT IO BuildResult
runRebuild env useIncremental schedule = do
  metaCount <- liftIO $ newIORef (0 :: Int)
  let countAndDispatch component = do
        case component of
          ResumeUnit {} -> atomicModifyIORef' metaCount (\n -> (n + 1, ()))
          _ -> pure ()
        runDispatch (resumeStrategy env useIncremental False) component
  result <- liftIO $ runSchedule (MaxJobs 1) (Dispatch countAndDispatch) unit0PreCompleted (unit1Schedule schedule)
  count <- liftIO $ readIORef metaCount
  diff count (==) 1
  pure result

test_incrementalBuild :: TestTree
test_incrementalBuild =
  withTestEnv \ getTestEnv ->
    unitTest "incremental vs full metadata equivalence" do
      sessionEnv <- liftIO (newSessionEnv =<< getTestEnv)
      liftIO $ writeProjectSources sessionEnv.sourceDir sourcesMap

      -- Step 1: Initial build with buck_source_hashes to establish incremental state.
      -- This is a full build (no prior state file), but it writes the state file for the next run.
      void $ liftIO (writeProjectHashes sessionEnv)
      initialResult <- liftIO $ runInitialBuild sessionEnv True
      assert (not initialResult.hasErrors)

      -- Write resume cache files for all units so that the rebuild steps can restore unit 0 via 'loadCachedUnits'
      -- instead of re-running its metadata step.
      resumeSchedule <- liftIO $ writeResumeCache sessionEnv (testSchedule allUnits)

      -- Step 2: Modify a source, update buck_source_hashes, rebuild with incremental (fresh worker state).
      -- The incremental state file from step 1 persists, so buildPlanForSources takes the incremental path.
      -- modifySource also updates the per-unit buck_source_hashes for unit 1.
      void $ liftIO (modifySource sessionEnv)
      incrementalEnv <- liftIO $ newResumeSessionEnv sessionEnv
      incrementalResult <- runRebuild incrementalEnv True resumeSchedule
      diff incrementalResult.hasErrors (==) False

      incrementalPlans <- liftIO $ readAllBuildPlans incrementalEnv

      -- Step 3: Same modified sources, but full metadata from a clean state (no buck_source_hashes).
      fullEnv <- liftIO $ newResumeSessionEnv sessionEnv
      fullResult <- runRebuild fullEnv False resumeSchedule
      diff fullResult.hasErrors (==) False

      fullPlans <- liftIO $ readAllBuildPlans fullEnv

      -- Core assertion: incremental and full metadata produce the same build plan.
      incrementalPlans === fullPlans

      -- Verify that the build produced non-empty results.
      diff (Map.size incrementalPlans) (==) 2
