module Resource.LazyByteCodeTest where

import Control.Concurrent (MVar, readMVar)
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Data.FastString (FastString)
import GHC.Types.Unique.DFM (eltsUDFM)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), UnitEnvGraph (..))
import GHC.Unit.Home.ModInfo (HomeModInfo (..))
import GHC.Unit.Home.PackageTable (HomePackageTable (..))
import GHC.Unit.Module.ModIface (mi_mnwib)
import GHC.Utils.Outputable (showPprUnsafe)
import Hedgehog ((===))
import Hedgehog.Internal.Property (TestT)
import Resource.Measure (assertMeasurements, checkEnvironment)
import System.IO (hPutStrLn, stderr)
import Test.Build (initialStrategy, resumeStrategy)
import Test.BuildSystem (mkBuildSystem)
import Test.Bytecode (enableLazyByteCode, loadedBcos)
import Test.Data.Env (SessionEnv (..), TestEnv)
import Test.Data.Project (
  BuildModule (..),
  Component,
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  ModuleSource (..),
  TaskKey (..),
  UnitKey,
  weakenResumeComponent,
  )
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..), scheduleKeys)
import Test.Data.Scheduler (Schedule (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Gen.ProjectBuild (metaTask, moduleTask, sortSchedule)
import qualified Test.Path as Test
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resource.Build (phaseName, withMeasuredBuild)
import Test.Resource.Stats (PhaseReference (..), PhaseResult (..))
import Test.Resume (setupResumeBuild, trimResumeSchedule)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

numMods :: Int
numMods = 8

keys1 :: UnitKey -> [ModuleKey]
keys1 unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [1 .. numMods]]

keys2 :: UnitKey -> [ModuleKey]
keys2 unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [numMods + 1 .. 2 * numMods]]

mods1 :: UnitKey -> [BuildModule]
mods1 unit =
  [BuildModule {key, deps = mempty, th = False, bindings = 1, extDeps = mempty} | key <- keys1 unit]

mods2 :: UnitKey -> [BuildModule]
mods2 unit =
  [BuildModule {key, deps = mempty, th = False, bindings = 1, extDeps = mempty} | key <- keys2 unit]

modTh :: BuildModule
modTh =
  BuildModule {
    key = ModuleKey {unit = 1, number = 2 * numMods + 1, errorVariant = Nothing},
    deps = Set.fromList (keys1 0 ++ keys1 1),
    th = True,
    bindings = 1,
    extDeps = mempty
  }

modNoTh :: BuildModule
modNoTh =
  BuildModule {
    key = ModuleKey {unit = 1, number = 2 * numMods + 2, errorVariant = Nothing},
    deps = Set.fromList (keys2 0 ++ keys2 1),
    th = False,
    bindings = 1,
    extDeps = mempty
  }

unit0 :: GenUnit BuildModule
unit0 =
  GenUnit {key, depUnits = mempty, modules = mods1 key ++ mods2 key}
  where
    key = 0

unit1 :: GenUnit BuildModule
unit1 =
  GenUnit {key, depUnits = [unit0.key], modules = mods1 key ++ mods2 key ++ [modTh, modNoTh]}
  where
    key = 1

units :: [GenUnit BuildModule]
units =
  [unit0, unit1]

modules :: [BuildModule]
modules =
  concatMap (.modules) units

moduleSource :: BuildModule -> ModuleSource
moduleSource BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

thInitial :: InitialProject
thInitial =
  InitialProject
    { modules = mods
    , modulesSuccess = mods
    , modulesError = mempty
    , unitCount = length units
    , moduleCount = length modules
    }
  where
    mods = Map.fromList [(m.key, moduleSource m) | m <- modules]

schedule :: Schedule TaskKey Component
schedule =
  fst (sortSchedule (concatMap unitTasks units))
  where
    unitTasks unit = metaTask unit unit.depUnits : [moduleTask unit m | m <- unit.modules]

build :: ProjectBuild
build =
  ProjectBuild {
    initial = thInitial,
    schedule = schedule,
    resumePlan = ResumePlan {
      fixErrors = False,
      moduleMutations = rebuild,
      depMutations = mempty,
      rebuild = RebuildSet {
        moduleKeys = rebuildKeys,
        allAffectedKeys = Set.map TaskCompile rebuildKeys,
        hasChanges = True
      }
    },
    resumeSchedule = schedule,
    allKeys = scheduleKeys schedule,
    incrementalBuildPlan = False
  }
  where
    rebuildKeys = Map.keysSet rebuild
    rebuild = [(modTh.key, moduleSource modTh), (modNoTh.key, moduleSource modNoTh)]

hptEntries :: MVar WorkerState -> IO [String]
hptEntries stateVar = do
  state <- readMVar stateVar
  sort <$> concatMapM unitEntries (Map.elems $ unitEnv_graph state.make.hug)
  where
    unitEntries HomeUnitEnv {homeUnitEnv_hpt} = do
      hpt <- readIORef homeUnitEnv_hpt.table
      pure (showPprUnsafe . mi_mnwib . hm_iface <$> eltsUDFM hpt)

targetEntriesAll :: [String]
targetEntriesAll =
  sort [Test.moduleName key | BuildModule {key} <- modules]

targetEntriesAfterResume1 :: [String]
targetEntriesAfterResume1 =
  sort [Test.moduleName key | key <- keys1 0 ++ keys1 1 ++ [modTh.key]]

targetBcos :: [(FastString, FastString, [String])]
targetBcos =
  [
    ("unit0", "Unit0Module1", ["value_0_1"]),
    ("unit0", "Unit0Module2", ["value_0_2"]),
    ("unit0", "Unit0Module3", ["value_0_3"]),
    ("unit0", "Unit0Module4", ["value_0_4"]),
    ("unit0", "Unit0Module5", ["value_0_5"]),
    ("unit0", "Unit0Module6", ["value_0_6"]),
    ("unit0", "Unit0Module7", ["value_0_7"]),
    ("unit0", "Unit0Module8", ["value_0_8"]),
    ("unit1", "Unit1Module1", ["value_1_1"]),
    ("unit1", "Unit1Module2", ["value_1_2"]),
    ("unit1", "Unit1Module3", ["value_1_3"]),
    ("unit1", "Unit1Module4", ["value_1_4"]),
    ("unit1", "Unit1Module5", ["value_1_5"]),
    ("unit1", "Unit1Module6", ["value_1_6"]),
    ("unit1", "Unit1Module7", ["value_1_7"]),
    ("unit1", "Unit1Module8", ["value_1_8"])
  ]

-- | This test restores bytecode from cache in the resume build.
-- The project consists of two units with 16 modules each.
-- The second unit has two additional modules that are both recompiled.
-- One of those has a splice in them that depends on the first 8 modules in each unit, while the other has no splice and
-- depends on the remaining 8 modules in each.
--
-- The test asserts that BCOs are only loaded for the deps of the splice.
-- It also asserts the allocations made for each compilation.
--
-- TODO Also assert that no linkable is in the HPT for the second half
testMemoryBytecode :: TestEnv -> TestT IO ([PhaseResult], [PhaseResult], [PhaseResult])
testMemoryBytecode testEnv = do
  sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  let buildSys = mkBuildSystem 6 False sessionEnv

  (initialResult, measureInitial) <- liftIO do
    writeProjectSources sessionEnv.sourceDir build.initial.modules
    withMeasuredBuild (initialStrategy sessionEnv False) phaseName [] build.schedule
  assertNoFailures "initial" initialResult
  entriesAfterInit <- liftIO $ hptEntries sessionEnv.env.state
  targetEntriesAll === entriesAfterInit

  cachedSchedule <- liftIO $ setupResumeBuild buildSys sessionEnv build initialResult
  resumeEnv <- liftIO $ newResumeSessionEnv sessionEnv

  let (resumeTasks, unmodified) = trimResumeSchedule initialResult build.resumePlan.rebuild cachedSchedule.tasks
      strat = resumeStrategy resumeEnv False False
      resumeBuild tasks =
        withMeasuredBuild strat (phaseName . weakenResumeComponent) unmodified (Schedule tasks)

  (resumeResult1, measureResume1) <- liftIO $ resumeBuild (take 1 resumeTasks.tasks)
  assertNoFailures "resume 1" resumeResult1
  entriesAfterResume1 <- liftIO $ hptEntries resumeEnv.env.state
  targetEntriesAfterResume1 === entriesAfterResume1
  bcos0 <- loadedBcos resumeEnv.env
  targetBcos === bcos0

  (resumeResult2, measureResume2) <- liftIO $ resumeBuild (drop 1 resumeTasks.tasks)
  assertNoFailures "resume 2" resumeResult2
  entriesAfterResume2 <- liftIO $ hptEntries resumeEnv.env.state
  targetEntriesAll === entriesAfterResume2

  bcos1 <- loadedBcos resumeEnv.env
  bcos0 === bcos1
  pure (measureInitial, measureResume1, measureResume2)

mkRef :: (String, Double) -> PhaseReference
mkRef (name, allocatedMB) =
  PhaseReference {name, allocatedMB, tolerancePercent = 5}

targetInit :: [PhaseReference]
targetInit =
  mkRef <$> [
    ("unit_0_metadata", 24.7),
    ("unit_1_metadata", 13.8),
    ("unit_1_compile_17", 57.5),
    ("unit_1_compile_18", 23.3)
  ]

targetResume1 :: [PhaseReference]
targetResume1 =
  [mkRef ("unit_1_compile_17", 90)]

targetResume2 :: [PhaseReference]
targetResume2 =
  [mkRef ("unit_1_compile_18", 24.62)]

test_memory_lazyByteCode :: TestTree
test_memory_lazyByteCode =
  withTestEnv \ getEnv ->
    unitTest "lazy bytecode allocations" do
      maybe (run getEnv) skip =<< liftIO checkEnvironment
  where
    run getEnv = do
      env <- liftIO getEnv
      (initial, resume1, resume2) <- testMemoryBytecode env
      assertMeasurements targetInit initial
      assertMeasurements targetResume1 resume1
      assertMeasurements targetResume2 resume2

    skip reason =
      liftIO $ hPutStrLn stderr $ "Skipping resource test: " ++ reason
