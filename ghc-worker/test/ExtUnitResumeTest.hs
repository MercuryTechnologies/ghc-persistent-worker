{-# LANGUAGE CPP #-}

-- | Resume builds in which units are restored from the cache, and a splice runs the code of a restored module that
-- uses an external package.
--
-- Unit 1's module imports external package 0. In the resume build, unit 3's splice runs first, as unit 2's module
-- imports unit 3's, and starts the worker's loader before unit 1 is restored. The loader preloads the packages of the
-- units it knows when it starts, so it does not preload unit 1's. Unit 2's splice then runs unit 1's code, and the
-- linker learns which packages to load for it from the restored module graph.
module ExtUnitResumeTest (test_extUnitResume) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import GHC.Unit.Module.Graph (ModuleGraphNode (..))
import GHC.Unit.Types (stringToUnitId, unitIdString)
import Hedgehog (TestT, annotate, assert, failure)
import Test.BuildSystem (mkBuildSystem)
import Test.Bytecode (enableLazyByteCode, envLoader)
import Test.Data.BuildSystem (BuildResult, BuildSystem (..))
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (
  BuildModule (..),
  BuildTask,
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  ModuleSource (..),
  )
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..), scheduleKeys)
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.ExtDep (createExtDepPackageDbs)
import Test.Gen.ProjectBuild (metaTask, moduleTask, sortSchedule)
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

module_1_1 :: BuildModule
module_1_1 =
  BuildModule {
    key = ModuleKey {unit = 1, number = 1, errorVariant = Nothing},
    deps = mempty,
    th = False,
    bindings = 1,
    extDeps = Set.singleton 0
  }

module_2_1 :: BuildModule
module_2_1 =
  BuildModule {
    key = ModuleKey {unit = 2, number = 1, errorVariant = Nothing},
    deps = mempty,
    th = False,
    bindings = 1,
    extDeps = mempty
  }

module_2_2 :: BuildModule
module_2_2 =
  BuildModule {
    key = ModuleKey {unit = 2, number = 2, errorVariant = Nothing},
    deps = Set.fromList [module_1_1.key, module_3_2.key],
    th = True,
    bindings = 1,
    extDeps = mempty
  }

module_3_1 :: BuildModule
module_3_1 =
  BuildModule {
    key = ModuleKey {unit = 3, number = 1, errorVariant = Nothing},
    deps = mempty,
    th = False,
    bindings = 1,
    extDeps = mempty
  }

module_3_2 :: BuildModule
module_3_2 =
  BuildModule {
    key = ModuleKey {unit = 3, number = 2, errorVariant = Nothing},
    deps = Set.singleton module_3_1.key,
    th = True,
    bindings = 1,
    extDeps = mempty
  }

unit1 :: GenUnit BuildModule
unit1 = GenUnit {key = 1, depUnits = mempty, modules = [module_1_1]}

unit2 :: GenUnit BuildModule
unit2 = GenUnit {key = 2, depUnits = Set.fromList [1, 3], modules = [module_2_1, module_2_2]}

unit3 :: GenUnit BuildModule
unit3 = GenUnit {key = 3, depUnits = mempty, modules = [module_3_1, module_3_2]}

source :: BuildModule -> ModuleSource
source BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

initial :: InitialProject
initial =
  InitialProject {
    modules = mods,
    modulesSuccess = mods,
    modulesError = mempty,
    unitCount = 3,
    moduleCount = 5
  }
  where
    mods = Map.fromList [(m.key, source m) | m <- [module_1_1, module_2_1, module_2_2, module_3_1, module_3_2]]

-- | Initial build: every module without a splice.
initialTasks :: [BuildTask]
initialTasks =
  [
    metaTask unit1 unit1.depUnits,
    moduleTask unit1 module_1_1,
    metaTask unit3 unit3.depUnits,
    moduleTask unit3 module_3_1,
    metaTask unit2 unit2.depUnits,
    moduleTask unit2 module_2_1
  ]

-- | Resume build: the two modules with a splice. Unit 3's runs first, as unit 2's imports it, and starts the worker's
-- loader before unit 1 is restored, so the loader does not preload unit 1's packages.
thResumeTasks :: [BuildTask]
thResumeTasks = initialTasks ++ [moduleTask unit3 module_3_2, moduleTask unit2 module_2_2]

-- | The initial build compiles the modules without a splice, the resume build the two with one, restoring all units
-- from the cache.
build :: ProjectBuild
build =
  ProjectBuild {
    initial = initial,
    schedule = fst (sortSchedule initialTasks),
    resumePlan = ResumePlan {
      fixErrors = False,
      moduleMutations = mempty,
      depMutations = mempty,
      rebuild = RebuildSet {moduleKeys = mempty, allAffectedKeys = mempty, hasChanges = True}
    },
    resumeSchedule = resumeSchedule',
    allKeys = scheduleKeys resumeSchedule'
  }
  where
    resumeSchedule' = fst (sortSchedule thResumeTasks)

-- | Run the initial build and the resume build, returning the resume build's environment and result.
runResume :: TestEnv -> TestT IO (SessionEnv, BuildResult)
runResume testEnv = do
  sessionEnv0 <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  extDepDbs <- liftIO (createExtDepPackageDbs sessionEnv0.tempDir extDeps)
  let sessionEnv = sessionEnv0 {extDepDbs, extDeps}
      -- One job, so that unit 3's splice runs before unit 1 is restored.
      buildSys = mkBuildSystem 1 sessionEnv

  initialResult <- liftIO do
    writeProjectSources sessionEnv.sourceDir build.initial.modules
    buildSys.runInitialBuild build.schedule
  assertNoFailures "initial" initialResult

  cachedSchedule <- liftIO $ setupResumeBuild buildSys sessionEnv build initialResult
  resumeEnv0 <- liftIO $ newResumeSessionEnv sessionEnv
  let resumeEnv = resumeEnv0 {extDepDbs, extDeps}

  -- Make sure we didn't accidentally keep the first build's state
  assert . isNothing =<< liftIO (envLoader resumeEnv.env)

  resumeResult <- liftIO $ executeResumeBuild buildSys resumeEnv build initialResult cachedSchedule
  pure (resumeEnv, resumeResult)
  where
    extDeps = Set.singleton 0

-- | The restored graph has a node for external package 0, and one for every unit an external unit's node depends on,
-- as after fresh metadata.
--
-- TODO: The resume build itself fails until the restored modules have edges to their external units, so its result is
-- not checked here.
testUnitNodes :: TestEnv -> TestT IO ()
testUnitNodes testEnv = do
  (resumeEnv, _) <- runResume testEnv
  WorkerState {make = MakeState {moduleGraphNodes}} <- liftIO (readMVar resumeEnv.env.state)
  let unitNodes = Map.fromList [(uid, deps) | UnitNode deps uid <- Map.elems moduleGraphNodes]
      missing =
        [stringToUnitId "extdep0" | not (Map.member (stringToUnitId "extdep0") unitNodes)]
        <> [dep | deps <- Map.elems unitNodes, dep <- deps, not (Map.member dep unitNodes)]
  case missing of
    [] -> pure ()
    _ -> do
      annotate ("external units without a node: " <> show (map unitIdString missing))
      failure

test_extUnitResume :: TestTree
test_extUnitResume =
  withTestEnv \ getTestEnv ->
    unitTest "restored units have nodes for their external units" do
      env <- liftIO getTestEnv
      testUnitNodes env

#else

import Test.Tasty (TestTree, testGroup)

-- | GHC 9.10's module graph has no nodes for external units.
test_extUnitResume :: TestTree
test_extUnitResume = testGroup "restored units have nodes for their external units" []

#endif
