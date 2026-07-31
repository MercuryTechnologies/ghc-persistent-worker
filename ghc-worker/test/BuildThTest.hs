module BuildThTest where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import GHC.Data.FastString (FastString)
import Hedgehog (TestT, assert, (===))
import Test.BuildSystem (mkBuildSystem)
import Test.Bytecode (enableLazyByteCode, envLoader, loadedBcos)
import Test.Data.BuildSystem (BuildSystem (..))
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
import Test.Gen.ProjectBuild (metaTask, moduleTask, sortSchedule)
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)

module_1_1 :: BuildModule
module_1_1 =
  BuildModule {
    key = ModuleKey {unit = 1, number = 1, errorVariant = Nothing},
    deps = mempty,
    th = False,
    bindings = 1,
    extDeps = mempty
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
    deps = Set.fromList [module_1_1.key, module_2_1.key],
    th = True,
    bindings = 1,
    extDeps = mempty
  }

unit1 :: GenUnit BuildModule
unit1 = GenUnit {key = 1, depUnits = mempty, modules = [module_1_1]}

unit2 :: GenUnit BuildModule
unit2 = GenUnit {key = 2, depUnits = Set.singleton 1, modules = [module_2_1, module_2_2]}

source :: BuildModule -> ModuleSource
source BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

initial :: InitialProject
initial =
  InitialProject {
    modules = mods,
    modulesSuccess = mods,
    modulesError = mempty,
    unitCount = 2,
    moduleCount = 3
  }
  where
    mods = Map.fromList [(m.key, source m) | m <- [module_1_1, module_2_1, module_2_2]]

-- | Initial build: compile A and B only, no compile task for C.
initialTasks :: [BuildTask]
initialTasks =
  [
    metaTask unit1 unit1.depUnits,
    moduleTask unit1 module_1_1,
    metaTask unit2 unit2.depUnits,
    moduleTask unit2 module_2_1
  ]

-- | Resume build: same tasks plus the compile task for C.
thResumeTasks :: [BuildTask]
thResumeTasks = initialTasks ++ [moduleTask unit2 module_2_2]

-- | The initial build compiles A and B, the resume build compiles only the TH module C, restoring A and B from cache.
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
    allKeys = scheduleKeys resumeSchedule',
    incrementalBuildPlan = False
  }
  where
    resumeSchedule' = fst (sortSchedule thResumeTasks)

targetBcos :: [(FastString, FastString, [String])]
targetBcos =
  [
    (
      "unit1",
      "Unit1Module1",
      ["value_1_1"]
    ),
    (
      "unit2",
      "Unit2Module1",
      ["value_2_1"]
    )
  ]

-- | This test builds a module with a splice that has deps on one home unit module and one in another unit, then asserts
-- that there are BCOs for those deps in the loader state.
testBuildTh :: TestEnv -> TestT IO ()
testBuildTh testEnv = do
  sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  let buildSys = mkBuildSystem 6 False sessionEnv

  initialResult <- liftIO do
    writeProjectSources sessionEnv.sourceDir build.initial.modules
    buildSys.runInitialBuild build.schedule
  assertNoFailures "initial" initialResult

  cachedSchedule <- liftIO $ setupResumeBuild buildSys sessionEnv build initialResult
  resumeEnv <- liftIO $ newResumeSessionEnv sessionEnv

  -- Make sure we didn't accidentally keep the first build's state
  assert . isNothing =<< liftIO (envLoader resumeEnv.env)

  resumeResult <- liftIO $ executeResumeBuild buildSys resumeEnv build initialResult cachedSchedule
  assertNoFailures "resume" resumeResult

  bcos <- loadedBcos resumeEnv.env
  targetBcos === bcos

test_buildTh :: TestTree
test_buildTh =
  withTestEnv \ getTestEnv ->
    unitTest "resume build with a splice" do
      env <- liftIO getTestEnv
      testBuildTh env
