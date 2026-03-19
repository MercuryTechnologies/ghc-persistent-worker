module ProjectBuildTest where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (PropertyT, forAllWith, property, withTests)
import Test.BuildSystem (mkBuildSystem)
import Test.Data.BuildSystem (BuildResult (..), BuildSystem (..))
import Test.Data.Env (SessionEnv (..), TestConfig (..), TestEnv, withTestConfig)
import Test.Data.Project (InitialProject (..))
import Test.Data.ProjectBuild (ProjectBuild (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Gen.ProjectBuild (genProjectBuild)
import Test.ProjectBuild.Classify (classifyFirstBuild, classifyProject, classifyResume)
import Test.ProjectBuild.Property (annotateRebuildPlan, assertBuildResult, showProjectBuild)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Source (writeProjectSources, toModuleSourceMap)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

-- | Generate a test case and create temp directories, state, and handlers.
setup :: TestConfig -> TestEnv -> PropertyT IO (ProjectBuild, SessionEnv, BuildSystem)
setup conf env = do
  project <- forAllWith showProjectBuild (genProjectBuild conf)
  sessionEnv <- liftIO (newSessionEnv env)
  pure (project, sessionEnv, mkBuildSystem conf.maxConcurrentJobs sessionEnv)

-- | Write source files to the temp dir and run the initial build.
runInitialBuild :: ProjectBuild -> BuildSystem -> SessionEnv -> PropertyT IO BuildResult
runInitialBuild project buildSys sessionEnv = do
  result <- liftIO do
    writeProjectSources sessionEnv.sourceDir (toModuleSourceMap project.initial.modules)
    buildSys.runInitialBuild project.schedule
  classifyProject project
  classifyFirstBuild result
  assertBuildResult sessionEnv.tempDir project result
  pure result

-- | Update source files, write Buck cache, and run the resume build.
runResumeBuild :: ProjectBuild -> BuildSystem -> SessionEnv -> BuildResult -> PropertyT IO ()
runResumeBuild build buildSys initialEnv initialResult = do
  cachedSchedule <- liftIO $ setupResumeBuild buildSys initialEnv build initialResult
  resumeEnv <- liftIO $ newResumeSessionEnv initialEnv
  resumeResult <- liftIO $ executeResumeBuild buildSys resumeEnv build initialResult cachedSchedule
  classifyResume build initialResult
  annotateRebuildPlan build.resumePlan
  assertBuildResult resumeEnv.tempDir build resumeResult

prop_projectBuild :: TestConfig -> TestEnv -> PropertyT IO ()
prop_projectBuild conf env = do
  (project, sessionEnv, buildSys) <- setup conf env
  initialResult <- runInitialBuild project buildSys sessionEnv
  runResumeBuild project buildSys sessionEnv initialResult

-- | The options can be overridden on the command line:
-- > cabal test ghc-worker --test-options="--max-units 10 --max-modules-per-unit 8 --max-concurrent-jobs 4 --hedgehog-tests 200"
test_projectBuild :: TestTree
test_projectBuild =
  withTestEnv \ getTestEnv ->
    withTestConfig \ conf ->
      testProperty "multi-unit project build" $ withTests 100 $ property do
        env <- liftIO getTestEnv
        prop_projectBuild conf env
