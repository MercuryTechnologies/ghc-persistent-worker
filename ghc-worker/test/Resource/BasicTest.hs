-- | Resource consumption regression test.
--
-- Compiles a static 3-unit × 3-module project and measures allocations per build phase, comparing against reference
-- values.
module Resource.BasicTest where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Resource.LazyByteCodeTest (test_memory_lazyByteCode)
import Resource.Measure (assertMeasurements, checkEnvironment)
import System.IO (hPutStrLn, stderr)
import Test.Data.Project (BuildModule (..), GenUnit (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Resource.Build (runResourceBuild)
import Test.Resource.Project (mkUnit)
import Test.Resource.Stats (PhaseReference (..))
import Test.Run (unitTest)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup)

-- | All units in dependency order for the resource test.
-- Three units with TH enabled, 20 bindings per module, and 2 external dependency packages.
allUnits :: [GenUnit BuildModule]
allUnits =
  [
    mkUnit True 20 extDeps 0 Set.empty,
    mkUnit True 20 extDeps 1 (Set.singleton 0),
    mkUnit True 20 extDeps 2 (Set.fromList [0, 1])
  ]
  where
    extDeps = Set.fromList [0, 1]

-- | Reference allocation values with tolerance, calibrated from a baseline run.
referenceData :: [PhaseReference]
referenceData =
  [
    PhaseReference {name, allocatedMB, tolerancePercent = 5}
    | (name, allocatedMB) <- [
      ("unit_0_metadata", 15.27),
      ("unit_0_compile_0", 105.14),
      ("unit_0_compile_1", 51.48),
      ("unit_0_compile_2", 63.59),
      ("unit_1_metadata", 7.54),
      ("unit_1_compile_0", 75.81),
      ("unit_1_compile_1", 87.99),
      ("unit_1_compile_2", 100.30),
      ("unit_2_metadata", 8.20),
      ("unit_2_compile_0", 112.66),
      ("unit_2_compile_1", 125.27),
      ("unit_2_compile_2", 138.11)
    ]
  ]

test_memory_basic :: TestTree
test_memory_basic =
  withTestEnv \ getEnv ->
    unitTest "simple build allocations" do
      maybe (run getEnv) skip =<< liftIO checkEnvironment
  where
    run getEnv = do
      env <- liftIO (newSessionEnv =<< getEnv)
      (_, phases) <- liftIO (runResourceBuild allUnits env)
      assertMeasurements referenceData phases

    skip reason =
      liftIO $ hPutStrLn stderr $ "Skipping resource test: " ++ reason

test_resources :: TestTree
test_resources =
    dependentTestGroup "resources" AllFinish [
      test_memory_basic,
      test_memory_lazyByteCode
    ]
