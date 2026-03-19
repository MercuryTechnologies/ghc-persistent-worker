-- | Resource consumption regression test.
--
-- Compiles a static 3-unit × 3-module project and measures allocations per build phase, comparing against reference
-- values.
module ResourceTest where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Set as Set
import Hedgehog.Internal.Property (failWith)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Test.Data.Env (TestEnv)
import Test.Data.Project (BuildModule, GenUnit)
import Test.Env (newSessionEnv, withTestEnv)
import Test.Resource.Build (runResourceBuild)
import Test.Resource.Project (mkUnit)
import Test.Resource.Stats (PhaseReference (..), PhaseResult (..), phaseSummary, rtsStatsAvailable)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)

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

-- | Pair each measured result with its reference by name.
pairResults :: [PhaseResult] -> [(PhaseReference, PhaseResult)]
pairResults phases =
  [(ref, result) | ref <- referenceData, Just result <- [lookup ref.name resultMap]]
  where
    resultMap = [(r.name, r) | r <- phases]

-- | Format the failure report with regression details and all phase deviations.
formatReport :: [String] -> [String] -> String
formatReport summaries regressions =
  intercalate "\n" $
  ["Allocation regressions detected:"]
  ++
  indent regressions
  ++
  ["", "All phases:"]
  ++
  indent summaries
  where
    indent = fmap ("  " ++)

-- | Check whether the environment supports running the resource test.
-- Requires both RTS stats (compiled with @-T@) and the @resource_test_ext_deps@ env var
-- (set by the @test-ext-deps@ devshell), which ensures controlled build conditions.
checkEnvironment :: IO (Maybe String)
checkEnvironment =
  rtsStatsAvailable >>= \case
    False -> pure (Just "RTS stats not available (compiled without -T?)")
    True -> lookupEnv "resource_test_ext_deps" <&> \case
      Just _ -> Nothing
      Nothing -> Just "resource_test_ext_deps not set (use the test-ext-deps devshell)"

test_memory :: IO TestEnv -> TestTree
test_memory getEnv =
  unitTest "allocations" do
    maybe run skip =<< liftIO checkEnvironment
  where
    run = do
      env <- liftIO (newSessionEnv =<< getEnv)
      (_, phases) <- liftIO (runResourceBuild allUnits env)
      let checks = uncurry phaseSummary <$> pairResults phases
          regressions = [r | (_, Just r) <- checks]
      unless (null regressions) do
        failWith Nothing (formatReport (fst <$> checks) regressions)

    skip reason =
      liftIO $ hPutStrLn stderr $ "Skipping resource test: " ++ reason

test_resources :: TestTree
test_resources =
  withTestEnv \ getEnv ->
    testGroup "resources" [
      test_memory getEnv
    ]
