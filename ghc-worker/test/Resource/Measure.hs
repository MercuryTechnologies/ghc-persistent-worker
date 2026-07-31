module Resource.Measure where

import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (dropMissing, traverseMissing, zipWithMatched)
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog.Internal.Property (TestT, failWith)
import System.Environment (lookupEnv)
import Test.Resource.Stats (PhaseReference (..), PhaseResult (..), phaseSummary, rtsStatsAvailable)

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

-- | Pair each measured result with its reference by name.
pairResults ::
  HasCallStack =>
  [PhaseReference] ->
  [PhaseResult] ->
  TestT IO [(PhaseReference, PhaseResult)]
pairResults refs phases =
  withFrozenCallStack do
    Map.elems <$> Map.mergeA (traverseMissing resultMissing) dropMissing (zipWithMatched matched) refMap resultMap
  where
    refMap = Map.fromList [(r.name, r) | r <- refs]
    resultMap = Map.fromList [(r.name, r) | r <- phases]
    resultMissing name _ = failWith Nothing ("References key missing in results: " ++ name)

    matched _ ref res = (ref, res)

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

assertMeasurements ::
  HasCallStack =>
  [PhaseReference] ->
  [PhaseResult] ->
  TestT IO ()
assertMeasurements refs results =
  withFrozenCallStack do
    checks <- fmap (uncurry phaseSummary) <$> pairResults refs results
    let regressions = [r | (_, Just r) <- checks]
    unless (null regressions) do
      failWith Nothing (formatReport (fst <$> checks) regressions)
