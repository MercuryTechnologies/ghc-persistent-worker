-- | Allocation tracking via 'GHC.Stats.getRTSStats' snapshots.
module Test.Resource.Stats where

import Data.Fixed (E2, Fixed, showFixed)
import Data.Int (Int64)
import Data.Word (Word64)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.IO.Error (catchIOError)
import System.Mem (performBlockingMajorGC)

-- | A snapshot of cumulative allocated bytes at a point in time.
newtype Snapshot =
  Snapshot { allocatedBytes :: Word64 }
  deriving stock (Eq, Show)

-- | Take a snapshot of the current cumulative allocation counter.
snapshot :: IO Snapshot
snapshot =
  Snapshot . (.allocated_bytes) <$> getRTSStats

-- | Check whether RTS stats are available.
-- Forces a GC first (since @allocated_bytes@ starts at zero before any GC), then verifies the counter is nonzero.
-- Returns 'False' when running without the @-T@ RTS option (e.g. in GHCi), where @getRTSStats@ throws.
rtsStatsAvailable :: IO Bool
rtsStatsAvailable = do
  performBlockingMajorGC
  fmap ((/= 0) . (.allocatedBytes)) snapshot `catchIOError` \ _ -> pure False

-- | Compute the allocation delta between two snapshots.
delta :: Snapshot -> Snapshot -> Int64
delta before after =
  fromIntegral after.allocatedBytes - fromIntegral before.allocatedBytes

-- | Named allocation measurement for a phase.
data PhaseResult =
  PhaseResult {
    name :: String,
    allocatedBytes :: Int64
  }
  deriving stock (Eq, Show)

-- | Execute an action and measure the allocations it performs.
--
-- Forces a major GC both before and after the action to ensure that dead objects from the previous phase don't inflate
-- the current measurement, and that live objects from the current phase are accounted for.
measurePhase :: String -> IO a -> IO (a, PhaseResult)
measurePhase name action = do
  performBlockingMajorGC
  before <- snapshot
  result <- action
  performBlockingMajorGC
  after <- snapshot
  pure (result, PhaseResult {name, allocatedBytes = delta before after})

-- | Reference allocation value with tolerance.
data PhaseReference =
  PhaseReference {
    name :: String,
    allocatedMB :: Double,
    tolerancePercent :: Int
  }
  deriving stock (Eq, Show)

-- | Result of comparing a measured value against a reference.
data Comparison =
  WithinTolerance
  |
  Regression {
    measured :: Int64,
    referenceMB :: Double,
    deviationPercent :: Double
  }
  deriving stock (Eq, Show)

-- | Convert MB to bytes.
mbToBytes :: Double -> Int64
mbToBytes mb = round (mb * 1_000_000)

-- | Compute the deviation percentage of a measured value from a reference.
deviation :: PhaseReference -> PhaseResult -> Double
deviation ref result =
  100.0 * fromIntegral (result.allocatedBytes - refBytes) / fromIntegral refBytes
  where
    refBytes = mbToBytes ref.allocatedMB

-- | Compare a measured allocation against a reference value.
comparePhase :: PhaseReference -> PhaseResult -> Comparison
comparePhase ref result
  | abs deviationPercent <= fromIntegral ref.tolerancePercent
  = WithinTolerance
  | otherwise
  = Regression {
    measured = result.allocatedBytes,
    referenceMB = ref.allocatedMB,
    deviationPercent
  }
  where
    deviationPercent = deviation ref result

-- | Format a reference value in MB for display.
showMB :: Double -> String
showMB mb = showFixed True (realToFrac mb :: Fixed E2) ++ " MB"

-- | SI suffixes for byte counts, ordered from largest to smallest.
siSuffixes :: [(Int64, String)]
siSuffixes =
  [(1_000_000_000, " GB"), (1_000_000, " MB"), (1_000, " KB")]

-- | Format bytes with SI suffixes, showing up to 2 decimal digits.
showBytes :: Int64 -> String
showBytes n =
  case dropWhile (\ (threshold, _) -> abs n < threshold) siSuffixes of
    (divisor, suffix) : _ -> showScaled divisor ++ suffix
    [] -> show n ++ " B"
  where
    showScaled divisor = showFixed True (fromIntegral n / fromIntegral divisor :: Fixed E2)

-- | Format a phase result for display.
showPhaseResult :: PhaseResult -> String
showPhaseResult PhaseResult {name, allocatedBytes} =
  name ++ ": " ++ showBytes allocatedBytes

-- | Format a deviation percentage with up to 2 decimal digits.
showDeviation :: Double -> String
showDeviation d =
  showFixed True (realToFrac d :: Fixed E2) ++ "%"

-- | Format a regression for display.
showRegression :: PhaseReference -> PhaseResult -> Double -> String
showRegression ref result d =
  ref.name ++ ": " ++ showBytes result.allocatedBytes
  ++ " (reference: " ++ showMB ref.allocatedMB
  ++ ", deviation: " ++ showDeviation (abs d) ++ ")"

-- | Format a display line for one phase showing measured and reference values.
showPhaseLine :: PhaseReference -> PhaseResult -> String
showPhaseLine ref result =
  showPhaseResult result ++ " (reference: " ++ showMB ref.allocatedMB ++ ")"

-- | Summarize one phase as a display line with deviation, and optionally a regression detail.
phaseSummary :: PhaseReference -> PhaseResult -> (String, Maybe String)
phaseSummary ref result =
  case comparePhase ref result of
    WithinTolerance -> (summary, Nothing)
    Regression {} -> (summary, Just (showRegression ref result d))
  where
    summary = showPhaseLine ref result ++ " [" ++ showDeviation (abs d) ++ "]"
    d = deviation ref result
