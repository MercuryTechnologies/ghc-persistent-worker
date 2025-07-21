module Types.Args where

import Data.Map (Map)
import Data.Map.Strict ((!?))
import Types.CachedDeps (CachedBuildPlans, CachedDeps)

newtype TargetId = TargetId {string :: String}
  deriving newtype (Show, Eq, Ord)

data Args =
  Args {
    topdir :: Maybe String,
    workerTargetId :: Maybe TargetId,
    binPath :: [String],
    tempDir :: Maybe String,
    ghcPath :: Maybe String,
    ghcOptions :: [String],
    cachedBuildPlans :: Maybe CachedBuildPlans,
    cachedDeps :: Maybe CachedDeps
  }
  deriving stock (Eq, Show)

emptyArgs :: Map String String -> Args
emptyArgs env =
  Args {
    topdir = Nothing,
    workerTargetId = Nothing,
    binPath = [],
    tempDir = env !? "TMPDIR",
    ghcPath = Nothing,
    ghcOptions = [],
    cachedBuildPlans = Nothing,
    cachedDeps = Nothing
  }
