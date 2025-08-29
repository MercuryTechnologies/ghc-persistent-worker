module Types.Args where

import Data.Map (Map)
import Data.Map.Strict ((!?))
import Types.CachedDeps (CachedDeps)
import GHC (ModuleName)
import GHC.Unit (UnitId)
import GHC.Utils.Outputable (showPprUnsafe)

newtype TargetId = TargetId {string :: String}
  deriving newtype (Show, Eq, Ord)

newtype UnitName =
  UnitName UnitId
  deriving stock (Eq)

instance Show UnitName where
  show (UnitName uid) = showPprUnsafe uid

data Args =
  Args {
    topdir :: Maybe String,
    workerTargetId :: Maybe TargetId,
    binPath :: [String],
    tempDir :: Maybe String,
    unit :: Maybe UnitName,
    moduleName :: Maybe ModuleName,
    ghcOptions :: [String],
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
    unit = Nothing,
    moduleName = Nothing,
    ghcOptions = [],
    cachedDeps = Nothing
  }
