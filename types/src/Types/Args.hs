module Types.Args where

import Data.Map (Map)
import Data.Map.Strict ((!?))

newtype TargetId = TargetId {string :: String}
  deriving newtype (Show, Eq, Ord)

data Args =
  Args {
    topdir :: Maybe String,
    workerTargetId :: Maybe TargetId,
    env :: Map String String,
    binPath :: [String],
    tempDir :: Maybe String,
    ghcPath :: Maybe String,
    ghcOptions :: [String]
  }
  deriving stock (Eq, Show)

emptyArgs :: Map String String -> Args
emptyArgs env =
  Args {
    topdir = Nothing,
    workerTargetId = Nothing,
    env,
    binPath = [],
    tempDir = env !? "TMPDIR",
    ghcPath = Nothing,
    ghcOptions = []
  }
