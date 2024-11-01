module Internal.Args where

import Data.Map (Map)
import Data.Map.Strict ((!?))

data Args =
  Args {
    topdir :: Maybe String,
    workerTargetId :: Maybe String,
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
