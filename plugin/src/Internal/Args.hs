module Internal.Args where

import Data.Map (Map)
import Data.Map.Strict ((!?))

data Args =
  Args {
    topdir :: Maybe String,
    binPath :: [String],
    tempDir :: Maybe String,
    ghcOptions :: [String]
  }
  deriving stock (Eq, Show)

emptyArgs :: Map String String -> Args
emptyArgs env =
  Args {
    topdir = Nothing,
    binPath = [],
    tempDir = env !? "TMPDIR",
    ghcOptions = []
  }
