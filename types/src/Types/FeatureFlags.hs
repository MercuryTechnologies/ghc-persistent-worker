{-# LANGUAGE CPP #-}

module Types.FeatureFlags where

import Data.Char (isDigit)
import Data.Either.Extra (maybeToEither)
import Text.Read (readMaybe)

data FeatureFlag =
  FeatureFixedNodesCache
  |
  FeatureFlagParser
  |
  FeatureConcurrentInitUnits
  |
  FeatureInstrument
  |
  FeatureLazyByteCode
  deriving stock (Eq, Show)

-- | Runtime feature flags that control alternative implementations.
data FeatureFlags =
  FeatureFlags {
    -- | Use fixed module graph nodes instead of calling 'summariseFile' when restoring from cache.
    fixedNodesCache :: Bool,
    -- | Use the custom flatparse-based flag parser instead of GHC's 'parseDynamicFlags'.
    flagParser :: Bool,
    -- | When restoring units from cache, perform as much work as possible concurrently.
    concurrentInitUnits :: Bool,
    -- | Integrated with accompanying monitoring instrument app
    instrument :: Bool,
    -- | Load bytecode on demand when linking splices or evaluating tests.
    lazyByteCode :: Bool,
    -- | Use incremental update of ModuleGraph.
    useIncrModGraph :: Bool,
    -- | Limit the number of BCOs that may reside in the loader state.
    --
    -- When set and 'lazyByteCode' is enabled, the least recently used entries are unloaded at the end of each compile
    -- job once the tracked total exceeds this limit. 'Nothing' disables unloading entirely.
    lazyByteCodeCacheLimit :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | Parse a @--max-bytecode@ CLI argument: a decimal number followed by an optional @k@, @M@ or @G@ suffix.
parseByteSize :: String -> Either String Int
parseByteSize s = do
  number <- maybeToEither invalid (readMaybe digits)
  factor <- parseFactor suffix
  pure (number * factor)
  where
    invalid = "Invalid --max-bytecode value: " ++ s

    parseFactor = \case
      "" -> Right 1
      "k" -> Right 1000
      "M" -> Right 1000000
      "G" -> Right 1000000000
      _ -> Left ("Invalid --max-bytecode suffix (expected k, M or G): " ++ suffix)

    (digits, suffix) = span isDigit s

defaultFeatureFlags :: FeatureFlags
defaultFeatureFlags =
  FeatureFlags {
    fixedNodesCache = True,
    flagParser = False,
    concurrentInitUnits = True,
    instrument = False,
#if defined(LINKABLES)
    lazyByteCode = True,
#else
    lazyByteCode = False,
#endif
    lazyByteCodeCacheLimit = Nothing,
    useIncrModGraph = True
  }
