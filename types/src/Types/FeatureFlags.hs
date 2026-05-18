module Types.FeatureFlags where

data FeatureFlag =
  FeatureFixedNodesCache
  |
  FeatureFlagParser
  deriving stock (Eq, Show)

-- | Runtime feature flags that control alternative implementations.
data FeatureFlags =
  FeatureFlags {
    -- | Use fixed module graph nodes instead of calling 'summariseFile' when restoring from cache.
    fixedNodesCache :: Bool,
    -- | Use the custom flatparse-based flag parser instead of GHC's 'parseDynamicFlags'.
    flagParser :: Bool
  }
  deriving stock (Eq, Show)

defaultFeatureFlags :: FeatureFlags
defaultFeatureFlags =
  FeatureFlags {
    fixedNodesCache = True,
    flagParser = False
  }
