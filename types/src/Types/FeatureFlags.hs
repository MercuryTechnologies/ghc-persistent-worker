module Types.FeatureFlags where

data FeatureFlag
  deriving stock (Eq, Show)

-- | Runtime feature flags that control alternative implementations.
data FeatureFlags =
  FeatureFlags {
  }
  deriving stock (Eq, Show)

defaultFeatureFlags :: FeatureFlags
defaultFeatureFlags =
  FeatureFlags {
  }
