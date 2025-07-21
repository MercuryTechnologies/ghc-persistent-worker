{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Types.CachedDeps where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), withArray, withText)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC (ModuleName (..))
import GHC.Data.FastString (FastString, mkFastStringByteString)
import GHC.Generics (Generic)
import GHC.Unit (UnitId (..))
import GHC.Utils.Outputable (showPprUnsafe)

newtype JsonFs a =
  JsonFs a
  deriving stock (Eq, Ord)

instance Coercible a FastString => Show (JsonFs a) where
  show = showPprUnsafe @FastString . coerce

jsonFsFromText ::
  Coercible a FastString =>
  Text ->
  JsonFs a
jsonFsFromText =
  JsonFs . coerce . mkFastStringByteString . encodeUtf8

instance Coercible a FastString => FromJSON (JsonFs a) where
  parseJSON = withText "JsonFs" (pure . jsonFsFromText)

instance Coercible a FastString => FromJSONKey (JsonFs a) where
  fromJSONKey =
    FromJSONKeyText jsonFsFromText

  fromJSONKeyList =
    FromJSONKeyValue (withArray "JsonFs" (traverse (withText "JsonFs" (pure . jsonFsFromText)) . toList))

-- | A cross-package dependency within the project provided by Buck.
data CachedProjectDep =
  CachedProjectDep {
    name :: JsonFs ModuleName,
    package :: JsonFs UnitId,
    interfaces :: NonEmpty FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | The data Buck provides in order to restore the state when recompiling after restart.
data CachedDeps =
  CachedDeps {
    home_unit :: Map (JsonFs ModuleName) (NonEmpty FilePath),
    project :: [CachedProjectDep]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data CachedBuildPlan =
  CachedBuildPlan {
    name :: JsonFs UnitId,
    build_plan :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype CachedBuildPlans =
  CachedBuildPlans (Map (JsonFs UnitId) FilePath)
  deriving stock (Eq, Show)

instance FromJSON CachedBuildPlans where
  parseJSON =
    withArray "CachedBuildPlans" (fmap (CachedBuildPlans . Map.fromList . toList . fmap toTuple) . traverse parseJSON)
    where
      toTuple CachedBuildPlan {..} = (name, build_plan)
