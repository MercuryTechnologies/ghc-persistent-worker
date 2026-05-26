{-# LANGUAGE DeriveAnyClass #-}

module Types.CachedDeps where

import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  FromJSONKeyFunction (..),
  ToJSON (..),
  ToJSONKey (..),
  withArray,
  withObject,
  withText,
  (.:),
  (.:?),
  )
import Data.Coerce (Coercible, coerce)
import Data.Foldable (fold, toList)
import Data.Functor.Contravariant (contramap)
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC (ModuleName (..))
import GHC.Data.FastString (FastString, bytesFS, mkFastString, mkFastStringByteString)
import GHC.Generics (Generic)
import GHC.Unit (UnitId (..))
import System.OsPath.Extra (OsPath, toOsPath)

newtype JsonFs a =
  JsonFs { raw :: a }
  deriving stock (Eq, Ord)

instance Coercible a FastString => Show (JsonFs a) where
  show = show @FastString . coerce

jsonFsFromText ::
  Coercible a FastString =>
  Text ->
  JsonFs a
jsonFsFromText =
  JsonFs . coerce . mkFastStringByteString . encodeUtf8

jsonFsToText ::
  Coercible a FastString =>
  JsonFs a ->
  Text
jsonFsToText =
  decodeUtf8 . bytesFS . coerce

jsonFsFromString ::
  Coercible a FastString =>
  String ->
  JsonFs a
jsonFsFromString =
  JsonFs . coerce . mkFastString

instance Coercible a FastString => IsString (JsonFs a) where
  fromString = jsonFsFromString

instance Coercible a FastString => FromJSON (JsonFs a) where
  parseJSON = withText "JsonFs" (pure . jsonFsFromText)

instance Coercible a FastString => ToJSON (JsonFs a) where
  toJSON = toJSON . jsonFsToText

instance Coercible a FastString => ToJSONKey (JsonFs a) where
  toJSONKey = contramap jsonFsToText toJSONKey

instance Coercible a FastString => FromJSONKey (JsonFs a) where
  fromJSONKey =
    FromJSONKeyText jsonFsFromText

  fromJSONKeyList =
    FromJSONKeyValue (withArray "JsonFs" (traverse (withText "JsonFs" (pure . jsonFsFromText)) . toList))

-- | A cross-package dependency within the project provided by Buck.
data CachedDep =
  CachedDep {
    name :: JsonFs ModuleName,
    package :: JsonFs UnitId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The data Buck provides in order to restore the state when recompiling after restart.
newtype CachedDeps =
  CachedDeps [CachedDep]
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data CachedPackageDep =
  CachedPackageDep {
     id :: JsonFs UnitId,
     modules :: [JsonFs ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- Compat instance for the legacy schema that uses @name@.
instance FromJSON CachedPackageDep where
  parseJSON =
    withObject "CachedPackageDep" \ o -> do
      mb_id <- o .:? "id"
      mb_name <- o .:? "name"
      modules <- o .: "modules"
      case (mb_id, mb_name) of
        (Just uid, _) -> pure CachedPackageDep {id = uid, modules}
        (Nothing, Just name) -> pure CachedPackageDep {id = name, modules}
        (Nothing, Nothing) -> fail "Neither 'id' nor 'name'"

data CachedModule =
  CachedModule {
    source :: OsPath,
    modules :: [JsonFs ModuleName],
    packages :: [CachedPackageDep],
    flags :: [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- Compat instance for the legacy schema that uses @sources@.
instance FromJSON CachedModule where
  parseJSON =
    withObject "CachedModule" \ o -> do
      mb_source <- o .:? "source"
      mb_sources <- o .:? "sources"
      modules <- o .: "modules"
      packages <- o .: "packages"
      flags <- fold <$> o .:? "flags"
      case (mb_source, mb_sources) of
        (Just source, _) -> pure CachedModule {source=toOsPath source,..}
        (Nothing, Just (source : _)) -> pure CachedModule {source=toOsPath source,..}
        (Nothing, Just _) -> fail "No 'source' and 'sources' does not contain exactly one element"
        (Nothing, Nothing) -> fail "Neither 'source' nor 'sources'"

data CachedUnit =
  CachedUnit {
    build_plan :: Maybe (Map (JsonFs ModuleName) CachedModule),
    cache :: Maybe (Map (JsonFs ModuleName) CachedModule),
    unit_args :: Maybe OsPath,
    unit_buck_args :: Maybe OsPath,
    dep_units :: Maybe OsPath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CachedBuildPlan =
  CachedBuildPlan {
    name :: JsonFs UnitId,
    build_plan :: OsPath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Sorted in dependency order by Buck.
newtype CachedBuildPlans =
  CachedBuildPlans [CachedBuildPlan]
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)
