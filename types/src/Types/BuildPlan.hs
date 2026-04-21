{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.BuildPlan where

import Data.Aeson (ToJSON (..), ToJSONKey, Value (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.String (IsString)
import GHC (HscEnv, ModSummary, moduleNameString, ms_mod_name)
import GHC.Data.FastString (unpackFS)
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import GHC.Types.Unique.Map (UniqMap)
import GHC.Unit (GenericUnitInfo (..), PackageName (..), UnitInfo)
import GHC.Unit.Module (IsBootInterface (..), ModuleName (..), UnitId (..))
import GHC.Unit.Module.Graph (ModuleGraph, NodeKey)
import GHC.Unit.Module.ModSummary (isBootSummary)
import GHC.Utils.Outputable (Outputable (..), text)
import Types.CachedDeps (CachedModule, JsonFs (..))

data Dep =
  Dep {
    name :: ModuleName,
    unit :: UnitId
  }

data PackageDep =
  PackageDep {
    id :: JsonFs UnitId,
    name :: PackageKey,
    modules :: [JsonFs ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

newtype Preprocessor =
  Preprocessor (Maybe String)
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

-- | The specific representation of a package name used by Buck.
-- If a unit is a Cabal sublibrary, the key will consist of both names, formatted @package:library@.
-- For home units, this is simply the name, while toolchain deps may include the Cabal hash suffix.
newtype PackageKey =
  PackageKey String
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSON, ToJSONKey, IsString, Semigroup, Monoid)

packageKey :: UnitInfo -> PackageKey
packageKey unit =
  PackageKey (maybe name withLibName (unitComponentName unit))
  where
    PackageName nameFS = unitPackageName unit
    name = unpackFS nameFS
    withLibName (PackageName c) = name ++ ":" ++ unpackFS c

-- | All data required to compute the individual build plan fields for one home module.
data BuildPlanModule =
  BuildPlanModule {
    source :: FilePath,
    -- Legacy field
    sources :: [FilePath],
    boot :: Bool,
    modules :: [(ModuleKey, JsonFs ModuleName)],
    modulesBoot :: [(ModuleKey, JsonFs ModuleName)],
    packages :: [PackageDep],
    options :: Set String,
    thEnabled :: Bool,
    preprocessor :: Preprocessor
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | The specific representation of a module name used by Buck.
-- Boot modules are marked by a @-boot@ suffix, e.g. @Project.App-boot@.
newtype ModuleKey =
  ModuleKey String
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSON, ToJSONKey, IsString, Semigroup, Monoid)

instance Outputable ModuleKey where
  ppr (ModuleKey k) = text k

moduleKey :: ModuleName -> ModuleKey
moduleKey name =
  ModuleKey (moduleNameString name)

moduleKeyBoot :: ModuleName -> ModuleKey
moduleKeyBoot name =
  ModuleKey (moduleNameString name) <> "-boot"

summaryModuleKey :: ModSummary -> ModuleKey
summaryModuleKey summary
  | IsBoot <- isBootSummary summary
  = moduleKeyBoot name
  | otherwise
  = moduleKey name
  where
    name = ms_mod_name summary

-- | Maps modules in the home unit to modules from other packages it depends on, grouped by the package name.
-- These can be other home units or toolchain packages.
newtype PackageDeps =
  PackageDeps { modules :: Map ModuleKey (Map PackageKey [JsonFs ModuleName]) }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

instance IsList PackageDeps where
  type Item PackageDeps = (ModuleKey, (Map PackageKey [JsonFs ModuleName]))
  fromList = PackageDeps . fromList
  toList = toList . (.modules)

instance Semigroup PackageDeps where
  PackageDeps l <> PackageDeps r = PackageDeps (Map.unionWith (<>) l r)

-- | The JSON protocol for communication with Buck.
-- This is the primary output of a metadata step, used by Buck to compute edges of the build graph, in order to
-- invalidate build artifacts when local modules or external dependencies change.
--
-- See 'Types.Args.BuildPlanField' for an explanation of the fields.
data BuildPlanSchema =
  BuildPlanSchema {
    exposed_modules :: Maybe [ModuleKey],
    module_graph :: Maybe (Map ModuleKey [ModuleKey]),
    package_deps :: Maybe PackageDeps,
    project_deps :: Maybe PackageDeps,
    toolchain_deps :: Maybe PackageDeps,
    th_modules :: Maybe [ModuleKey],
    cache :: Maybe (Map ModuleKey CachedModule)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data BuildPlanJson =
  BuildPlanJson {
    legacy :: Maybe (Map ModuleKey BuildPlanModule),
    schema :: BuildPlanSchema
  }
  deriving stock (Eq, Show)

instance ToJSON BuildPlanJson where
  toJSON BuildPlanJson {..} =
    case toJSON schema of
      Object values
        | Just legacyData <- legacy
        , Object legacyValues <- toJSON legacyData
        -> Object (values <> legacyValues)
      value -> value

-- | The final result of build plan generation.
data BuildPlan =
  BuildPlan {
    -- | The module graph is stored in the worker state.
    graph :: ModuleGraph,
    -- | The payload is written to a JSON file for Buck.
    json :: BuildPlanJson
  }

-- | Precomputed data used by all module entries.
data BuildPlanEnv =
  BuildPlanEnv {
    hsc_env :: HscEnv,

    -- | Preprocessor specified as a CLI arg, rather than in a module header.
    globalPreprocessor :: Preprocessor,

    -- | Canonical unit names that include Cabal sublibrary suffixes.
    unitNames :: UniqMap UnitId PackageKey,

    homeUnitIds :: Set UnitId,

    -- | Memory efficiency map for dependencies within the home unit.
    -- This avoids converting @ModuleName@ to @ModuleKey@ repeatedly.
    homeModules :: Map NodeKey (Either (ModuleKey, JsonFs ModuleName) (ModuleKey, JsonFs ModuleName)),

    -- | Memory efficiency map for dependencies on other units.
    -- The alternative would be to construct 'Dep' in 'modulePackageDeps' for each entry.
    -- While that data type is very small, it would result in thousands of redundant constructors, which consumes quite
    -- a bit of memory.
    packageModules :: Map NodeKey Dep
  }
