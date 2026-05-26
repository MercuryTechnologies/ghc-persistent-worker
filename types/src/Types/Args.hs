{-# LANGUAGE NoFieldSelectors #-}

module Types.Args where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import GHC.Paths (libdir)
import GHC.Unit (UnitId)
import GHC.Utils.Outputable (showPprUnsafe)
import System.OsPath.Extra (OsPath, toOsPath)
import Types.BuildPlan.Incremental (BuckHashesPath, BuildPlanPath, IncrementalStatePath)
import Types.CachedDeps (CachedBuildPlans, CachedDeps)
import Types.FeatureFlags (FeatureFlags, defaultFeatureFlags)
import Types.Target (ModuleTarget)
import Types.BuildPlan (ModuleKey)

newtype TargetId = TargetId {string :: String}
  deriving newtype (Show, Eq, Ord)

newtype UnitName =
  UnitName UnitId
  deriving stock (Eq)

instance Show UnitName where
  show (UnitName uid) = showPprUnsafe uid

-- | Fields in the build plan JSON.
-- These are used by Buck for separate purposes and can be enabled selectively via the CLI option @--fields@.
data BuildPlanField =
  -- | A list of the unit's modules available for import.
  FieldExposedModules
  |
  -- | A mapping from module names to dependency module names, representing the unit's local dep graph.
  -- Keys may have the suffix @-boot@ to distinguish boot modules.
  FieldModuleGraph
  |
  -- | A nested mapping from modules to units to dependency module names, representing the unit's package dependencies.
  -- These include both other units in the project and toolchain dependencies.
  FieldPackageDeps
  |
  -- | A nested mapping from modules to units to dependency module names, representing the unit's dependencies on other
  -- home units.
  -- This is not used at the moment, but might help improve Buck performance at some point.
  FieldProjectDeps
  |
  -- | A nested mapping from modules to units to dependency module names, representing the unit's toolchain
  -- dependencies.
  -- This is not used at the moment, but might help improve Buck performance at some point.
  FieldToolchainDeps
  |
  -- | A list of the unit's modules in which TH extensions are enabled.
  FieldThModules
  |
  -- | A mapping from modules to the information required to restore the module graph from cache upon recompilation.
  FieldCache
  |
  -- | The schema used before the restructuring.
  FieldLegacy
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | The JSON key used for a field.
buildPlanKey :: BuildPlanField -> String
buildPlanKey = \case
  FieldExposedModules -> "exposed_modules"
  FieldModuleGraph -> "module_graph"
  FieldPackageDeps -> "package_deps"
  FieldProjectDeps -> "project_deps"
  FieldToolchainDeps -> "toolchain_deps"
  FieldThModules -> "th_modules"
  FieldCache -> "cache"
  FieldLegacy -> "legacy"

-- | When @--fields@ wasn't specified or is @all@, this is the default.
buildPlanAll :: NonEmpty BuildPlanField
buildPlanAll = [minBound .. maxBound]

-- | Avoid having to maintain a second mapping by pattern match in @parseBuildPlanKey@.
fieldsByKey :: Map String BuildPlanField
fieldsByKey =
  Map.fromList [(buildPlanKey f, f) | f <- toList buildPlanAll]

-- | Parse a key used by @--fields@.
parseBuildPlanKey :: String -> Maybe BuildPlanField
parseBuildPlanKey name = fieldsByKey !? name

data Args =
  Args {
    topdir :: Maybe String,
    workerTargetId :: Maybe TargetId,
    binPath :: [OsPath],
    tempDir :: Maybe OsPath,
    unit :: Maybe UnitName,
    buildPlan :: Maybe BuildPlanPath,
    -- | The file containing the current source hashes tracked by the external build tool.
    sourceHashes :: Maybe BuckHashesPath,
    -- | The path for the state file in which the worker stores the hashes of source files for incremental build plans.
    incrementalState :: Maybe IncrementalStatePath,
    -- | The build plan fields included in the JSON.
    fields :: Maybe (NonEmpty BuildPlanField),
    moduleTarget :: Maybe ModuleTarget,
    ghcOptions :: [String],
    perModuleFlags :: Map ModuleKey [String],
    cachedBuildPlans :: Maybe CachedBuildPlans,
    staticBuildPlans :: Maybe CachedBuildPlans,
    cachedDeps :: Maybe CachedDeps,
    homeUnit :: Maybe OsPath,
    isBinary :: Bool,
    features :: FeatureFlags
  }
  deriving stock (Eq, Show)

emptyArgs :: Map String String -> Args
emptyArgs env =
  Args {
    topdir = Just libdir,
    workerTargetId = Nothing,
    binPath = [],
    tempDir = toOsPath <$> (env !? "TMPDIR"),
    unit = Nothing,
    buildPlan = Nothing,
    sourceHashes = Nothing,
    incrementalState = Nothing,
    fields = Nothing,
    moduleTarget = Nothing,
    ghcOptions = [],
    perModuleFlags = Map.empty,
    cachedBuildPlans = Nothing,
    staticBuildPlans = Nothing,
    cachedDeps = Nothing,
    homeUnit = Nothing,
    isBinary = False,
    features = defaultFeatureFlags
  }
