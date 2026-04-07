-- | Unit and project types for the standalone GHC server.
module GhcServer.Data.Unit where

import Data.Map.Strict (Map)
import Data.String (IsString)
import GHC (ModuleName, mkModuleName, moduleNameString)
import GHC.Data.Graph.Directed qualified as Graph
import GHC.Data.Graph.Directed (Graph)
import GHC.Unit (UnitId, stringToUnit)
import GHC.Unit.Types (toUnitId, unitIdString)
import GHC.Utils.Outputable (Outputable (..), text)
import GhcServer.Path (osPath)
import System.OsPath (OsPath, (</>))

-- | A unit name used as the identity of a build unit.
--
-- This is a plain string that can be converted to a GHC 'UnitId' at the API boundary via
-- 'unitId'.
newtype UnitName =
  UnitName { string :: String }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Outputable UnitName where
  ppr (UnitName name) = text name

-- | Convert a 'UnitName' to a GHC 'UnitId'.
unitId :: UnitName -> UnitId
unitId n =
  toUnitId (stringToUnit n.string)

-- | Precomputed cache file paths for a unit, derived at project discovery time.
data UnitCache =
  UnitCache {
    -- | The cache subdirectory for this unit (@cache/unitName/@).
    dir :: OsPath,
    -- | Path to @cached_unit.json@.
    cachedUnitPath :: OsPath,
    -- | Path to @unit_args@.
    unitArgsPath :: OsPath,
    -- | Path to @dep_units.json@.
    depUnitsPath :: OsPath
  }
  deriving stock (Show)

-- | Compute the absolute path to a module's @.dyn_hi@ file.
--
-- The path is @outputDir/unitId/ModuleName.dyn_hi@.
moduleHiPath :: OsPath -> UnitName -> ModuleName -> OsPath
moduleHiPath outputDir name modName =
  outputDir </> osPath (unitIdString (unitId name)) </> osPath (moduleNameString modName ++ ".dyn_hi")

-- | A unit discovered in the project, identified by its directory name.
data Unit =
  Unit {
    -- | Directory name, used as the unit identity.
    name :: UnitName,
    -- | Absolute path to the unit's directory.
    dir :: OsPath,
    -- | GHC CLI arguments read from @unit.json@.
    ghcArgs :: [String],
    -- | Source files (absolute paths) discovered in the unit directory.
    sources :: [OsPath],
    -- | Names of home units that this unit depends on, as declared in @unit.json@.
    depUnits :: [UnitName],
    -- | Precomputed cache paths for this unit.
    cache :: UnitCache
  }
  deriving stock (Show)

-- | The type of a node in the unit dependency graph.
--
-- Each node's key is the 'UnitName'; its payload is the unit's own cache path
-- (used when building 'CachedBuildPlans' from the transitive closure).
type UnitDepNode = Graph.Node UnitName OsPath

-- | A project is the collection of all units in the build root.
data Project =
  Project {
    -- | All units, keyed by unit name.
    units :: Map UnitName Unit,
    -- | Directed dependency graph of units.
    --
    -- Each node's payload is that unit's @cached_unit.json@ path.
    -- Edges point from a unit to its direct dependencies.
    -- Use 'reachablesG' to query the transitive closure.
    depGraph :: Graph UnitDepNode
  }

-- | A parsed module name from the client, used in schedule requests.
--
-- Distinct from GHC's 'ModuleName' to represent a user-provided string that hasn't been
-- validated against the module graph yet.
newtype ClientModule =
  ClientModule { string :: String }
  deriving stock (Show)
  deriving newtype (Eq)

-- | Convert a 'ClientModule' to a GHC 'ModuleName'.
clientModuleName :: ClientModule -> ModuleName
clientModuleName m =
  mkModuleName m.string

-- | Compute cache paths for a unit.
mkUnitCache :: OsPath -> UnitName -> UnitCache
mkUnitCache projectRoot name =
  UnitCache {
    dir = cDir,
    cachedUnitPath = cDir </> osPath "cached_unit.json",
    unitArgsPath = cDir </> osPath "unit_args",
    depUnitsPath = cDir </> osPath "dep_units.json"
  }
  where
    cDir = projectRoot </> osPath "cache" </> osPath name.string
