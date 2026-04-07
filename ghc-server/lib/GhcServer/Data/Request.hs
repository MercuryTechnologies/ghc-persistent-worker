-- | Request types for the standalone GHC server build schedule.
module GhcServer.Data.Request where

import GhcServer.Data.Unit (ClientModule, UnitName)

-- | What to build for a unit: metadata, individual modules, or everything.
data UnitRequest =
  -- | Run the metadata step only.
  UnitMetadata
  |
  -- | Compile all modules (skip metadata).
  UnitModulesOnly
  |
  -- | Compile specific modules (skip metadata).
  UnitModules [ClientModule]
  |
  -- | Run metadata and compile all modules.
  UnitAll
  deriving stock (Show, Eq)

-- | The sequence of build steps requested by the user.
data ScheduleRequest =
  ScheduleRequest {
    steps :: [(UnitName, UnitRequest)],
    -- | Force recompilation of modules even when cached artifacts exist.
    recompile :: Bool,
    -- | Recompute metadata and recompile even when cached.
    rebuild :: Bool
  }
  deriving stock (Show, Eq)

-- | A unit target as computed by 'effectiveRequests'.
--
-- Separates explicit user requests from implicit transitive dependencies.
-- Implicit deps exist solely for ordering and always use 'UnitAll' scope;
-- their request type is fixed by construction rather than computed.
data EffectiveUnit =
  -- | A unit explicitly requested by the user with a specific scope.
  Explicit UnitName UnitRequest
  |
  -- | A transitive dependency added implicitly, always built with 'UnitAll' scope.
  ImplicitDep UnitName
  deriving stock (Show, Eq)

-- | Extract the unit name from an effective unit.
effectiveUnitName :: EffectiveUnit -> UnitName
effectiveUnitName = \case
  Explicit name _ -> name
  ImplicitDep name -> name

-- | Whether an effective unit triggers compilation.
--
-- Explicit requests delegate to 'isCompileRequest'.
-- Implicit deps are controlled by the @recompile@ parameter: when 'True',
-- implicit deps enable compilation directly; when 'False' (default), their
-- compile tasks start disabled and are only promoted transitively by the
-- scheduler when they appear as dependencies of explicitly-compiled units.
--
-- Module index entries are always built for all units (including cached
-- implicit deps), so cross-unit scheduler dependencies are created.
-- Whether to actually compile or skip is decided at dispatch time by
-- 'dispatchTask' based on cache availability.
effectiveIsCompile :: Bool -> EffectiveUnit -> Bool
effectiveIsCompile recompile = \case
  Explicit _ req -> isCompileRequest req
  ImplicitDep _ -> recompile

-- | Whether a 'UnitRequest' triggers compilation for a unit.
isCompileRequest :: UnitRequest -> Bool
isCompileRequest = \case
  UnitMetadata -> False
  _ -> True
