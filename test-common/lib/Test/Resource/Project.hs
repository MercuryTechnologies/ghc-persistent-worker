-- | Project structure helpers for the resource consumption test.
--
-- Provides functions for constructing units, modules, and schedules for a fixed three-unit project with three modules
-- each. The concrete unit configuration (TH, bindings, ext deps) is defined in @ResourceTest@.
--
-- The dependency structure is a linear unit chain:
--
-- * Unit 0: leaf unit, no cross-unit deps
-- * Unit 1: depends on Unit 0
-- * Unit 2: depends on Unit 0 and Unit 1
--
-- Within each unit, modules have a prefix dependency chain:
-- Module 0 has no intra-unit deps, Module 1 depends on Module 0, Module 2 depends on Module 0 and Module 1.
module Test.Resource.Project where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Project (BuildModule (..), Component (..), GenUnit (..), ModuleKey (..), TaskKey (..), UnitKey (..))
import Test.Data.Scheduler (Schedule (..), Task (..))
import Test.Data.SourceMode (ModuleSource (..))

modulesPerUnit :: Int
modulesPerUnit = 3

-- | All module keys for a unit.
unitModuleKeys :: UnitKey -> [ModuleKey]
unitModuleKeys unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [0 .. modulesPerUnit - 1]]

-- | Module dependencies within a unit: each module depends on all prefix modules.
intraUnitDeps :: ModuleKey -> Set ModuleKey
intraUnitDeps ModuleKey {unit, number} =
  Set.fromList [ModuleKey {unit, number = n, errorVariant = Nothing} | n <- [0 .. number - 1]]

-- | Cross-unit module dependencies: all modules from dependency units.
crossUnitDeps :: Set UnitKey -> Set ModuleKey
crossUnitDeps depUnits =
  Set.fromList [mk | u <- Set.toList depUnits, mk <- unitModuleKeys u]

-- | All module dependencies (intra-unit + cross-unit) for a module in a unit with the given dep units.
moduleDeps :: Set UnitKey -> ModuleKey -> Set ModuleKey
moduleDeps depUnits key =
  Set.union (intraUnitDeps key) (crossUnitDeps depUnits)

-- | Construct a 'GenUnit' for one unit with the given dependency units.
-- The parameters @th@, @bindings@, and @extDeps@ are supplied by the caller (see @allUnits@ in @ResourceTest@).
mkUnit :: Bool -> Int -> Set Int -> UnitKey -> Set UnitKey -> GenUnit BuildModule
mkUnit th bindings extDeps unitKey depUnits =
  GenUnit {
    key = unitKey,
    depUnits,
    modules = [
      BuildModule {key, deps = moduleDeps depUnits key, th, bindings, extDeps}
      | key <- unitModuleKeys unitKey
    ]
  }

-- | The full module source map for writing project sources, keyed by 'ModuleKey'.
allModuleSources :: [GenUnit BuildModule] -> Map ModuleKey ModuleSource
allModuleSources units =
  Map.fromList
    [(bm.key, ModuleSource {deps = Set.toList bm.deps, th = bm.th, bindings = bm.bindings, extDeps = bm.extDeps})
    | u <- units, bm <- u.modules]

-- | Build a metadata task for one unit.
metaTask :: GenUnit BuildModule -> Task TaskKey Component
metaTask unit =
  Task {
    key = TaskMeta unit.key,
    deps = Set.map TaskMeta unit.depUnits,
    value = ComponentUnit unit
  }

-- | Build a compile task for one module within a unit.
moduleTask :: UnitKey -> BuildModule -> Task TaskKey Component
moduleTask unitKey BuildModule {key, deps} =
  Task {
    key = TaskCompile key,
    deps = Set.insert (TaskMeta unitKey) (Set.map TaskCompile deps),
    value = ComponentModule key
  }

-- | Tasks for a single unit: one metadata task followed by all module compile tasks.
unitTasks :: GenUnit BuildModule -> [Task TaskKey Component]
unitTasks unit =
  metaTask unit : [moduleTask unit.key bm | bm <- unit.modules]

-- | The full schedule in strict dependency order.
-- Since this is a static project, we use a deterministic order: units in dependency order, within each unit metadata
-- first, then modules by number.
schedule :: [GenUnit BuildModule] -> Schedule TaskKey Component
schedule units =
  Schedule {tasks = concatMap unitTasks units}
