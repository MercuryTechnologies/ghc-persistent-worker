module Internal.BuildPlan.Json where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (dropMissing, preserveMissing, zipWithMatched)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Module (ModuleName (..), UnitId)
import qualified System.File.OsPath as OsPath
import System.OsPath.Extra (OsPath)
import Types.Args (Args (..), BuildPlanField (..))
import Types.BuildPlan (
  BuildPlan (..),
  BuildPlanJson (..),
  BuildPlanModule (..),
  BuildPlanSchema (..),
  ModuleKey (..),
  PackageDep (..),
  PackageDeps (..),
  PackageKey (..),
  unionPackageDepsDeep,
  )
import Types.CachedDeps (
  CachedLevelDeps (..),
  CachedModule (..),
  CachedPackageDep (..),
  JsonFs (..),
  OtherLevel (..),
  )

--- | Modules available for import downstream.
fieldExposedModules :: Map ModuleKey BuildPlanModule -> Set ModuleKey
fieldExposedModules =
  Map.keysSet
  .
  Map.filter \ BuildPlanModule {boot} -> not boot

-- | Dependencies within the current unit, including boot files indicated by the suffix @-boot@.
fieldModuleGraph :: Map ModuleKey BuildPlanModule -> Map ModuleKey [ModuleKey]
fieldModuleGraph =
  fmap \ BuildPlanModule {modules, modulesBoot} -> (fst <$> modules) <> (fst <$> modulesBoot)

-- | Dependencies on other home units.
fieldPackageDeps :: Map ModuleKey BuildPlanModule -> PackageDeps
fieldPackageDeps =
  PackageDeps
  .
  fmap \ BuildPlanModule {packages} ->
    Map.fromList [(name, modules) | PackageDep {name, modules} <- packages]

-- | Modules with TH extensions enabled.
fieldThModules :: Map ModuleKey BuildPlanModule -> Set ModuleKey
fieldThModules =
  Map.keysSet
  .
  Map.filter \ BuildPlanModule {thEnabled} -> thEnabled

-- | A module's dependencies at the levels other than the normal one.
data ModuleLevelDeps =
  ModuleLevelDeps {
    -- | Its dependencies at each level other than the normal one.
    levels :: Map OtherLevel CachedLevelDeps,
    -- | Those it imports at no level but these. They stay out of the plain
    -- lists of its cache entry, which are at the normal level.
    otherOnly :: CachedLevelDeps
  }

-- | The data to restore each module's graph node from: its dependencies at the
-- normal level, within the current unit, on other home units, and on units
-- outside the build, and those at splice or quote level.
--
-- A restored node gets a normal edge for each dependency in the plain lists,
-- and an edge at splice or quote level for each in 'splice' and 'quote', so
-- the plain lists leave out what the module imports only at those levels. The
-- node then has the edges of the node GHC builds. The build plan's other
-- fields, which Buck orders the compiles by, keep every level.
fieldCache ::
  -- | Each module's external dependencies, as in 'assembleFields', whose
  -- units become the module's @units@.
  Map ModuleKey (Map UnitId (PackageKey, [ModuleName])) ->
  -- | Each module's dependencies at splice and quote level.
  Map ModuleKey ModuleLevelDeps ->
  -- | The modules of the build plan, with their dependencies within the
  -- build.
  Map ModuleKey BuildPlanModule ->
  Map ModuleKey CachedModule
fieldCache externalDeps levelDeps =
  Map.mapWithKey basic
  where
    -- TODO do we need boot deps here?
    basic key BuildPlanModule {source, modules, packages, flags} =
      CachedModule {
        source,
        modules = filter (`Set.notMember` otherModules) (snd <$> modules),
        packages = mapMaybe normalPackage packages,
        flags,
        units =
          filter (`Set.notMember` otherUnits)
            (JsonFs <$> Map.keys (Map.findWithDefault Map.empty key externalDeps)),
        splice = atLevel SpliceImport,
        quote = atLevel QuoteImport
      }
      where
        moduleLevels = Map.lookup key levelDeps

        atLevel level = Map.lookup level . (.levels) =<< moduleLevels

        (otherModules, otherPackages, otherUnits) = case (.otherOnly) <$> moduleLevels of
          Nothing -> (Set.empty, Map.empty, Set.empty)
          Just CachedLevelDeps {modules = ms, packages = ps, units = us} ->
            (
              Set.fromList ms,
              Map.fromListWith (<>) [(p.id, Set.fromList p.modules) | p <- ps],
              Set.fromList us
            )

        -- A static unit is outside the build for GHC, so its modules are left
        -- out with the unit.
        normalPackage dep
          | Set.member dep.id otherUnits = Nothing
          | null kept = Nothing
          | otherwise = Just CachedPackageDep {id = dep.id, modules = kept}
          where
            kept = filter (`Set.notMember` othersOf dep.id) dep.modules

            othersOf unit = Map.findWithDefault Set.empty unit otherPackages

-- | Append externally resolved package deps to each module.
mergePackageDeps ::
  Map ModuleKey (Map UnitId (PackageKey, [ModuleName])) ->
  Map ModuleKey BuildPlanModule ->
  Map ModuleKey BuildPlanModule
mergePackageDeps =
  Map.merge dropMissing preserveMissing (zipWithMatched combine)
  where
    combine _ deps BuildPlanModule {..} = BuildPlanModule {packages = packages ++ packageDeps deps, ..}

    packageDeps deps =
      [
        PackageDep {id = JsonFs unit, name, modules = coerce modules}
        | (unit, (name, modules)) <- Map.toList deps
      ]

-- | Create the final payload of the build plan JSON.
-- Include only the fields selected on the command line by the option @--fields@.
--
-- The external dependencies are each module's imports from units outside
-- the build, as 'Internal.BuildPlan.External.unitImports' resolves them. They
-- are of two kinds:
--
-- * Toolchain packages, from the package databases of the toolchain.
-- * Static units, the units of the project that a metadata request with
--   static dependency units makes visible as external packages, through a
--   synthetic package database (see
--   'Internal.Metadata.Static.prepareStaticSession').
--
-- The toolchain dependencies are the external dependencies without the
-- static units.
assembleFields ::
  Set BuildPlanField ->
  -- | The external dependencies.
  Map ModuleKey (Map UnitId (PackageKey, [ModuleName])) ->
  -- | Each module's dependencies at splice and quote level.
  Map ModuleKey ModuleLevelDeps ->
  -- | The toolchain dependencies.
  Map ModuleKey (Map UnitId (PackageKey, [ModuleName])) ->
  Map ModuleKey BuildPlanModule ->
  BuildPlanJson
assembleFields fields externalDeps levelDeps toolchainDeps modules =
  BuildPlanJson {
    legacy = fieldIf FieldLegacy (mergePackageDeps toolchainDeps modules),
    schema = BuildPlanSchema {
      exposed_modules = fieldIf FieldExposedModules (fieldExposedModules modules),
      module_graph = fieldIf FieldModuleGraph (fieldModuleGraph modules),
      package_deps = fieldIf FieldPackageDeps (unionPackageDepsDeep toolchainDepsPayload projectDeps),
      project_deps = fieldIf FieldProjectDeps projectDeps,
      toolchain_deps = fieldIf FieldToolchainDeps toolchainDepsPayload,
      th_modules = fieldIf FieldThModules (fieldThModules modules),
      cache = fieldIf FieldCache (fieldCache externalDeps levelDeps modules)
    }
  }
  where
    projectDeps = fieldPackageDeps modules

    toolchainDepsPayload = coerce (fmap (Map.fromList . Map.elems) toolchainDeps)

    fieldIf :: forall a . BuildPlanField -> a -> Maybe a
    fieldIf key value = if Set.member key fields then Just value else Nothing

-- | Write a JSON file for the given build plan.
writeBuildPlan :: OsPath -> BuildPlan -> IO ()
writeBuildPlan path BuildPlan {json} =
  OsPath.writeFile path (Aeson.encode json)

-- | Write the build plan JSON, passing through additional paths required by the Buck rules.
writeBuildPlanWith :: Args -> OsPath -> BuildPlan -> IO ()
writeBuildPlanWith args path BuildPlan {json} =
  OsPath.writeFile path (Aeson.encode (withExtras json))
  where
    withExtras value =
      case Aeson.toJSON value of
        Aeson.Object o -> Aeson.Object (KeyMap.union (KeyMap.fromList extraFields) o)
        v -> v

    extraFields =
      [
        ("is_binary", Aeson.toJSON args.isBinary),
        ("unit_args", nullableString args.unitArgsPath),
        ("unit_buck_args", nullableString args.unitBuckArgsPath),
        ("dep_units", nullableString args.depUnitsPath)
      ]

    nullableString = maybe Aeson.Null (Aeson.toJSON @String)
