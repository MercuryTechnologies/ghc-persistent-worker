module Internal.BuildPlan.Json where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (dropMissing, preserveMissing, zipWithMatched)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Module (ModuleName (..), UnitId)
import qualified System.File.OsPath as OsPath
import Types.Args (BuildPlanField (..))
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
import Types.BuildPlan.Incremental (BuildPlanPath (..))
import Types.CachedDeps (CachedModule (..), CachedPackageDep (..), JsonFs (..))

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

-- | Dependencies within the current unit, including boot files indicated by the suffix @-boot@.
fieldCache ::
  Map ModuleKey BuildPlanModule ->
  Map ModuleKey CachedModule
fieldCache =
  fmap basic
  where
    -- TODO do we need boot deps here?
    basic BuildPlanModule {source, modules, packages, flags} =
      CachedModule {
        source,
        modules = (snd <$> modules),
        packages = [CachedPackageDep {id = dep.id, modules = dep.modules} | dep <- packages],
        flags
      }

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
assembleFields ::
  Set BuildPlanField ->
  Map ModuleKey (Map UnitId (PackageKey, [ModuleName])) ->
  Map ModuleKey BuildPlanModule ->
  BuildPlanJson
assembleFields fields toolchainDeps modules =
  BuildPlanJson {
    legacy = fieldIf FieldLegacy (mergePackageDeps toolchainDeps modules),
    schema = BuildPlanSchema {
      exposed_modules = fieldIf FieldExposedModules (fieldExposedModules modules),
      module_graph = fieldIf FieldModuleGraph (fieldModuleGraph modules),
      package_deps = fieldIf FieldPackageDeps (unionPackageDepsDeep toolchainDepsPayload projectDeps),
      project_deps = fieldIf FieldProjectDeps projectDeps,
      toolchain_deps = fieldIf FieldToolchainDeps toolchainDepsPayload,
      th_modules = fieldIf FieldThModules (fieldThModules modules),
      cache = fieldIf FieldCache (fieldCache modules)
    }
  }
  where
    projectDeps = fieldPackageDeps modules

    toolchainDepsPayload = coerce (fmap (Map.fromList . Map.elems) toolchainDeps)

    fieldIf :: forall a . BuildPlanField -> a -> Maybe a
    fieldIf key value = if Set.member key fields then Just value else Nothing

-- | Write a JSON file for the given build plan.
writeBuildPlan :: BuildPlanPath -> BuildPlan -> IO ()
writeBuildPlan (BuildPlanPath path) BuildPlan {json} =
  OsPath.writeFile path (Aeson.encode json)
