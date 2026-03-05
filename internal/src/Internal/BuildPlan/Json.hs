module Internal.BuildPlan.Json where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath)
import Types.Args (BuildPlanField (..))
import Types.BuildPlan (
  BuildPlan (..),
  BuildPlanJson (..),
  BuildPlanModule (..),
  BuildPlanSchema (..),
  ModuleKey (..),
  PackageDep (..),
  PackageDeps (..),
  )
import Types.CachedDeps (CachedModule (..), CachedPackageDep (..))

--- | Modules available for import downstream.
fieldExposedModules :: Map ModuleKey BuildPlanModule -> [ModuleKey]
fieldExposedModules =
  Map.keys
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
fieldThModules :: Map ModuleKey BuildPlanModule -> [ModuleKey]
fieldThModules =
  coerce
  .
  Map.keys
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
    basic BuildPlanModule {source, modules, packages} =
      CachedModule {
        source,
        modules = (snd <$> modules),
        packages = [CachedPackageDep {id = dep.id, modules = dep.modules} | dep <- packages]
      }

-- | Create the final payload of the build plan JSON.
-- Include only the fields selected on the command line by the option @--fields@.
assembleFields ::
  Set BuildPlanField ->
  Map ModuleKey BuildPlanModule ->
  BuildPlanJson
assembleFields fields modules =
  BuildPlanJson {
    legacy = fieldIf FieldLegacy modules,
    schema = BuildPlanSchema {
      exposed_modules = fieldIf FieldExposedModules (fieldExposedModules modules),
      module_graph = fieldIf FieldModuleGraph (fieldModuleGraph modules),
      package_deps = fieldIf FieldPackageDeps projectDeps,
      project_deps = fieldIf FieldProjectDeps projectDeps,
      toolchain_deps = Nothing,
      th_modules = fieldIf FieldThModules (fieldThModules modules),
      cache = fieldIf FieldCache (fieldCache modules)
    }
  }
  where
    projectDeps = fieldPackageDeps modules

    fieldIf :: forall a . BuildPlanField -> a -> Maybe a
    fieldIf key value = if Set.member key fields then Just value else Nothing

-- | Write a JSON file for the given build plan.
writeBuildPlan :: OsPath -> BuildPlan -> IO ()
writeBuildPlan path BuildPlan {json} =
  OsPath.writeFile path (Aeson.encode json)
