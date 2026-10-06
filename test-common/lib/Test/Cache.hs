module Test.Cache where

import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import Data.List (partition)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Unit.Types (UnitId (..))
import Language.Haskell.Syntax.Module.Name (ModuleName (..))
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath.Extra (OsPath, fromOsPath, osp, (</>))
import Test.Build (metadataArgs)
import Test.Data.Env (SessionEnv (..))
import Test.Data.Project (
  BuildModule (..),
  Component (..),
  GenUnit (..),
  ModuleCache (..),
  ModuleKey (..),
  ResumeComponent (..),
  TaskKey (..),
  UnitCache (..),
  UnitKey,
  )
import Test.Data.Scheduler (Schedule (..), Task (..))
import Test.Path (cachedUnitPath, moduleName, moduleSourcePath, unitCacheDir, unitName)
import Types.Args (Args (..))
import Types.CachedDeps (
  CachedBuildPlan (..),
  CachedBuildPlans (..),
  CachedDep (..),
  CachedDeps (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  jsonFsFromString,
  )

-- | Write the GHC arguments used to construct a unit state to a file, as it is done by Buck.
writeUnitArgs :: OsPath -> [String] -> UnitKey -> IO OsPath
writeUnitArgs tempDir ghcOptions unit = do
  createDirectoryIfMissing True dir
  let argsPath = dir </> [osp|unit_args|]
  writeFile (fromOsPath argsPath) (unlines ghcOptions)
  pure argsPath
  where
    dir = tempDir </> unitCacheDir unit

cachedBuildPlan :: OsPath -> UnitKey -> CachedBuildPlan
cachedBuildPlan tempDir d =
  CachedBuildPlan {
    name = jsonFsFromString (unitName d),
    build_plan = tempDir </> unitCacheDir d </> [osp|cached_unit.json|]
  }

-- | Write the build plan index for a unit to a file.
-- These are consumed by both metadata and compile steps, and in the former we don't need to read them from files, so we
-- return them for direct use as well.
writeBuildPlans :: OsPath -> UnitKey -> [UnitKey] -> IO (OsPath, CachedBuildPlans)
writeBuildPlans tempDir unit depUnits = do
  Aeson.encodeFile (fromOsPath outFile) plans
  pure (outFile, plans)
  where
    outFile = tempDir </> unitCacheDir unit </> [osp|dep_units.json|]
    plans = CachedBuildPlans (cachedBuildPlan tempDir <$> depUnits)

-- | The full cache dataset describing a unit, used by compile steps to restore unit states in resume builds.
cachedUnit ::
  Map (JsonFs ModuleName) CachedModule ->
  OsPath ->
  OsPath ->
  CachedUnit
cachedUnit build_plan args depUnits =
  CachedUnit {
    build_plan = Just build_plan,
    is_binary = False, -- for now, only testing library
    unit_args = Just args,
    unit_buck_args = Nothing,
    dep_units = Just depUnits,
    cache = Nothing
  }

-- | A non-home-unit dependency entry for a module in a unit's build plan cache file.
cachedPackageDep :: NonEmpty ModuleKey -> CachedPackageDep
cachedPackageDep depMods@(ModuleKey {unit} :| _) =
  CachedPackageDep {
    id = jsonFsFromString (unitName unit),
    modules = jsonFsFromString . moduleName <$> toList depMods
  }

-- | One module entry for a unit's build plan cache file.
cachedModule :: SessionEnv -> UnitKey -> BuildModule -> CachedModule
cachedModule env unit BuildModule {key, deps} =
  CachedModule {
    source = env.sourceDir </> moduleSourcePath key,
    modules = jsonFsFromString . moduleName <$> foldMap toList home,
    packages = cachedPackageDep <$> packages,
    flags = []
  }
  where
    allDeps = Set.toList deps
    (home, packages) = partition matchUnit (NonEmpty.groupWith (.unit) allDeps)

    matchUnit (ModuleKey {unit = u} :| _) = unit == u

-- | One key-value pair for a module entry in a unit's build plan cache file.
buildPlanEntry ::
  SessionEnv ->
  UnitKey ->
  BuildModule ->
  (JsonFs ModuleName, CachedModule)
buildPlanEntry env unit module_ =
  (jsonFsFromString (moduleName module_.key), cachedModule env unit module_)

-- | Write all unit-related cache files that need to be decoded at some point.
--
-- The full cached unit is only consumed by compile steps, so it is only written it here.
-- For metadata steps, the 'CachedBuildPlans' are decoded in "Types.BuckArgs", so we can pass it to the handler as data.
writeUnitCache ::
  SessionEnv ->
  [UnitKey] ->
  GenUnit BuildModule ->
  IO CachedBuildPlans
writeUnitCache env deps unit = do
  argsFile <- writeUnitArgs env.tempDir ((metadataArgs env unit).ghcOptions) unit.key
  (depUnitsFile, buildPlans) <- writeBuildPlans env.tempDir unit.key deps
  Aeson.encodeFile outFile (cachedUnit buildPlan argsFile depUnitsFile)
  pure buildPlans
  where
    buildPlan = Map.fromList (buildPlanEntry env unit.key <$> unit.modules)
    outFile = fromOsPath (env.tempDir </> cachedUnitPath unit.key)

-- | The transitive dependencies of a node (excluding the node itself) in dependency postorder, i.e. every node is
-- preceded by all of its own dependencies.
depClosurePostorder :: Ord k => Map k (Set k) -> k -> [k]
depClosurePostorder deps root =
  reverse (snd (foldl' visit ([root], []) (nodeDeps root)))
  where
    -- When @(seen', acc') = visit (seen, acc) k@:
    --
    -- Preconditions
    -- * @fromList acc `isSubsetOf` seen@
    --
    -- Postconditions
    -- * @member k seen'@
    -- * @descendentsOf k `isSubsetOf` seen'@
    -- * @seen `isSubsetOf` seen'@
    -- * @fromList acc' `isSubsetOf` seen'@
    -- * @descendantsOf k `isSubsetOf` dropWhile (/= k) acc'@
    visit (seen, acc) k
      | Set.member k seen = (seen, acc)
      | otherwise =
          let (seen', acc') = foldl' visit (Set.insert k seen, acc) (nodeDeps k)
          in (seen', k : acc')

    nodeDeps k = Map.findWithDefault Set.empty k deps

-- | Construct all module-related cache data.
--
-- Although compile steps do have to decode JSON files for the home unit build plan, that file is written in
-- 'writeUnitCache', since it's a single file used by each module.
-- So this only returns the path to that file, alongside the names of all dependency modules in 'CachedDeps', which
-- is decoded in "Types.BuckArgs", so we can pass it as data.
moduleCache ::
  SessionEnv ->
  Map TaskKey (Set TaskKey) ->
  ModuleKey ->
  (OsPath, CachedDeps)
moduleCache env taskDeps key =
  (unitPath, CachedDeps (mkCachedDep <$> depKeys))
  where
    mkCachedDep dc =
      CachedDep {
        name = jsonFsFromString (moduleName dc),
        package = jsonFsFromString (unitName dc.unit)
      }

    unitPath = env.tempDir </> cachedUnitPath key.unit

    depKeys = [m | TaskCompile m <- depClosurePostorder taskDeps (TaskCompile key)]

-- | Bundle a build task with its associated cache data for the resume build.
cacheTask ::
  SessionEnv ->
  -- | Dep graph of units.
  Map UnitKey (Set UnitKey) ->
  -- | Dep graph of tasks, consisting of units and modules.
  Map TaskKey (Set TaskKey) ->
  Task TaskKey Component ->
  IO (Task TaskKey ResumeComponent)
cacheTask env directDeps taskDeps task =
  case task.value of
    ComponentUnit unit -> do
      cachedBuildPlans <- Just <$> writeUnitCache env (depClosurePostorder directDeps unit.key) unit
      pure task {value = ResumeUnit unit (UnitCache {cachedBuildPlans})}
    ComponentModule moduleKey ->
      pure task {value = ResumeModule moduleKey (ModuleCache {cachedUnit = unitPath, cachedDeps})}
      where
        (unitPath, cachedDeps) = moduleCache env taskDeps moduleKey

-- | Transform a schedule for the resume build by constructing and writing all required cache data and JSON files and
-- bundling that data with the tasks.
writeResumeCache ::
  SessionEnv ->
  Schedule TaskKey Component ->
  IO (Schedule TaskKey ResumeComponent)
writeResumeCache env (Schedule tasks) =
  Schedule <$> traverse (cacheTask env directDeps taskDeps) tasks
  where
    taskDeps = Map.fromList [(task.key, task.deps) | task <- tasks]
    directDeps = Map.fromList [(unit.key, unit.depUnits) | Task {value = ComponentUnit unit} <- tasks]
