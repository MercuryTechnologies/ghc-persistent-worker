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
import System.OsPath.Extra (OsPath, fromOsPath, decodeUtf, osp, toOsPath, (<.>), (</>))
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
import Test.Path (cachedUnitPath, moduleName, moduleSourcePath, unitCacheDir, unitName, unitOutputDir)
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
writeUnitArgs :: OsPath -> [String] -> UnitKey -> IO FilePath
writeUnitArgs tempDir ghcOptions unit = do
  createDirectoryIfMissing True dir
  argsPath <- decodeUtf (dir </> [osp|unit_args|])
  writeFile argsPath (unlines ghcOptions)
  pure argsPath
  where
    dir = tempDir </> unitCacheDir unit

cachedBuildPlan :: OsPath -> UnitKey -> CachedBuildPlan
cachedBuildPlan tempDir d =
  CachedBuildPlan {
    name = jsonFsFromString (unitName d),
    build_plan = fromOsPath (tempDir </> unitCacheDir d </> [osp|cached_unit.json|])
  }

-- | Write the build plan index for a unit to a file.
-- These are consumed by both metadata and compile steps, and in the former we don't need to read them from files, so we
-- return them for direct use as well.
writeBuildPlans :: OsPath -> UnitKey -> [UnitKey] -> IO (FilePath, CachedBuildPlans)
writeBuildPlans tempDir unit depUnits = do
  Aeson.encodeFile outFile plans
  pure (outFile, plans)
  where
    outFile = fromOsPath (tempDir </> unitCacheDir unit </> [osp|dep_units.json|])
    plans = CachedBuildPlans (cachedBuildPlan tempDir <$> depUnits)

-- | The full cache dataset describing a unit, used by compile steps to restore unit states in resume builds.
cachedUnit ::
  Map (JsonFs ModuleName) CachedModule ->
  FilePath ->
  FilePath ->
  CachedUnit
cachedUnit build_plan args depUnits =
  CachedUnit {
    build_plan = Just build_plan,
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
    packages = cachedPackageDep <$> packages
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
  GenUnit BuildModule ->
  IO CachedBuildPlans
writeUnitCache env unit = do
  argsFile <- writeUnitArgs env.tempDir ((metadataArgs env unit).ghcOptions) unit.key
  (depUnitsFile, buildPlans) <- writeBuildPlans env.tempDir unit.key (toList unit.depUnits)
  Aeson.encodeFile outFile (cachedUnit buildPlan argsFile depUnitsFile)
  pure buildPlans
  where
    buildPlan = Map.fromList (buildPlanEntry env unit.key <$> unit.modules)
    outFile = fromOsPath (env.tempDir </> cachedUnitPath unit.key)

-- | Construct all module-related cache data.
--
-- Although compile steps do have to decode JSON files for the home unit build plan, that file is written in
-- 'writeUnitCache', since it's a single file used by each module.
-- So this only returns the path to that file, alongside the paths to all dependency interfaces in 'CachedDeps', which
-- is decoded in "Types.BuckArgs", so we can pass it as data.
moduleCache ::
  SessionEnv ->
  ModuleKey ->
  Set TaskKey ->
  (OsPath, CachedDeps)
moduleCache env key deps =
  (unitPath, CachedDeps (mkCachedDep <$> depKeys))
  where
    mkCachedDep dc =
      CachedDep {
        name = jsonFsFromString (moduleName dc),
        package = jsonFsFromString (unitName dc.unit),
        interfaces = interfacePath dc :| []
      }

    unitPath = env.tempDir </> cachedUnitPath key.unit

    depKeys = [m | TaskCompile m <- Set.toList deps]

    interfacePath dc =
      fromOsPath (env.tempDir </> unitOutputDir dc.unit </> toOsPath (moduleName dc) <.> [osp|dyn_hi|])

-- | Bundle a build task with its associated cache data for the resume build.
cacheTask ::
  SessionEnv ->
  Task TaskKey Component ->
  IO (Task TaskKey ResumeComponent)
cacheTask env task =
  case task.value of
    ComponentUnit unit -> do
      cachedBuildPlans <- Just <$> writeUnitCache env unit
      pure task {value = ResumeUnit unit (UnitCache {cachedBuildPlans})}
    ComponentModule moduleKey ->
      pure task {value = ResumeModule moduleKey (ModuleCache {cachedUnit = unitPath, cachedDeps})}
      where
        (unitPath, cachedDeps) = moduleCache env moduleKey task.deps

-- | Transform a schedule for the resume build by constructing and writing all required cache data and JSON files and
-- bundling that data with the tasks.
writeResumeCache ::
  SessionEnv ->
  Schedule TaskKey Component ->
  IO (Schedule TaskKey ResumeComponent)
writeResumeCache env (Schedule tasks) =
  Schedule <$> traverse (cacheTask env) tasks
