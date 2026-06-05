{-# LANGUAGE PatternSynonyms #-}

module Incremental.FlowTest where

import Control.Concurrent (MVar)
import Control.Monad (void)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (LazyByteString)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (ModuleName (..))
import GHC.Driver.Monad (withSession)
import GHC.Utils.Outputable (showPprUnsafe)
import Hedgehog (TestT, evalEither, evalMaybe, (===))
import Incremental.Flow (replaceTemp, restoreUnit, runMetadata, writeUnitCache)
import Incremental.FlowData (
  Dep (..),
  Mod (..),
  PDep (..),
  Unit (..),
  bpFields,
  graphNames,
  unit1,
  unit2,
  unit2_modified,
  unitSpec,
  )
import Internal.BuildPlan (buildPlanFull, buildPlanIncremental, buildPlanModules, downsweepIncremental)
import Internal.BuildPlan.Incremental (
  loadCachedGraph,
  loadIncrementalState,
  mergeBuildPlanJson,
  mergeCacheAndDeps,
  pruneCachedPlan,
  readSourceHashes,
  writeIncrementalState,
  )
import Internal.BuildPlan.Json (writeBuildPlan)
import Internal.Compat.FixedNodes (support_FixedNodes)
import Internal.State (newState)
import System.Directory.OsPath (doesDirectoryExist, removeDirectoryRecursive)
import System.OsPath.Extra (OsPath, fromOsPath, osp, takeFileName, toOsPath, (<.>), (</>))
import Test.BuckHashes (writeHashesFromPaths)
import Test.Run (assertJust, unitTest, withTemp)
import Test.Target (fileUnitTargets)
import Test.Tasty (TestTree)
import qualified Types.BuildPlan as BuildPlan
import Types.BuildPlan (
  BuildPlanJson (..),
  BuildPlanSchema (..),
  ModuleKey,
  PackageDeps (..),
  PackageKey (..),
  unionPackageDepsDeep,
  )
import Types.BuildPlan.Incremental (
  BuckHash (..),
  BuckHashes (..),
  BuckHashesPath (..),
  BuildPlanPath (..),
  IncrementalStatePath (..),
  SourceChanges (..),
  SourceHashes,
  unsafeSourceHashes,
  )
import Types.CachedDeps (CachedBuildPlans (..))
import Types.State (WorkerState)

--------------------------------------------------------------------------------
-- Assertion targets
--------------------------------------------------------------------------------

targetMetadata1 :: BuckHashes
targetMetadata1 =
  BuckHashes {
    version = 1,
    digests = [
      BuckHash {path = [osp|temp/src/unit2/U2M0.hs|], digest = "8592847317355292305:30"},
      BuckHash {path = [osp|temp/src/unit2/U2M1.hs|], digest = "-5270433868467254481:30"},
      BuckHash {path = [osp|temp/src/unit2/U2M2.hs|], digest = "-2463643859312970673:30"},
      BuckHash {path = [osp|temp/src/unit2/U2M3.hs|], digest = "-2995559372185460638:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M4.hs|], digest = "3787816292599682415:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M5.hs|], digest = "4365233991208238977:66"},
      BuckHash {path = [osp|temp/src/unit2/U2M6.hs|], digest = "-4022354723098617458:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M7.hs|], digest = "-1188684826538732179:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M8.hs|], digest = "-7302214425244923396:54"}
    ]
  }

targetMetadata2 :: BuckHashes
targetMetadata2 =
  BuckHashes {
    version = 1,
    digests = [
      BuckHash {path = [osp|temp/src/unit2/U2M0.hs|], digest = "8592847317355292305:30"},
      BuckHash {path = [osp|temp/src/unit2/U2M3.hs|], digest = "-2995559372185460638:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M4.hs|], digest = "3787816292599682415:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M5.hs|], digest = "-7595932076908619578:78"},
      BuckHash {path = [osp|temp/src/unit2/U2M6.hs|], digest = "-4022354723098617458:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M7.hs|], digest = "-1188684826538732179:42"},
      BuckHash {path = [osp|temp/src/unit2/U2M8.hs|], digest = "-7302214425244923396:54"},
      BuckHash {path = [osp|temp/src/unit2/U2M9.hs|], digest = "295940999689793096:54"}
    ]
  }

targetUpdated :: Set OsPath
targetUpdated =
  [
    [osp|U2M5.hs|],
    [osp|U2M9.hs|]
  ]

targetInvalidated :: Set OsPath
targetInvalidated =
  [
    [osp|U2M1.hs|],
    [osp|U2M2.hs|],
    [osp|U2M5.hs|]
  ]

fixed :: Bool
fixed = support_FixedNodes

-- | The cached graph loaded in the rebuild pass, consisting of the graph written in the initial
-- pass, with all changed modules removed (modules 1, 2, 5 in unit2).
targetValid :: [(String, String, Bool, [(String, String)])]
targetValid =
  [
    ("unit1", "U1M0", fixed, []),
    ("unit1", "U1M1", fixed, []),
    ("unit1", "U1M2", fixed, []),
    ("unit1", "U1M3", fixed, []),
    ("unit1", "U1M4", fixed, []),
    ("unit1", "U1M5", fixed, [("unit1", "U1M4")]),
    ("unit1", "U1M6", fixed, []),
    ("unit1", "U1M7", fixed, []),
    ("unit1", "U1M8", fixed, []),
    ("unit1", "U1M9", fixed, []),
    ("unit2", "U2M0", fixed, [("unit1", "U1M0")]),
    ("unit2", "U2M3", fixed, [("unit2", "U2M0"), ("unit1", "U1M3")]),
    ("unit2", "U2M4", fixed, [("unit2", "U2M3"), ("unit1", "U1M4")]),
    ("unit2", "U2M6", fixed, [("unit2", "U2M5"), ("unit1", "U1M6")]),
    ("unit2", "U2M7", fixed, [("unit2", "U2M6"), ("unit1", "U1M7")]),
    ("unit2", "U2M8", fixed, [("unit2", "U2M5"), ("unit2", "U2M7"), ("unit1", "U1M8")])
  ]

-- | The graph returned from downsweep in the rebuild pass.
-- The @Bool@ indicates whether a node is fixed, which should be the case for each node that was unchanged since the
-- initial build (module 5 was modified, module 9 was added, in unit2).
-- It's important to assert that all fixed nodes in the current unit have intact dependencies.
targetRecomputed :: [(String, String, Bool, [(String, String)])]
targetRecomputed =
  [
    ("unit1", "U1M0", fixed, []),
    ("unit1", "U1M1", fixed, []),
    ("unit1", "U1M2", fixed, []),
    ("unit1", "U1M3", fixed, []),
    ("unit1", "U1M4", fixed, []),
    ("unit1", "U1M5", fixed, [("unit1", "U1M4")]),
    ("unit1", "U1M6", fixed, []),
    ("unit1", "U1M7", fixed, []),
    ("unit1", "U1M8", fixed, []),
    ("unit1", "U1M9", fixed, []),
    ("unit2", "U2M0", fixed, [("unit1", "U1M0")]),
    ("unit2", "U2M3", fixed, [("unit2", "U2M0"), ("unit1", "U1M3")]),
    ("unit2", "U2M4", fixed, [("unit2", "U2M3"), ("unit1", "U1M4")]),
    ("unit2", "U2M5", False, [("unit1", "U1M8"), ("unit1", "U1M5"), ("unit2", "U2M4"), ("unit2", "U2M3"), ("unit2", "U2M0")]),
    ("unit2", "U2M6", fixed, [("unit2", "U2M5"), ("unit1", "U1M6")]),
    ("unit2", "U2M7", fixed, [("unit2", "U2M6"), ("unit1", "U1M7")]),
    ("unit2", "U2M8", fixed, [("unit2", "U2M5"), ("unit2", "U2M7"), ("unit1", "U1M8")]),
    ("unit2", "U2M9", False, [("unit1", "U1M9"), ("unit2", "U2M5"), ("unit2", "U2M4")])
  ]

targetModuleGraph :: Unit -> Map ModuleKey [ModuleKey]
targetModuleGraph unit =
  Map.fromList [(mkey, [dep.mkey | dep <- home]) | Mod {mkey, home} <- unit.mods]

targetProjectDeps :: Unit -> PackageDeps
targetProjectDeps Unit {mods} =
  PackageDeps $ Map.fromList [(mkey, Map.fromList (unitDep <$> package)) | Mod {mkey, package} <- mods]
  where
    unitDep dep = (PackageKey dep.unit, (.mname) <$> dep.mods)

targetToolchainDeps :: Unit -> PackageDeps
targetToolchainDeps Unit {mods} =
  PackageDeps $ Map.fromList [(mkey, [("base", ["Prelude"])]) | Mod {mkey} <- mods]

targetPackageDeps :: Unit -> PackageDeps
targetPackageDeps unit =
  unionPackageDepsDeep (targetProjectDeps unit) (targetToolchainDeps unit)

--------------------------------------------------------------------------------
-- Test setup
--------------------------------------------------------------------------------

data IncrementalPaths =
  IncrementalPaths {
    tempDir :: OsPath,
    srcDir :: OsPath,
    buildPlan :: BuildPlanPath,
    incrementalState :: IncrementalStatePath,
    hashesInitial :: BuckHashesPath,
    hashesRebuild :: BuckHashesPath,
    sourcesInitial :: [OsPath],
    sourcesRebuild :: [OsPath]
  }
  deriving stock (Eq, Show)

sourcePath :: OsPath -> Mod -> OsPath
sourcePath srcDir Mod {unit, name} =
  srcDir </> toOsPath unit </> toOsPath name <.> [osp|hs|]

incrementalPaths :: OsPath -> IncrementalPaths
incrementalPaths tempDir =
  IncrementalPaths {
    tempDir,
    srcDir,
    buildPlan = BuildPlanPath (tempDir </> [osp|unit2-build-plan.json|]),
    incrementalState = IncrementalStatePath (tempDir </> [osp|incremental.json|]),
    hashesInitial = BuckHashesPath (tempDir </> [osp|source_hashes_1.json|]),
    hashesRebuild = BuckHashesPath (tempDir </> [osp|source_hashes_2.json|]),
    sourcesInitial = sourcePath srcDir <$> unit2.mods,
    sourcesRebuild = sourcePath srcDir <$> unit2_modified.mods
  }
  where
    srcDir = tempDir </> [osp|src|]

initUnitSources :: OsPath -> Unit -> IO ()
initUnitSources srcDir unit = do
  whenM (doesDirectoryExist unitDir) do
    removeDirectoryRecursive unitDir
  void $ fileUnitTargets (fromOsPath srcDir) (unitSpec unit.index unit.mods)
  where
    unitDir = srcDir </> toOsPath unit.name

prepareInitialBuild :: IncrementalPaths -> IO (LazyByteString, MVar WorkerState, CachedBuildPlans)
prepareInitialBuild IncrementalPaths {tempDir, srcDir, hashesInitial, sourcesInitial} = do
  initUnitSources srcDir unit1
  initUnitSources srcDir unit2
  meta <- writeHashesFromPaths hashesInitial sourcesInitial
  state <- liftIO newState
  unit1Plans <- writeUnitCache tempDir srcDir unit1
  pure (meta, state, unit1Plans)

initialBuild ::
  IncrementalPaths ->
  MVar WorkerState ->
  CachedBuildPlans ->
  TestT IO BuildPlanSchema
initialBuild IncrementalPaths {buildPlan, incrementalState, hashesInitial, sourcesInitial} state unit1Plans = do
  restoreUnit state unit1Plans
  runMetadata "initial" state \ logger -> lift do
    plan <- buildPlanFull logger bpFields [] [] sourcesInitial
    liftIO (writeBuildPlan buildPlan plan)
    hashes <- readSourceHashes targets hashesInitial
    liftIO $ writeIncrementalState incrementalState hashes plan.json
    pure plan.json.schema
  where
    targets = Set.fromList sourcesInitial

-- | Write modified source files and update the source hashes.
prepareRebuild :: IncrementalPaths -> IO (LazyByteString, MVar WorkerState)
prepareRebuild IncrementalPaths {srcDir, hashesRebuild, sourcesRebuild} = do
  initUnitSources srcDir unit2_modified
  meta <- writeHashesFromPaths hashesRebuild sourcesRebuild
  state <- newState
  pure (meta, state)

--------------------------------------------------------------------------------
-- Test logic
--------------------------------------------------------------------------------

rebuild ::
  IncrementalPaths ->
  MVar WorkerState ->
  CachedBuildPlans ->
  TestT IO (BuildPlanSchema, SourceHashes)
rebuild IncrementalPaths {buildPlan, incrementalState, hashesRebuild, sourcesRebuild} state unit1Plans = do
  restoreUnit state unit1Plans
  runMetadata "rebuild" state \ logger -> do
    hashes <- lift $ readSourceHashes sources hashesRebuild

    -- Compare Buck's incremental source hashes with the worker's incremental state written in the initial build.
    (changes, cachedJson) <- evalMaybe =<< lift (loadIncrementalState incrementalState hashes sources)

    -- The updated sources consist of the modified/added modules.
    targetUpdated === Set.map takeFileName changes.updated

    -- The invalidated sources consist of the modified/removed modules.
    targetInvalidated === Set.map takeFileName changes.invalidated

    -- Compute the partial graph of modules that remain valid after changes.
    -- This is the cached graph from the initial build without the sources in @changes.invalidated@.
    (cached, removed) <- hscGets (loadCachedGraph True buildPlan changes.invalidated)
    valid <- hscGets (pure . mergeCacheAndDeps cached)
    targetValid === graphNames valid

    -- The downsweep result and the merged graph contain all modules in the modified source tree.
    recomputed <- lift (downsweepIncremental valid (Set.toList changes.updated))
    targetRecomputed === graphNames recomputed

    -- Compute the new build plan for only the changed modules and merge it with the cached build plan.
    freshJson <- lift $ withSession (buildPlanModules bpFields [] [] recomputed)
    finalJson <- evalEither (first showPprUnsafe (mergeBuildPlanJson freshJson (pruneCachedPlan removed cachedJson)))

    -- Ensure that this test isn't outdated by running 'buildPlanIncremental' and comparing the result
    nativePlan <- lift $ buildPlanIncremental True logger bpFields [] [] buildPlan changes cachedJson
    nativePlan.json.schema === finalJson.schema

    pure (finalJson.schema, hashes)
  where
    hscGets f = lift $ withSession (liftIO . f)

    sources = Set.fromList sourcesRebuild

checkSchema :: Unit -> BuildPlanSchema -> TestT IO ()
checkSchema unit BuildPlanSchema {..} = do
  assertJust (Set.fromList ((.mkey) <$> unit.mods)) exposed_modules
  assertJust (targetModuleGraph unit) module_graph
  assertJust (targetProjectDeps unit) project_deps
  assertJust (targetToolchainDeps unit) toolchain_deps
  assertJust (targetPackageDeps unit) package_deps

-- TODO in Buck, paths in source hashes are relative
test_incrementalFlow :: TestTree
test_incrementalFlow =
  withTemp "incremental-flow" \ tmpResource ->
    unitTest "incremental flow" do
      paths <- incrementalPaths . toOsPath <$> liftIO tmpResource
      (meta1, state1, unit1Plans) <- liftIO $ prepareInitialBuild paths
      assertJust targetMetadata1 (Aeson.decode (replaceTemp paths.tempDir meta1))
      initialSchema <- initialBuild paths state1 unit1Plans
      checkSchema unit2 initialSchema
      (meta2, state2) <- liftIO $ prepareRebuild paths
      assertJust targetMetadata2 (Aeson.decode (replaceTemp paths.tempDir meta2))
      (rebuildSchema, rebuildHashes) <- rebuild paths state2 unit1Plans
      checkSchema unit2_modified rebuildSchema
      Map.keysSet (unsafeSourceHashes rebuildHashes) === Set.fromList paths.sourcesRebuild
