{-# LANGUAGE CPP #-}
-- | End-to-end tests for the standalone GHC server build pipeline.
--
-- Creates synthetic multi-unit projects in temporary directories and builds
-- them under various scheduling scenarios.
module Test.BuildTest where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName, moduleNameString)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), unitEnv_lookup)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Types (stringToUnit, toUnitId)
import GhcServer.Build (
  Build (..),
  BuildResult (..),
  awaitBuild,
  newBuild,
  newBuildState,
  runBuild,
  scheduleBatch,
  stopBuild,
  )
import GhcServer.Cache (cacheExists)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (BuildEvent (..), BuildEvents, newBuildEvents, readEvents)
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (ClientModule (..), Project (..), Unit (..), UnitCache (..), UnitName (..))
import GhcServer.Data.UnitConfig (UnitConfig (..))
import GhcServer.Log (newLogger)
import GhcServer.Path (osPath)
import GhcServer.Project (discoverProject)
import Hedgehog (TestT, annotate, assert, diff, property, test, withTests, (===))
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, removePathForcibly)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.OsPath (OsPath)
import System.Timeout (timeout)
import Test.Tasty (DependencyType (..), TestName, TestTree, dependentTestGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (emptyArgs)
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

-- ---------------------------------------------------------------------------
-- Low-level helpers
-- ---------------------------------------------------------------------------

acquireTemp :: FilePath -> IO FilePath
acquireTemp name = do
  tmpBase <- getCanonicalTemporaryDirectory
  createTempDirectory tmpBase name

-- | Use a temp dir for a Tasty test.
-- We use this instead of @withSystemTempDirectory@ because 'TestT' doesn't have 'MonadMask'.
withTemp :: FilePath -> (IO FilePath -> TestTree) -> TestTree
withTemp name =
  withResource (acquireTemp name) removePathForcibly

writeProjectFile :: FilePath -> FilePath -> String -> IO ()
writeProjectFile base rel content =
  writeFile (base ++ "/" ++ rel) content

writeUnitConfig :: FilePath -> FilePath -> UnitConfig -> IO ()
writeUnitConfig base unitDir config =
  LBS.writeFile (base ++ "/" ++ unitDir ++ "/unit.json") (encode config)

baseGhcArgs :: [String]
baseGhcArgs = []

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

-- | Discovered project environment, created once per test.
data TestProject =
  TestProject {
    root :: FilePath,
    rootOs :: OsPath,
    project :: Project,
    outputDir :: OsPath,
    tmpDir :: OsPath
  }

acquireProject :: IO FilePath -> IO TestProject
acquireProject acquireRoot = do
  root <- acquireRoot
  let
    rootOs = osPath root
    outputDir = osPath (root ++ "/output")
    tmpDir = osPath (root ++ "/tmp")
  project <- discoverProject rootOs outputDir tmpDir
  pure TestProject {root, rootOs, project, outputDir, tmpDir}

newBuildEnv :: TestProject -> MVar WorkerState -> IO (BuildEnv, BuildEvents)
newBuildEnv tp stateVar = do
  log <- newLogger False
  events <- newBuildEvents
  pure (BuildEnv {
    baseArgs = emptyArgs Map.empty,
    projectRoot = tp.rootOs,
    outputDir = tp.outputDir,
    tmpDir = tp.tmpDir,
    stateVar,
    project = tp.project,
    log,
    events
  }, events)

-- ---------------------------------------------------------------------------
-- Build operations (MonadIO)
-- ---------------------------------------------------------------------------

type Steps = [(UnitName, UnitRequest)]

-- | Per-task timeout for tests (seconds).  Matches the scheduler's @taskTimeout@.
testTaskTimeout :: Int
testTaskTimeout = 3

-- | Overall build timeout for tests (microseconds).  Covers scheduler-level deadlocks.
testBuildTimeoutUs :: Int
testBuildTimeoutUs = 10 * 1_000_000

-- | Wrap a build action with an overall timeout.  Fails hard if the build does not
-- complete within 'testBuildTimeoutUs', covering scheduler-level hangs that the
-- per-task timeout cannot catch.
timedBuild :: IO a -> IO a
timedBuild action =
  timeout testBuildTimeoutUs action >>= \ case
    Just a  -> pure a
    Nothing -> fail ("Build deadlocked (timed out after " ++ show (testBuildTimeoutUs `div` 1_000_000) ++ "s)")

-- | Run a fresh build with the given schedule steps.
runFresh :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFresh tp steps = liftIO $ timedBuild do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  result <- runBuild 4 testTaskTimeout env ScheduleRequest {steps, recompile = False, rebuild = False}
  evs <- readEvents events
  pure (result, evs)

-- | Run a fresh build with empty request (build everything).
runFreshAll :: MonadIO m => TestProject -> m (BuildResult, [BuildEvent])
runFreshAll tp =
  runFresh tp []

-- | 'stopBuild' wrapped with 'timedBuild'.
timedStop :: Build -> IO BuildResult
timedStop cb = timedBuild (stopBuild cb)

-- | Create a new 'Build' for multi-batch tests.
newTestBuild :: MonadIO m => TestProject -> m (Build, BuildEvents)
newTestBuild tp = liftIO do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  cb <- newBuild 4 testTaskTimeout env
  pure (cb, events)

-- | Run a fresh build with @maxJobs=1@ and return both the result and recorded events.
runFreshWithEvents :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFreshWithEvents tp steps = liftIO $ timedBuild do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  result <- runBuild 1 testTaskTimeout env ScheduleRequest {steps, recompile = False, rebuild = False}
  evs <- readEvents events
  pure (result, evs)

-- | Run a fresh build and return the 'WorkerState' MVar alongside the result and events.
-- The returned 'MVar' can be used to inspect the post-build HPT.
runFreshWithState :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent], MVar WorkerState)
runFreshWithState tp steps = liftIO $ timedBuild do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  result <- runBuild 1 testTaskTimeout env ScheduleRequest {steps, recompile = False, rebuild = False}
  evs <- readEvents events
  pure (result, evs, stateVar)

deleteUnitCache :: MonadIO m => TestProject -> String -> m ()
deleteUnitCache tp name = liftIO do
  removePathForcibly (tp.root ++ "/cache/" ++ name)

-- | Delete per-module @.dyn_hi@ interface files for a unit, leaving the metadata cache
-- and object files intact.
deleteModuleHiFiles :: MonadIO m => TestProject -> String -> m ()
deleteModuleHiFiles tp name = liftIO do
  let outputUnitDir = tp.root ++ "/output/" ++ name
  entries <- listDirectory outputUnitDir
  mapM_ removeFile [outputUnitDir ++ "/" ++ e | e <- entries, ".dyn_hi" `isSuffixOf` e]

-- ---------------------------------------------------------------------------
-- Event extraction
-- ---------------------------------------------------------------------------

-- | Extract unit names for which metadata ran.
eventMetadata :: [BuildEvent] -> [String]
eventMetadata events =
  sort [name.string | MetadataRan name <- events]

-- | Extract "unit:module" strings for compiled modules.
eventCompiled :: [BuildEvent] -> [String]
eventCompiled events =
  sort [name.string ++ ":" ++ moduleNameString modName | ModuleCompiled name modName <- events]

-- | Extract unit names that had at least one module compiled.
eventCompiledUnits :: [BuildEvent] -> [String]
eventCompiledUnits events =
  sort $ Set.toList $ Set.fromList [name.string | ModuleCompiled name _ <- events]

-- | Extract unit names that had at least one module skipped.
eventSkippedUnits :: [BuildEvent] -> [String]
eventSkippedUnits events =
  sort $ Set.toList $ Set.fromList [name.string | CompileSkipped name _ <- events]

-- ---------------------------------------------------------------------------
-- Assertions
-- ---------------------------------------------------------------------------

prettyBuildResult :: String -> BuildResult -> String
prettyBuildResult label result =
  unlines $
    [label ++ ":",
     "  success: " ++ show result.success,
     "  metadata errors:"]
    ++ ["    " ++ u.string ++ ": " ++ msg | (u, msg) <- result.metadataErrors]
    ++ ["  compile errors:"]
    ++ ["    " ++ u.string ++ ":" ++ show modName ++ ": " ++ msg | (u, modName, msg) <- result.compileErrors]

assertSuccess :: HasCallStack => String -> BuildResult -> TestT IO ()
assertSuccess label result =
  withFrozenCallStack do
    annotate (prettyBuildResult label result)
    assert result.success

assertHasMetadata :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertHasMetadata unitName events =
  withFrozenCallStack do
    diff unitName elem (eventMetadata events)

assertNoMetadata :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertNoMetadata unitName events =
  withFrozenCallStack do
    diff unitName notElem (eventMetadata events)

assertHasCompiled :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertHasCompiled unitName events =
  withFrozenCallStack do
    diff unitName elem (eventCompiledUnits events)

assertNoCompiled :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertNoCompiled unitName events =
  withFrozenCallStack do
    diff unitName notElem (eventCompiledUnits events)

assertHasSkipped :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertHasSkipped unitName events =
  withFrozenCallStack do
    diff unitName elem (eventSkippedUnits events)

assertNoSkipped :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertNoSkipped unitName events =
  withFrozenCallStack do
    diff unitName notElem (eventSkippedUnits events)

assertCacheExists :: HasCallStack => TestProject -> String -> TestT IO ()
assertCacheExists tp name =
  withFrozenCallStack do
    exists <- liftIO (cacheExists (unitCache tp.project (UnitName name)))
    assert exists

unitCache :: Project -> UnitName -> UnitCache
unitCache project name =
  case Map.lookup name project.units of
    Just unit -> unit.cache
    Nothing -> error ("Unit not found: " ++ name.string)

-- | Look up a module in a specific unit's HPT from the 'WorkerState'.
lookupHptModule :: MVar WorkerState -> String -> String -> IO Bool
lookupHptModule stateVar unitStr modStr = do
  state <- readMVar stateVar
  let uid = toUnitId (stringToUnit unitStr)
      hue = unitEnv_lookup uid state.make.hug
      hpt = homeUnitEnv_hpt hue
      modName = mkModuleName modStr
  maybe False (const True) <$> lookupHpt hpt modName

-- | Assert that a module is present in a unit's HPT in the 'WorkerState'.
assertHptHasModule :: HasCallStack => MVar WorkerState -> String -> String -> TestT IO ()
assertHptHasModule stateVar unitStr modStr =
  withFrozenCallStack do
    present <- liftIO (lookupHptModule stateVar unitStr modStr)
    annotate ("Expected module " ++ modStr ++ " in unit " ++ unitStr ++ " HPT")
    assert present

-- ---------------------------------------------------------------------------
-- Project-scoped test combinators
-- ---------------------------------------------------------------------------

-- | Run a test with the small 2-unit project.
smallTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
smallTest =
  projectTest "ghc-server-small" createSmallProject

-- | Run a test with the 4-unit project.
largeTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
largeTest =
  projectTest "ghc-server-large" createLargeProject

-- | Run a test with the 3-unit chain project.
chainTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
chainTest =
  projectTest "ghc-server-chain" createChainProject

-- | Run a test with the intra-dep project.
intraDepTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
intraDepTest =
  projectTest "ghc-server-intradep" createIntraDepProject

projectTest :: FilePath -> (FilePath -> IO ()) -> TestName -> (TestProject -> TestT IO ()) -> TestTree
projectTest dirName create name body =
  withTemp dirName \ acquire ->
    testProperty name $ withTests 1 $ property $ test do
      tp <- liftIO do
        root <- acquire
        create root
        acquireProject (pure root)
      body tp

-- ---------------------------------------------------------------------------
-- Two-unit project: unit0 (leaf) \u2192 unit1 (depends on unit0)
-- ---------------------------------------------------------------------------

createSmallProject :: FilePath -> IO ()
createSmallProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  createDirectoryIfMissing True (root ++ "/unit1")

  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/A.hs" $ unlines
    [ "module A where"
    , ""
    , "hello :: String"
    , "hello = \"hello from unit0\""
    ]

  writeUnitConfig root "unit1" UnitConfig {deps = ["unit0"], args = baseGhcArgs}
  writeProjectFile root "unit1/B.hs" $ unlines
    [ "module B where"
    , ""
    , "import A (hello)"
    , ""
    , "greeting :: String"
    , "greeting = hello ++ \" world\""
    ]

-- ---------------------------------------------------------------------------
-- Four-unit project: unit0 (leaf), unit1/unit2 (dep on unit0),
-- unit3 (dep on unit1 + unit2).
-- Four modules each.
-- ---------------------------------------------------------------------------

createLargeProject :: FilePath -> IO ()
createLargeProject root = do
  writeUnit "unit0" [] \ u -> do
    writeModule u "A0" [] "a0 = \"a0\""
    writeModule u "B0" [] "b0 = \"b0\""
    writeModule u "C0" [] "c0 = \"c0\""
    writeModule u "D0" [] "d0 = \"d0\""

  writeUnit "unit1" ["unit0"] \ u -> do
    writeModule u "A1" ["A0"] "a1 = a0 ++ \"_a1\""
    writeModule u "B1" [] "b1 = \"b1\""
    writeModule u "C1" [] "c1 = \"c1\""
    writeModule u "D1" [] "d1 = \"d1\""

  writeUnit "unit2" ["unit0"] \ u -> do
    writeModule u "A2" ["B0"] "a2 = b0 ++ \"_a2\""
    writeModule u "B2" [] "b2 = \"b2\""
    writeModule u "C2" [] "c2 = \"c2\""
    writeModule u "D2" [] "d2 = \"d2\""

  writeUnit "unit3" ["unit1", "unit2"] \ u -> do
    writeModule u "A3" ["A1"] "a3 = a1 ++ \"_a3\""
    writeModule u "B3" ["A2"] "b3 = a2 ++ \"_b3\""
    writeModule u "C3" [] "c3 = \"c3\""
    writeModule u "D3" [] "d3 = \"d3\""
  where
    writeUnit name deps body = do
      let dir = root ++ "/" ++ name
      createDirectoryIfMissing True dir
      writeUnitConfig root name UnitConfig {deps, args = baseGhcArgs}
      body name

    writeModule unitName modName imports body =
      writeProjectFile root (unitName ++ "/" ++ modName ++ ".hs") $ unlines $
        ["module " ++ modName ++ " where"]
        ++ ["import " ++ imp ++ " (" ++ lcFirst imp ++ ")" | imp <- imports]
        ++ ["", lcFirst modName ++ " :: String", body]

    lcFirst (c : cs) = toLower c : cs
    lcFirst [] = []

-- ---------------------------------------------------------------------------
-- Three-unit chain: unit0 -> unit1 -> unit2 (leaf)
-- Each unit has 2 modules. unit0 imports from unit1, unit1 imports from unit2.
-- ---------------------------------------------------------------------------

createChainProject :: FilePath -> IO ()
createChainProject root = do
  writeUnit "unit2" [] \ u -> do
    writeModule u "A2" [] "a2 = \"a2\""
    writeModule u "B2" [] "b2 = \"b2\""

  writeUnit "unit1" ["unit2"] \ u -> do
    writeModule u "A1" ["A2"] "a1 = a2 ++ \"_a1\""
    writeModule u "B1" [] "b1 = \"b1\""

  writeUnit "unit0" ["unit1"] \ u -> do
    writeModule u "A0" ["A1"] "a0 = a1 ++ \"_a0\""
    writeModule u "B0" [] "b0 = \"b0\""
  where
    writeUnit name deps body = do
      let dir = root ++ "/" ++ name
      createDirectoryIfMissing True dir
      writeUnitConfig root name UnitConfig {deps, args = baseGhcArgs}
      body name

    writeModule unitName modName imports body =
      writeProjectFile root (unitName ++ "/" ++ modName ++ ".hs") $ unlines $
        ["module " ++ modName ++ " where"]
        ++ ["import " ++ imp ++ " (" ++ lcFirst imp ++ ")" | imp <- imports]
        ++ ["", lcFirst modName ++ " :: String", body]

    lcFirst (c : cs) = toLower c : cs
    lcFirst [] = []

-- ---------------------------------------------------------------------------
-- Intra-dep project: unit0 (U0M0), unit1 -> unit0 (U1M0, U1M1 -> [U1M0, U0M0])
-- ---------------------------------------------------------------------------

createIntraDepProject :: FilePath -> IO ()
createIntraDepProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/U0M0.hs" $ unlines
    [ "module U0M0 where"
    , ""
    , "u0m0 :: String"
    , "u0m0 = \"u0m0\""
    ]

  createDirectoryIfMissing True (root ++ "/unit1")
  writeUnitConfig root "unit1" UnitConfig {deps = ["unit0"], args = baseGhcArgs}
  writeProjectFile root "unit1/U1M0.hs" $ unlines
    [ "module U1M0 where"
    , ""
    , "u1m0 :: String"
    , "u1m0 = \"u1m0\""
    ]
  writeProjectFile root "unit1/U1M1.hs" $ unlines
    [ "module U1M1 where"
    , ""
    , "import U1M0 (u1m0)"
    , "import U0M0 (u0m0)"
    , ""
    , "u1m1 :: String"
    , "u1m1 = u1m0 ++ u0m0"
    ]

-- ---------------------------------------------------------------------------
-- Test group: Basic dispatch
-- ---------------------------------------------------------------------------

test_buildAll :: TestTree
test_buildAll =
  smallTest "build entire project" \ tp -> do
    (result, events) <- runFreshAll tp
    assertSuccess "build all" result
    assertHasMetadata "unit0" events
    assertHasMetadata "unit1" events
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events

test_metadataOnly :: TestTree
test_metadataOnly =
  smallTest "metadata only" \ tp -> do
    (result, events) <- runFresh tp [(UnitName "unit0", UnitMetadata)]
    assertSuccess "metadata only" result
    assertHasMetadata "unit0" events
    assertNoCompiled "unit0" events
    assertNoMetadata "unit1" events

test_singleUnit :: TestTree
test_singleUnit =
  smallTest "build single unit" \ tp -> do
    (result, events) <- runFresh tp [(UnitName "unit0", UnitAll)]
    assertSuccess "single unit" result
    assertHasMetadata "unit0" events
    assertHasCompiled "unit0" events
    assertNoMetadata "unit1" events
    assertNoCompiled "unit1" events

test_specificModule :: TestTree
test_specificModule =
  smallTest "build specific module" \ tp -> do
    (result, _events) <- runFresh tp
      [ (UnitName "unit0", UnitAll)
      , (UnitName "unit1", UnitMetadata)
      , (UnitName "unit1", UnitModules [ClientModule "B"])
      ]
    assertSuccess "specific module" result

test_modulesOnly :: TestTree
test_modulesOnly =
  smallTest "modules only after prior build" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, _) <- runFresh tp
      [ (UnitName "unit0", UnitMetadata)
      , (UnitName "unit1", UnitModulesOnly)
      ]
    assertSuccess "modules only" result2

test_basicDispatch :: TestTree
test_basicDispatch =
  dependentTestGroup "Basic dispatch" AllFinish
    [ test_buildAll
    , test_metadataOnly
    , test_singleUnit
    , test_specificModule
    , test_modulesOnly
    ]

-- ---------------------------------------------------------------------------
-- Test group: Cache restore
-- ---------------------------------------------------------------------------

test_cacheRestoreAll :: TestTree
test_cacheRestoreAll =
  smallTest "full rebuild from cache" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "first build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    (result2, events2) <- runFreshAll tp
    assertSuccess "second build" result2
    [] === eventMetadata events2

test_cacheMetadataNoOp :: TestTree
test_cacheMetadataNoOp =
  smallTest "metadata-only for cached unit is a no-op" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, events2) <- runFresh tp [(UnitName "unit0", UnitMetadata)]
    assertSuccess "cached metadata" result2
    [] === eventMetadata events2
    [] === eventCompiled events2

test_cacheModulesOnly :: TestTree
test_cacheModulesOnly =
  smallTest "modules-only for cached unit" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitModulesOnly)]
    assertSuccess "cached modules-only" result2
    assertNoMetadata "unit1" events2
    assertHasCompiled "unit1" events2
    assertNoMetadata "unit0" events2

test_cacheMixedFreshAndCached :: TestTree
test_cacheMixedFreshAndCached =
  smallTest "mixed cached and fresh units" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    deleteUnitCache tp "unit1"
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitAll)]
    assertSuccess "rebuild unit1" result2
    assertNoMetadata "unit0" events2
    assertHasMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheSpecificModules :: TestTree
test_cacheSpecificModules =
  smallTest "specific modules for cached unit" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitModules [ClientModule "B"])]
    assertSuccess "cached specific module" result2
    assertNoMetadata "unit0" events2
    assertNoMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheDeleteLeafRebuildsChain :: TestTree
test_cacheDeleteLeafRebuildsChain =
  smallTest "deleting leaf cache: leaf interface present, skip compile" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    deleteUnitCache tp "unit0"
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitAll)]
    assertSuccess "chain rebuild" result2
    assertHasMetadata "unit0" events2
    -- unit0's .dyn_hi files still exist in the output dir, so compile is skipped
    assertHasSkipped "unit0" events2
    assertNoMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheDeleteMiddleUnit :: TestTree
test_cacheDeleteMiddleUnit =
  largeTest "delete middle unit cache in chain" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    deleteUnitCache tp "unit1"
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitAll)]
    assertSuccess "middle unit rebuild" result2
    assertNoMetadata "unit0" events2
    assertHasMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheRestore :: TestTree
test_cacheRestore =
  dependentTestGroup "Cache restore" AllFinish
    [ test_cacheRestoreAll
    , test_cacheMetadataNoOp
    , test_cacheModulesOnly
    , test_cacheMixedFreshAndCached
    , test_cacheSpecificModules
    , test_cacheDeleteLeafRebuildsChain
    , test_cacheDeleteMiddleUnit
    ]

-- ---------------------------------------------------------------------------
-- Test group: Pending pool and promotion
-- ---------------------------------------------------------------------------

test_implicitDeps :: TestTree
test_implicitDeps =
  largeTest "implicit dep units are built" \ tp -> do
    (result, events) <- runFresh tp [(UnitName "unit3", UnitAll)]
    assertSuccess "implicit deps" result
    assertHasMetadata "unit0" events
    assertHasMetadata "unit1" events
    assertHasMetadata "unit2" events
    assertHasMetadata "unit3" events
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events
    assertHasCompiled "unit2" events
    assertHasCompiled "unit3" events

test_pendingThenEnable :: TestTree
test_pendingThenEnable =
  smallTest "pending tasks enabled by later batch" \ tp -> do
    (cb, evRef) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitMetadata)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "pending then enable" result
    assertHasMetadata "unit0" events
    assertHasCompiled "unit0" events

test_metadataOnlyLeavesTasksPending :: TestTree
test_metadataOnlyLeavesTasksPending =
  smallTest "metadata-only leaves compile tasks pending" \ tp -> do
    (cb, evRef) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitMetadata)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "metadata pending" result
    assertHasMetadata "unit0" events
    assertNoCompiled "unit0" events

test_enabledNotDowngraded :: TestTree
test_enabledNotDowngraded =
  smallTest "enabled flag not downgraded by metadata request" \ tp -> do
    (cb, evRef) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit1", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitMetadata)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "enabled not downgraded" result
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events

test_sameUnitMultipleRequestTypes :: TestTree
test_sameUnitMultipleRequestTypes =
  smallTest "same unit with metadata then all in one batch" \ tp -> do
    (result, events) <- runFresh tp
      [ (UnitName "unit0", UnitMetadata)
      , (UnitName "unit0", UnitAll)
      ]
    assertSuccess "same unit multi" result
    assertHasMetadata "unit0" events
    assertHasCompiled "unit0" events

test_metadataOnlyForDep :: TestTree
test_metadataOnlyForDep =
  smallTest "metadata-only dep with compiled dependent" \ tp -> do
    (result, events) <- runFresh tp
      [ (UnitName "unit0", UnitMetadata)
      , (UnitName "unit1", UnitAll)
      ]
    assertSuccess "metadata dep" result
    assertHasMetadata "unit0" events
    assertHasMetadata "unit1" events
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events

test_pendingPool :: TestTree
test_pendingPool =
  dependentTestGroup "Pending pool and promotion" AllFinish
    [ test_implicitDeps
    , test_pendingThenEnable
    , test_metadataOnlyLeavesTasksPending
    , test_enabledNotDowngraded
    , test_sameUnitMultipleRequestTypes
    , test_metadataOnlyForDep
    ]

-- ---------------------------------------------------------------------------
-- Test group: Multi-batch scheduling
-- ---------------------------------------------------------------------------

test_multiBatch :: TestTree
test_multiBatch =
  largeTest "three batches with overlapping deps" \ tp -> do
    (cb, evRef) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit3", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit1", UnitAll), (UnitName "unit2", UnitAll)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "multi-batch" result
    ["unit0", "unit1", "unit2", "unit3"] === eventMetadata events

test_redundantBatch :: TestTree
test_redundantBatch =
  smallTest "redundant batch for completed units" \ tp -> do
    (cb, _) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {steps = [], recompile = False, rebuild = False}
    result1 <- liftIO (awaitBuild cb)
    assertSuccess "first batch" result1
    liftIO $ scheduleBatch cb ScheduleRequest {steps = [], recompile = False, rebuild = False}
    result2 <- liftIO (awaitBuild cb)
    assertSuccess "redundant batch" result2
    liftIO (cancel cb.thread)

test_stateAccumulation :: TestTree
test_stateAccumulation =
  smallTest "state accumulates across batches" \ tp -> do
    (cb, evRef) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    result1 <- liftIO (awaitBuild cb)
    assertSuccess "batch 1" result1
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit1", UnitAll)],
      recompile = False, rebuild = False
    }
    result2 <- liftIO (timedStop cb)
    assertSuccess "batch 2" result2
    -- Events accumulate across batches, but metadata for unit0 should run exactly once
    events <- liftIO (readEvents evRef)
    let metaUnit0Count = length [() | MetadataRan (UnitName "unit0") <- events]
    1 === metaUnit0Count
    assertHasMetadata "unit1" events

test_multiBatchWithCache :: TestTree
test_multiBatchWithCache =
  largeTest "batches with cache in same scheduler" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "round 1" result1
    (cb, evRef) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit2", UnitAll), (UnitName "unit3", UnitAll)],
      recompile = False, rebuild = False
    }
    result2 <- liftIO (timedStop cb)
    events2 <- liftIO (readEvents evRef)
    assertSuccess "round 2" result2
    [] === eventMetadata events2

test_largeFreshBuild :: TestTree
test_largeFreshBuild =
  largeTest "full fresh build of large project" \ tp -> do
    (result, events) <- runFreshAll tp
    assertSuccess "large fresh" result
    ["unit0", "unit1", "unit2", "unit3"] === eventMetadata events
    ["unit0", "unit1", "unit2", "unit3"] === eventCompiledUnits events
    16 === length (eventCompiled events)

test_multiBatchScheduling :: TestTree
test_multiBatchScheduling =
  dependentTestGroup "Multi-batch scheduling" AllFinish
    [ test_multiBatch
    , test_redundantBatch
    , test_stateAccumulation
    , test_multiBatchWithCache
    , test_largeFreshBuild
    ]

-- ---------------------------------------------------------------------------
-- Test group: Home-unit dep regression
-- ---------------------------------------------------------------------------

-- | Test that when @unit1@ is cached (metadata only) and @U1M1@ imports @U1M0@,
-- requesting only @U1M1@ correctly compiles @U1M0@ first.
test_cachedUnitIntraDep :: TestTree
test_cachedUnitIntraDep =
  intraDepTest "cached unit: U1M1 compiles U1M0 as dep" \ tp -> do
    -- Phase 1: build unit0 fully and run metadata for unit1 only.
    (result1, events1) <- runFresh tp
      [ (UnitName "unit0", UnitAll)
      , (UnitName "unit1", UnitMetadata)
      ]
    assertSuccess "phase 1" result1
    assertHasMetadata "unit0" events1
    assertHasMetadata "unit1" events1
    assertHasCompiled "unit0" events1
    assertNoCompiled "unit1" events1

    assertCacheExists tp "unit1"

    -- Phase 2: fresh WorkerState + fresh scheduler.
    (result2, events2) <- runFresh tp
      [(UnitName "unit1", UnitModules [ClientModule "U1M1"])]
    annotate (prettyBuildResult "phase 2" result2)
    assertSuccess "phase 2" result2
    assertHasCompiled "unit1" events2
    assertNoMetadata "unit1" events2

test_homeUnitDep :: TestTree
test_homeUnitDep =
  dependentTestGroup "Home-unit dep regression" AllFinish
    [ test_cachedUnitIntraDep
    ]

-- ---------------------------------------------------------------------------
-- Test group: Build event flows
-- ---------------------------------------------------------------------------

-- | Filter events to only metadata and resolution events (not per-module compile detail).
metaEvents :: [BuildEvent] -> [BuildEvent]
metaEvents =
  filter isMeta
  where
    isMeta = \case
      MetadataSkipped {} -> True
      MetadataRan {} -> True
      ResolutionComputed {} -> True
      _ -> False

assertEventsContain :: HasCallStack => [BuildEvent] -> [BuildEvent] -> TestT IO ()
assertEventsContain expected actual =
  withFrozenCallStack do
    annotate ("Expected events (subset):\n" ++ unlines (map show expected))
    annotate ("Actual events:\n" ++ unlines (map show actual))
    mapM_ (\e -> diff e elem actual) expected

assertNoEvent :: HasCallStack => (BuildEvent -> Bool) -> [BuildEvent] -> TestT IO ()
assertNoEvent predicate actual =
  withFrozenCallStack do
    let matches = filter predicate actual
    annotate ("Unexpected events:\n" ++ unlines (map show matches))
    assert (null matches)

test_eventsFreshBuild :: TestTree
test_eventsFreshBuild =
  smallTest "events: fresh build" \ tp -> do
    (result, events) <- runFreshWithEvents tp []
    assertSuccess "fresh build" result
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
        modA = mkModuleName "A"
        modB = mkModuleName "B"
    -- Both units should run metadata fresh
    assertEventsContain [MetadataRan u0, MetadataRan u1] events
    -- Both resolved from cache
    assertEventsContain [ResolutionComputed u0, ResolutionComputed u1] events
    -- No metadata was skipped
    assertNoEvent (\case MetadataSkipped {} -> True; _ -> False) events
    -- Modules were compiled
    assertEventsContain [ModuleCompiled u0 modA, ModuleCompiled u1 modB] events

test_eventsFullCacheRestore :: TestTree
test_eventsFullCacheRestore =
  smallTest "events: full cache restore" \ tp -> do
    -- First build: populate cache
    (result1, _) <- runFresh tp []
    assertSuccess "initial build" result1
    -- Second build: everything from cache
    (result2, events) <- runFreshWithEvents tp []
    assertSuccess "cache restore" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- Both units should skip metadata
    assertEventsContain [MetadataSkipped u0, MetadataSkipped u1] events
    -- Both resolved from cache
    assertEventsContain [ResolutionComputed u0, ResolutionComputed u1] events
    -- No fresh metadata
    assertNoEvent (\case MetadataRan {} -> True; _ -> False) events

test_eventsDeleteLeafCache :: TestTree
test_eventsDeleteLeafCache =
  smallTest "events: delete leaf cache" \ tp -> do
    -- First build: populate cache
    (result1, _) <- runFresh tp []
    assertSuccess "initial build" result1
    -- Delete unit0's cache
    deleteUnitCache tp "unit0"
    -- Rebuild: unit0 fresh, unit1 cached
    (result2, events) <- runFreshWithEvents tp []
    assertSuccess "rebuild" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0: fresh metadata, unit1: skipped
    assertEventsContain [MetadataRan u0, MetadataSkipped u1] events
    -- Both units resolved
    assertEventsContain [ResolutionComputed u0, ResolutionComputed u1] events

test_eventsMixedCacheFresh :: TestTree
test_eventsMixedCacheFresh =
  smallTest "events: mixed cached and fresh" \ tp -> do
    -- First build
    (result1, _) <- runFresh tp []
    assertSuccess "initial build" result1
    -- Delete unit1's cache, keep unit0
    deleteUnitCache tp "unit1"
    -- Rebuild unit1 explicitly
    (result2, events) <- runFreshWithEvents tp [(UnitName "unit1", UnitAll)]
    assertSuccess "rebuild" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0 is an implicit dep, still cached -> skip
    assertEventsContain [MetadataSkipped u0] events
    -- unit1 is fresh
    assertEventsContain [MetadataRan u1] events
    -- Both units resolved
    assertEventsContain [ResolutionComputed u1, ResolutionComputed u0] events

test_eventsMetadataOnly :: TestTree
test_eventsMetadataOnly =
  smallTest "events: metadata only" \ tp -> do
    (result, events) <- runFreshWithEvents tp [(UnitName "unit0", UnitMetadata)]
    assertSuccess "metadata only" result
    let u0 = UnitName "unit0"
    -- Metadata ran
    assertEventsContain [MetadataRan u0] events
    assertEventsContain [ResolutionComputed u0] events
    -- No modules compiled
    assertNoEvent (\case ModuleCompiled {} -> True; _ -> False) events
    -- No unit1 activity
    assertNoEvent (\case MetadataRan (UnitName "unit1") -> True; MetadataSkipped (UnitName "unit1") -> True; _ -> False) events

test_eventFlow :: TestTree
test_eventFlow =
  dependentTestGroup "Build event flows" AllFinish
    [ test_eventsFreshBuild
    , test_eventsFullCacheRestore
    , test_eventsDeleteLeafCache
    , test_eventsMixedCacheFresh
    , test_eventsMetadataOnly
    ]

-- ---------------------------------------------------------------------------
-- Test group: Implicit dep compile skip
-- ---------------------------------------------------------------------------

-- | Both units fully cached. Request @unit1:modules@.
-- unit0 is an implicit dep with compilation artifacts cached (CachedDeps exist).
-- Expected: unit0's modules are skipped (CompileSkipped), unit1's modules are compiled.
test_implicitDepCachedSkip :: TestTree
test_implicitDepCachedSkip =
  smallTest "implicit dep: cached modules skipped" \ tp -> do
    -- Phase 1: full build to populate all caches
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    -- Phase 2: fresh WorkerState, request unit1:modules
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit1", UnitModulesOnly)]
    assertSuccess "cached skip" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0 metadata skipped (cached)
    assertEventsContain [MetadataSkipped u0] events2
    -- unit1 metadata skipped (cached, UnitModulesOnly skips metadata)
    assertEventsContain [MetadataSkipped u1] events2
    -- unit0's modules skipped because CachedDeps exist and it's not explicitly requested
    assertEventsContain [CompileSkipped u0 (mkModuleName "A")] events2
    assertHasSkipped "unit0" events2
    assertNoCompiled "unit0" events2
    -- unit1's modules compiled because it's explicitly requested
    assertEventsContain [ModuleCompiled u1 (mkModuleName "B")] events2
    assertHasCompiled "unit1" events2
    assertNoSkipped "unit1" events2

-- | Both units have metadata cached. Request @unit1:modules@.
-- unit0's @.dyn_hi@ interface files are deleted (compilation artifacts unavailable).
-- Expected: unit0's modules are compiled (not skipped), unit1's modules are compiled.
test_implicitDepNoCacheCompiled :: TestTree
test_implicitDepNoCacheCompiled =
  smallTest "implicit dep: uncached modules compiled" \ tp -> do
    -- Phase 1: full build to populate all caches
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    -- Delete unit0's interface files but keep metadata cache
    deleteModuleHiFiles tp "unit0"
    -- Phase 2: fresh WorkerState, request unit1:modules
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit1", UnitModulesOnly)]
    assertSuccess "uncached compile" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0 metadata skipped (metadata cache still exists)
    assertEventsContain [MetadataSkipped u0] events2
    -- unit1 metadata skipped (cached)
    assertEventsContain [MetadataSkipped u1] events2
    -- unit0's modules compiled because .dyn_hi files don't exist
    assertEventsContain [ModuleCompiled u0 (mkModuleName "A")] events2
    assertHasCompiled "unit0" events2
    assertNoSkipped "unit0" events2
    -- unit1's modules compiled because it's explicitly requested
    assertEventsContain [ModuleCompiled u1 (mkModuleName "B")] events2
    assertHasCompiled "unit1" events2

test_implicitDepCompileSkip :: TestTree
test_implicitDepCompileSkip =
  dependentTestGroup "Implicit dep compile skip" AllFinish
    [ test_implicitDepCachedSkip
    , test_implicitDepNoCacheCompiled
    ]

-- ---------------------------------------------------------------------------
-- Test group: HPT assembly
-- ---------------------------------------------------------------------------

-- | After a full cache restore (both metadata and some compiles skipped),
-- verify that cross-unit dep modules are present in the HPT.
--
-- This is the scenario that triggers the @hugSomeThingsBelowUs@ warning:
-- unit0:A is skipped (implicit dep, cached), unit1:B is compiled.
-- If the HPT is correctly assembled, unit0:A should be present in unit0's HPT
-- because 'loadCachedDeps' (or 'loadHomeUnit') loaded it before compiling B.
test_hptCacheRestore :: TestTree
test_hptCacheRestore =
  smallTest "HPT: cache restore populates cross-unit deps" \ tp -> do
    -- Phase 1: full build to populate cache and CachedDeps
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    -- Phase 2: fresh WorkerState, full cache restore
    (result2, events, stateVar) <- runFreshWithState tp []
    assertSuccess "cache restore" result2
    -- Both metadata skipped (cached)
    assertEventsContain [MetadataSkipped (UnitName "unit0"), MetadataSkipped (UnitName "unit1")] events
    -- unit1:B was compiled (either fresh or from cache)
    -- unit0:A should be in unit0's HPT so that unit1:B compilation
    -- can find it via hugSomeThingsBelowUs
    assertHptHasModule stateVar "unit0" "A"
    assertHptHasModule stateVar "unit1" "B"

-- | After a cache restore where the leaf unit's @.dyn_hi@ files are
-- deleted, the HPT should still be correctly populated because the
-- compile step runs (not skipped) and adds the module to the HPT.
test_hptCacheRestoreNoCachedDeps :: TestTree
test_hptCacheRestoreNoCachedDeps =
  smallTest "HPT: cache restore without interface files" \ tp -> do
    -- Phase 1: full build
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    -- Delete unit0's interface files but keep metadata cache
    deleteModuleHiFiles tp "unit0"
    -- Phase 2: fresh WorkerState
    (result2, _events, stateVar) <- runFreshWithState tp []
    assertSuccess "cache restore" result2
    -- unit0:A should still be in the HPT (it was recompiled, not skipped)
    assertHptHasModule stateVar "unit0" "A"
    assertHptHasModule stateVar "unit1" "B"

-- | Build only @unit0@ in session 1, then @unit1@ in session 2 (fresh 'WorkerState').
-- @unit1:B@ imports @unit0:A@, so the compilation of @B@ needs @A@'s interface in the HPT.
-- Since @unit0@ was fully built in session 1, its @.dyn_hi@ is on disk and the implicit
-- dep's compile task is skipped.  @B@ still compiles because 'CachedDeps' are assembled
-- from the module map (populated from @cached_unit.json@).
test_hptCrossSessionCachedDeps :: TestTree
test_hptCrossSessionCachedDeps =
  smallTest "HPT: cross-session implicit dep skip" \ tp -> do
    -- Session 1: build unit0 fully
    (result1, events1) <- runFreshWithEvents tp [(UnitName "unit0", UnitAll)]
    assertSuccess "session 1" result1
    assertHasMetadata "unit0" events1
    assertHasCompiled "unit0" events1
    assertNoMetadata "unit1" events1
    assertNoCompiled "unit1" events1
    assertCacheExists tp "unit0"
    -- Session 2: fresh WorkerState, build unit1
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit1", UnitAll)]
    assertSuccess "session 2" result2
    assertHasMetadata "unit1" events2
    assertHasCompiled "unit1" events2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
        modA = mkModuleName "A"
        modB = mkModuleName "B"
    -- unit0 is an implicit dep with cached artifacts from session 1.
    -- Its compile task is skipped since the .dyn_hi exists.
    assertEventsContain [CompileSkipped u0 modA] events2
    -- B imports A from unit0 — compilation succeeds because CachedDeps are
    -- assembled from the module map (populated from cached_unit.json).
    assertEventsContain [ModuleCompiled u1 modB] events2

test_hptAssembly :: TestTree
test_hptAssembly =
  dependentTestGroup "HPT assembly" AllFinish
    [ test_hptCacheRestore
    , test_hptCacheRestoreNoCachedDeps
    , test_hptCrossSessionCachedDeps
    ]

-- ---------------------------------------------------------------------------
-- Test group: Transitive dep cache restore
-- ---------------------------------------------------------------------------

-- | Delete the leaf unit's cache in a 3-unit chain (unit0 -> unit1 -> unit2).
-- After a full build, delete unit0's cache and output but keep unit1 and unit2.
-- Rebuild unit0: unit1 and unit2 should be restored from cache.
test_cacheTransitiveChain :: TestTree
test_cacheTransitiveChain =
  chainTest "transitive dep cache: 3-unit chain" \ tp -> do
    -- Phase 1: full build
    (result1, events1) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    assertCacheExists tp "unit2"
    -- Verify initial build compiled everything
    assertHasMetadata "unit0" events1
    assertHasMetadata "unit1" events1
    assertHasMetadata "unit2" events1
    assertHasCompiled "unit0" events1
    assertHasCompiled "unit1" events1
    assertHasCompiled "unit2" events1
    -- Delete unit0's cache and output
    deleteUnitCache tp "unit0"
    liftIO $ removePathForcibly (tp.root ++ "/output/unit0")
    liftIO $ createDirectoryIfMissing True (tp.root ++ "/output/unit0")
    -- Phase 2: rebuild unit0
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit0", UnitAll)]
    annotate (prettyBuildResult "rebuild unit0" result2)
    assertSuccess "rebuild unit0" result2
    assertHasMetadata "unit0" events2
    assertHasCompiled "unit0" events2
    -- unit1 and unit2 should be restored from cache
    assertNoMetadata "unit1" events2
    assertNoMetadata "unit2" events2

test_transitiveDepRestore :: TestTree
test_transitiveDepRestore =
  dependentTestGroup "Transitive dep cache restore" AllFinish
    [ test_cacheTransitiveChain
    , test_cacheTransitiveMultipleRoots
    ]

-- | Delete two root units' cache and output in the 4-unit project.
-- unit3 depends on unit1 and unit2, which depend on unit0.
-- After a full build, delete unit3's cache and output, rebuild unit3.
-- unit0, unit1, and unit2 (transitive deps) should be restored from cache.
test_cacheTransitiveMultipleRoots :: TestTree
test_cacheTransitiveMultipleRoots =
  largeTest "transitive dep cache: 4-unit project" \ tp -> do
    -- Phase 1: full build
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    assertCacheExists tp "unit2"
    assertCacheExists tp "unit3"
    -- Delete unit3's cache and output
    deleteUnitCache tp "unit3"
    liftIO $ removePathForcibly (tp.root ++ "/output/unit3")
    liftIO $ createDirectoryIfMissing True (tp.root ++ "/output/unit3")
    -- Phase 2: rebuild unit3
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit3", UnitAll)]
    annotate (prettyBuildResult "rebuild unit3" result2)
    assertSuccess "rebuild unit3" result2
    assertHasMetadata "unit3" events2
    assertHasCompiled "unit3" events2
    -- all transitive deps should be restored from cache
    assertNoMetadata "unit0" events2
    assertNoMetadata "unit1" events2
    assertNoMetadata "unit2" events2

-- ---------------------------------------------------------------------------
-- Top-level test tree
-- ---------------------------------------------------------------------------

test_serverBuild :: TestTree
test_serverBuild =
  dependentTestGroup "GhcServer.Build" AllFinish
    [ test_basicDispatch
    , test_cacheRestore
    , test_pendingPool
    , test_multiBatchScheduling
    , test_homeUnitDep
    , test_eventFlow
    , test_implicitDepCompileSkip
    , test_hptAssembly
    , test_transitiveDepRestore
    ]

