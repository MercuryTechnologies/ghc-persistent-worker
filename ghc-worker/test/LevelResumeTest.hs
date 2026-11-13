{-# LANGUAGE CPP #-}

-- | Explicit level imports in modules whose unit is restored from the cache.
--
-- The test project has two units with one module each. Unit 1's module, the
-- library, defines two Template Haskell functions, @mk@ and @decls@, a class
-- @Count@ and an instance @Count Tag@. Unit 2's module, the user, imports the
-- library with @import splice@, so the library's code runs only in the user's
-- splices. GHC gives the user's module graph node an edge to the library at
-- splice level, and its level check looks for what a splice needs at level -1
-- through that edge.
--
-- The user runs the library's code in one top-level splice. Plain GHC compiles
-- each of the three splices the tests use:
--
-- * @mk (count Tag)@, which needs the library's instance @Count Tag@
-- * @mk 3@, which needs @Num Int@, an instance from @base@
-- * @decls@, which needs no instance
--
-- The tests compile the user's module in the worker in six ways, and check
-- the levels of its node's edges to the library, and whether it compiles. A
-- restored node must have the edges of the node GHC builds in one build:
--
-- * In one build, the node has the splice edge, and the module compiles. The
--   build plan that the worker writes for the user's unit records the import
--   splice, and leaves the library out of the normal-level dependencies.
--
-- * With both units restored from the cache entries the worker wrote, the
--   node has the splice edge only, as in one build. Each of the three splices
--   compiles.
--
-- * With the library imported at both levels, the restored node has a normal
--   edge and a splice edge to it, as in one build, and the module compiles.
--
-- * With both units restored from a cache entry written before the worker
--   recorded levels, the node has one edge to the library, at the normal
--   level. The splice that needs the library's instance fails with a level
--   error (GHC-28914), as GHC finds nothing at level -1 through a normal edge.
--
-- * With only the library's unit restored, the user's unit runs its metadata
--   step in the worker, which gives the node its splice edge, and the module
--   compiles.
--
-- * With both units restored, with their levels, and the incremental module
--   graph disabled, the node has the splice edge, and the module compiles.
--
-- The test harness writes cache entries from the project description, with
-- every dependency at the normal level and no levels. The cases with levels
-- copy in the user's entry from the build plan the worker wrote, as Buck
-- stores it.
module LevelResumeTest (test_levelResume) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Control.Concurrent.MVar (readMVar)
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (for_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName, moduleNameString)
import GHC.Unit.Module.Graph (
  ModNodeKeyWithUid (..),
  ModuleGraphNode (..),
  ModuleNodeEdge (..),
  mnKey,
  )
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), stringToUnitId, unitIdString)
import GHC.Utils.Outputable (ppr, showPprUnsafe)
import Hedgehog (TestT, annotate, assert, (===))
import System.OsPath.Extra (fromOsPath, toOsPath, (</>))
import Test.BuildSystem (mkBuildSystem)
import Test.Bytecode (enableLazyByteCode)
import Test.Data.BuildSystem (BuildResult (..), BuildSystem (..))
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (
  BuildModule (..),
  BuildTask,
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  ModuleSource (..),
  )
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..), scheduleKeys)
import Test.Data.Scheduler (RequestFailure (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Gen.ProjectBuild (metaTask, moduleTask, sortSchedule)
import Test.Path (cachedUnitPath, moduleSourcePath, unitTmpDir)
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree, testGroup)
import Types.Env (Env (..))
import Types.Args (Args (..))
import Types.CachedDeps (CachedLevelDeps (..), JsonFs (..))
import qualified Types.CachedDeps as Cached
import Types.FeatureFlags (FeatureFlags (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

moduleL :: BuildModule
moduleL =
  BuildModule {
    key = ModuleKey {unit = 1, number = 1, errorVariant = Nothing},
    deps = mempty,
    th = True,
    bindings = 1,
    extDeps = mempty
  }

moduleM :: BuildModule
moduleM =
  BuildModule {
    key = ModuleKey {unit = 2, number = 1, errorVariant = Nothing},
    deps = Set.singleton moduleL.key,
    th = True,
    bindings = 1,
    extDeps = mempty
  }

unit1 :: GenUnit BuildModule
unit1 = GenUnit {key = 1, depUnits = mempty, modules = [moduleL]}

unit2 :: GenUnit BuildModule
unit2 = GenUnit {key = 2, depUnits = Set.singleton 1, modules = [moduleM]}

-- | The library: two Template Haskell functions, one taking an @Int@, and a
-- class with an instance.
sourceL :: String
sourceL =
  unlines [
    "{-# LANGUAGE TemplateHaskell #-}",
    "module Unit1Module1 (mk, decls, Count (..), Tag (..)) where",
    "",
    "import Language.Haskell.TH",
    "",
    "mk :: Int -> Q [Dec]",
    "mk n =",
    "  pure",
    "    [ SigD name (ConT ''Int),",
    "      ValD (VarP name) (NormalB (LitE (IntegerL (fromIntegral n)))) []",
    "    ]",
    "  where",
    "    name = mkName (\"value\" ++ show n)",
    "",
    "decls :: Q [Dec]",
    "decls = mk 0",
    "",
    "class Count a where",
    "  count :: a -> Int",
    "",
    "data Tag = Tag",
    "",
    "instance Count Tag where",
    "  count _ = 7"
    ]

-- | The user, importing the library at splice level only, with one of the
-- three splices, or at both levels, with the splice that needs the library's
-- instance and a normal use of the library's type.
data Splice = BaseInstance | HomeInstance | NameOnly | BothLevels
  deriving stock (Eq, Show)

sourceM :: Splice -> String
sourceM splice =
  unlines [
    "{-# LANGUAGE ExplicitLevelImports #-}",
    "{-# LANGUAGE TemplateHaskell #-}",
    "module Unit2Module1 where",
    "",
    "import splice Unit1Module1 (" ++ imports ++ ")",
    normalImport,
    "",
    body
    ]
  where
    normalImport = case splice of
      BothLevels -> "import Unit1Module1 (Tag (..))"
      _ -> ""

    (imports, body) = case splice of
      -- @Num Int@, from base
      BaseInstance -> ("mk", "mk 3")
      -- @Count Tag@, defined in the library
      HomeInstance -> ("mk, count, Tag (..)", "mk (count Tag)")
      NameOnly -> ("decls", "decls")
      -- @Count Tag@ in the splice, and @Tag@ at the normal level too
      BothLevels -> ("mk, count, Tag (..)", "mk (count Tag)\n\ntag :: Tag\ntag = Tag")

initialProject :: InitialProject
initialProject =
  InitialProject {
    modules = mods,
    modulesSuccess = mods,
    modulesError = mempty,
    unitCount = 2,
    moduleCount = 2
  }
  where
    mods = Map.fromList [(m.key, source m) | m <- [moduleL, moduleM]]
    source BuildModule {deps, th, bindings, extDeps} =
      ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

allTasks :: [BuildTask]
allTasks =
  [ metaTask unit1 unit1.depUnits,
    moduleTask unit1 moduleL,
    metaTask unit2 unit2.depUnits,
    moduleTask unit2 moduleM
  ]

projectBuild :: [BuildTask] -> ProjectBuild
projectBuild initialTasks =
  ProjectBuild {
    initial = initialProject,
    schedule = fst (sortSchedule initialTasks),
    resumePlan = ResumePlan {
      fixErrors = False,
      moduleMutations = mempty,
      depMutations = mempty,
      rebuild = RebuildSet {moduleKeys = mempty, allAffectedKeys = mempty, hasChanges = True}
    },
    resumeSchedule = resumeSchedule',
    allKeys = scheduleKeys resumeSchedule'
  }
  where
    resumeSchedule' = fst (sortSchedule allTasks)

-- | Write the generated sources, then replace the library's and the user's
-- with the hand-written ones.
writeSources :: SessionEnv -> Splice -> IO ()
writeSources env splice = do
  writeProjectSources env.sourceDir initialProject.modules
  writeFile (fromOsPath (env.sourceDir </> moduleSourcePath moduleL.key)) sourceL
  writeFile (fromOsPath (env.sourceDir </> moduleSourcePath moduleM.key)) (sourceM splice)

-- | The levels of the user's edges to the library in a worker's module graph,
-- in order. The user's edges, whatever their target, go into the test's
-- annotations, which show when it fails.
levelsOfEdgesToLibrary :: SessionEnv -> TestT IO [String]
levelsOfEdgesToLibrary env = do
  WorkerState {make = MakeState {moduleGraphNodes}} <- liftIO (readMVar env.env.state)
  let edges =
        concat [deps | ModuleNode deps info <- Map.elems moduleGraphNodes, mnKey info == userKey]
      target ModuleNodeEdge {edgeTargetKey} = showPprUnsafe (ppr edgeTargetKey)
      render edge@ModuleNodeEdge {edgeLevel} = show edgeLevel ++ " -> " ++ target edge
  annotate ("the user's edges: " ++ show (map render edges))
  pure (sort [show edgeLevel | edge@ModuleNodeEdge {edgeLevel} <- edges, target edge == library])
  where
    userKey =
      ModNodeKeyWithUid (GWIB (mkModuleName "Unit2Module1") NotBoot) (stringToUnitId "unit2")
    library = "unit1:Unit1Module1"

-- | The user's cache entry in the build plan that the worker wrote in the
-- user's unit's metadata step, if that step ran.
writtenEntryOfUser :: SessionEnv -> IO (Maybe Aeson.Object)
writtenEntryOfUser env = do
  result <- try (Aeson.decodeFileStrict (fromOsPath planPath))
  let plan = either (\ (_ :: IOException) -> Nothing) id result
  pure do
    Aeson.Object top <- plan
    Aeson.Object cache <- KeyMap.lookup "cache" top
    Aeson.Object entry <- KeyMap.lookup "Unit2Module1" cache
    pure entry
  where
    planPath = env.tempDir </> unitTmpDir 2 </> toOsPath "dep.json"

-- | A field of that entry, unless it is missing or null.
writtenFieldOfUser :: Aeson.Key -> SessionEnv -> IO (Maybe Aeson.Value)
writtenFieldOfUser field env = do
  entry <- writtenEntryOfUser env
  pure do
    value <- KeyMap.lookup field =<< entry
    case value of
      Aeson.Null -> Nothing
      _ -> Just value

-- | The user's splice-level dependencies in that build plan.
writtenSpliceOfUser :: SessionEnv -> IO (Maybe Aeson.Value)
writtenSpliceOfUser = writtenFieldOfUser "splice"

-- | The user's normal-level dependencies on other units in that build plan,
-- as unit and module names.
writtenPackagesOfUser :: SessionEnv -> IO (Maybe [(String, [String])])
writtenPackagesOfUser env = do
  value <- writtenFieldOfUser "packages" env
  pure case Aeson.fromJSON <$> value of
    Just (Aeson.Success packages) -> Just (packageTargets packages)
    _ -> Nothing

-- | The splice-level dependencies, as unit and module names.
spliceTargets :: Aeson.Value -> Maybe [(String, [String])]
spliceTargets value = case Aeson.fromJSON value of
  Aeson.Success CachedLevelDeps {packages} -> Just (packageTargets packages)
  Aeson.Error _ -> Nothing

-- | Dependencies on other units, as unit and module names.
packageTargets :: [Cached.CachedPackageDep] -> [(String, [String])]
packageTargets packages =
  [
    (unitIdString depId.raw, moduleNameString . (.raw) <$> depModules)
    |
    Cached.CachedPackageDep {Cached.id = depId, Cached.modules = depModules} <- packages
  ]

-- | Store the user's cache entry, as the worker wrote it in the build plan of
-- the user's unit, in that unit's cache, as Buck stores it. The test harness
-- writes cache entries from the project description, with every dependency
-- at the normal level and no levels.
copyWrittenEntry :: SessionEnv -> IO ()
copyWrittenEntry env =
  writtenEntryOfUser env >>= \case
    Nothing -> pure ()
    Just entry -> do
      let path = fromOsPath (env.tempDir </> cachedUnitPath 2)
      cachedUnit <- Aeson.decodeFileStrict path
      for_ cachedUnit \ value -> Aeson.encodeFile path (setEntry entry (value :: Aeson.Value))
  where
    setEntry entry = \case
      Aeson.Object top
        | Just (Aeson.Object plan) <- KeyMap.lookup "build_plan" top ->
          let plan' = KeyMap.insert "Unit2Module1" (Aeson.Object entry) plan
          in Aeson.Object (KeyMap.insert "build_plan" (Aeson.Object plan') top)
      other -> other

-- | Codes of the diagnostics that failed a build.
failureCodes :: BuildResult -> [Integer]
failureCodes result =
  [fromIntegral c | UnexpectedDiagnostics cs <- Map.elems result.failures, c <- Set.toList cs]

-- | Compile both modules in one build, and return the levels of the user's
-- edges to the library, the build's result, and the user's splice-level
-- dependencies in the build plan the worker wrote, and its normal-level ones
-- on other units there, as unit and module names.
oneBuild ::
  Splice ->
  TestEnv ->
  TestT IO ([String], BuildResult, Maybe [(String, [String])], Maybe [(String, [String])])
oneBuild splice testEnv = do
  env <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  let buildSys = mkBuildSystem 1 env
  result <- liftIO do
    writeSources env splice
    buildSys.runInitialBuild (fst (sortSchedule allTasks))
  levels <- levelsOfEdgesToLibrary env
  written <- liftIO (writtenSpliceOfUser env)
  normal <- liftIO (writtenPackagesOfUser env)
  annotate ("written splice dependencies: " ++ show written)
  annotate ("written normal dependencies on other units: " ++ show normal)
  pure (levels, result, spliceTargets =<< written, normal)

-- | Whether the cache entries of a resume build are as the worker wrote them,
-- with levels, or as written before levels were recorded.
data Levels = WrittenLevels | NoLevels
  deriving stock (Eq, Show)

-- | Run the initial tasks in one worker, then the rest in a second worker that
-- restores the first build's units from the cache. Return the levels of the
-- user's edges to the library and the second build's result.
resume ::
  (TestEnv -> TestEnv) ->
  Levels ->
  Splice ->
  [BuildTask] ->
  TestEnv ->
  TestT IO ([String], BuildResult)
resume adjust levels splice initialTasks testEnv = do
  env <- liftIO (newSessionEnv (adjust (enableLazyByteCode testEnv)))
  let buildSys = mkBuildSystem 1 env
      build = projectBuild initialTasks
  initialResult <- liftIO do
    writeSources env splice
    buildSys.runInitialBuild build.schedule
  assertNoFailures "initial" initialResult
  cached <- liftIO (setupResumeBuild buildSys env build initialResult)
  liftIO (when (levels == WrittenLevels) (copyWrittenEntry env))
  resumeEnv <- liftIO (newResumeSessionEnv env)
  result <- liftIO (executeResumeBuild buildSys resumeEnv build initialResult cached)
  annotate ("failure codes: " ++ show (failureCodes result))
  edgeLevels <- levelsOfEdgesToLibrary resumeEnv
  pure (edgeLevels, result)

-- | The tasks of the initial build when both units are to be restored:
-- everything but compiling the user.
bothRestored :: [BuildTask]
bothRestored =
  [metaTask unit1 unit1.depUnits, moduleTask unit1 moduleL, metaTask unit2 unit2.depUnits]

-- | The tasks of the initial build when only the library's unit is to be
-- restored.
libraryRestored :: [BuildTask]
libraryRestored = [metaTask unit1 unit1.depUnits, moduleTask unit1 moduleL]

-- | Whether a build failed with GHC's level error, GHC-28914.
levelError :: BuildResult -> Bool
levelError result = 28914 `elem` failureCodes result

noIncrModGraph :: TestEnv -> TestEnv
noIncrModGraph testEnv =
  testEnv {
    baseArgs = testEnv.baseArgs {features = testEnv.baseArgs.features {useIncrModGraph = False}}
  }

test_levelResume :: TestTree
test_levelResume =
  testGroup "explicit level imports in restored units" [
    withTestEnv \ getEnv ->
      unitTest
        ( "in one build, an import splice gives a splice edge, the module compiles, and the "
            ++ "build plan records the import"
        )
        do
          (levels, result, written, normal) <- oneBuild HomeInstance =<< liftIO getEnv
          levels === ["SpliceLevel"]
          assertNoFailures "one build" result
          written === Just [("unit1", ["Unit1Module1"])]
          normal === Just [],
    withTestEnv \ getEnv ->
      unitTest
        ( "restored from the cache with its levels, an import splice gives a splice edge only, and "
            ++ "a splice that needs the imported module's instance compiles"
        )
        do
          (levels, result) <- resume id WrittenLevels HomeInstance bothRestored =<< liftIO getEnv
          levels === ["SpliceLevel"]
          assertNoFailures "restored with levels" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "imported at both levels, the module has a normal edge and a splice edge to the "
            ++ "import, in one build and restored from the cache"
        )
        do
          (fresh, freshResult, _, normal) <- oneBuild BothLevels =<< liftIO getEnv
          fresh === ["NormalLevel", "SpliceLevel"]
          assertNoFailures "one build" freshResult
          normal === Just [("unit1", ["Unit1Module1"])]
          (restored, result) <- resume id WrittenLevels BothLevels bothRestored =<< liftIO getEnv
          restored === fresh
          assertNoFailures "restored" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "restored from a cache entry without levels, an import splice gives a normal edge, "
            ++ "and a splice that needs the imported module's instance fails with a level error"
        )
        do
          (levels, result) <- resume id NoLevels HomeInstance bothRestored =<< liftIO getEnv
          levels === ["NormalLevel"]
          assert (levelError result),
    withTestEnv \ getEnv ->
      unitTest "restored from the cache, a splice that needs an instance from base compiles" do
        (levels, result) <- resume id WrittenLevels BaseInstance bothRestored =<< liftIO getEnv
        levels === ["SpliceLevel"]
        assertNoFailures "instance from base" result,
    withTestEnv \ getEnv ->
      unitTest "restored from the cache, a splice that needs no instance compiles" do
        (levels, result) <- resume id WrittenLevels NameOnly bothRestored =<< liftIO getEnv
        levels === ["SpliceLevel"]
        assertNoFailures "no instance" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "with only the imported unit restored, the importing unit's metadata step gives "
            ++ "the splice edge, and the module compiles"
        )
        do
          (levels, result) <- resume id WrittenLevels HomeInstance libraryRestored =<< liftIO getEnv
          levels === ["SpliceLevel"]
          assertNoFailures "imported unit restored" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "restored without the incremental module graph, an import splice gives a splice "
            ++ "edge, and the splice that needs the imported module's instance compiles"
        )
        do
          (levels, result) <-
            resume noIncrModGraph WrittenLevels HomeInstance bothRestored =<< liftIO getEnv
          levels === ["SpliceLevel"]
          assertNoFailures "restored without the incremental module graph" result
  ]

#else

import Test.Tasty (TestTree, testGroup)

-- | GHC 9.10 has no explicit level imports.
test_levelResume :: TestTree
test_levelResume = testGroup "explicit level imports in restored units" []

#endif
