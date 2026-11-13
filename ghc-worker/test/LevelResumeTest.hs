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
-- The tests compile the user's module in the worker in four ways, and check
-- the levels of its node's edges to the library, and whether it compiles:
--
-- * In one build, the node has the splice edge, and the module compiles.
--
-- * With both units restored from the cache, the node has one edge to the
--   library, at the normal level, as the cache keeps no levels. The splice
--   that needs the library's instance fails with a level error (GHC-28914),
--   as GHC finds nothing at level -1 through a normal edge. The splices that
--   need an instance from @base@, or no instance, compile.
--
-- * With only the library's unit restored, the user's unit runs its metadata
--   step in the worker, which gives the node its splice edge, and the module
--   compiles.
--
-- * With both units restored and the incremental module graph disabled, the
--   node has the normal edge alone, and the splice that needs the library's
--   instance fails with the level error.
module LevelResumeTest (test_levelResume) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName)
import GHC.Unit.Module.Graph (
  ModNodeKeyWithUid (..),
  ModuleGraphNode (..),
  ModuleNodeEdge (..),
  mnKey,
  )
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), stringToUnitId)
import GHC.Utils.Outputable (ppr, showPprUnsafe)
import Hedgehog (TestT, annotate, assert, (===))
import System.OsPath.Extra (fromOsPath, (</>))
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
import Test.Path (moduleSourcePath)
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree, testGroup)
import Types.Env (Env (..))
import Types.Args (Args (..))
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
-- three splices.
data Splice = BaseInstance | HomeInstance | NameOnly
  deriving stock (Eq, Show)

sourceM :: Splice -> String
sourceM splice =
  unlines [
    "{-# LANGUAGE ExplicitLevelImports #-}",
    "{-# LANGUAGE TemplateHaskell #-}",
    "module Unit2Module1 where",
    "",
    "import splice Unit1Module1 (" ++ imports ++ ")",
    "",
    body
    ]
  where
    (imports, body) = case splice of
      -- @Num Int@, from base
      BaseInstance -> ("mk", "mk 3")
      -- @Count Tag@, defined in the library
      HomeInstance -> ("mk, count, Tag (..)", "mk (count Tag)")
      NameOnly -> ("decls", "decls")

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

-- | Codes of the diagnostics that failed a build.
failureCodes :: BuildResult -> [Integer]
failureCodes result =
  [fromIntegral c | UnexpectedDiagnostics cs <- Map.elems result.failures, c <- Set.toList cs]

-- | Compile both modules in one build, and return the levels of the user's
-- edges to the library and the build's result.
oneBuild :: Splice -> TestEnv -> TestT IO ([String], BuildResult)
oneBuild splice testEnv = do
  env <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  let buildSys = mkBuildSystem 1 env
  result <- liftIO do
    writeSources env splice
    buildSys.runInitialBuild (fst (sortSchedule allTasks))
  levels <- levelsOfEdgesToLibrary env
  pure (levels, result)

-- | Run the initial tasks in one worker, then the rest in a second worker that
-- restores the first build's units from the cache. Return the levels of the
-- user's edges to the library and the second build's result.
resume ::
  (TestEnv -> TestEnv) ->
  Splice ->
  [BuildTask] ->
  TestEnv ->
  TestT IO ([String], BuildResult)
resume adjust splice initialTasks testEnv = do
  env <- liftIO (newSessionEnv (adjust (enableLazyByteCode testEnv)))
  let buildSys = mkBuildSystem 1 env
      build = projectBuild initialTasks
  initialResult <- liftIO do
    writeSources env splice
    buildSys.runInitialBuild build.schedule
  assertNoFailures "initial" initialResult
  cached <- liftIO (setupResumeBuild buildSys env build initialResult)
  resumeEnv <- liftIO (newResumeSessionEnv env)
  result <- liftIO (executeResumeBuild buildSys resumeEnv build initialResult cached)
  annotate ("failure codes: " ++ show (failureCodes result))
  levels <- levelsOfEdgesToLibrary resumeEnv
  pure (levels, result)

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
      unitTest "in one build, an import splice gives a splice edge, and the module compiles" do
        (levels, result) <- oneBuild HomeInstance =<< liftIO getEnv
        levels === ["SpliceLevel"]
        assertNoFailures "one build" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "restored from the cache, an import splice gives a normal edge, and a splice that "
            ++ "needs the imported module's instance fails with a level error"
        )
        do
          (levels, result) <- resume id HomeInstance bothRestored =<< liftIO getEnv
          levels === ["NormalLevel"]
          assert (levelError result),
    withTestEnv \ getEnv ->
      unitTest "restored from the cache, a splice that needs an instance from base compiles" do
        (levels, result) <- resume id BaseInstance bothRestored =<< liftIO getEnv
        levels === ["NormalLevel"]
        assertNoFailures "instance from base" result,
    withTestEnv \ getEnv ->
      unitTest "restored from the cache, a splice that needs no instance compiles" do
        (levels, result) <- resume id NameOnly bothRestored =<< liftIO getEnv
        levels === ["NormalLevel"]
        assertNoFailures "no instance" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "with only the imported unit restored, the importing unit's metadata step gives "
            ++ "the splice edge, and the module compiles"
        )
        do
          (levels, result) <- resume id HomeInstance libraryRestored =<< liftIO getEnv
          levels === ["SpliceLevel"]
          assertNoFailures "imported unit restored" result,
    withTestEnv \ getEnv ->
      unitTest
        ( "restored without the incremental module graph, an import splice gives a normal "
            ++ "edge, and the splice that needs the imported module's instance fails"
        )
        do
          (levels, result) <- resume noIncrModGraph HomeInstance bothRestored =<< liftIO getEnv
          levels === ["NormalLevel"]
          assert (levelError result)
  ]

#else

import Test.Tasty (TestTree, testGroup)

-- | GHC 9.10 has no explicit level imports.
test_levelResume :: TestTree
test_levelResume = testGroup "explicit level imports in restored units" []

#endif
