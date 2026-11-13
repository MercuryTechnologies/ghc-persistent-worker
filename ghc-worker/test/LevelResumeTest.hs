{-# LANGUAGE CPP #-}

-- | Explicit level imports in modules whose unit is restored from the cache.
--
-- Unit 2's module M imports unit 1's module L at splice level (@import splice@) and runs L's code in a top-level
-- splice. In the instance cases, the splice is @mk 3@: the literal needs @Num Int@ at level -1, which GHC finds only
-- through M's splice edge to L and L's edges to @base@. In the name case, the splice is @decls@, which needs no
-- instance. Plain GHC accepts both.
--
-- The cases differ in which units the worker restores from the cache before compiling M:
--
-- * A: none, one build compiles everything
-- * B: unit 2 (M's), and unit 1
-- * B': only unit 1, as unit 2's metadata step runs in the resume build
-- * C: as B, with the name-only splice
-- * D: as B, with the incremental module graph disabled
--
-- Each case prints the edges of M's node, with their levels, to stderr.
module LevelResumeTest (test_levelResume) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Control.Concurrent.MVar (readMVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName)
import GHC.Unit.Module.Graph (ModNodeKeyWithUid (..), ModuleGraphNode (..), ModuleNodeEdge (..), mnKey)
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), stringToUnitId)
import GHC.Utils.Outputable (ppr, showPprUnsafe)
import Hedgehog (TestT, annotate, assert, (===))
import System.IO (hPutStrLn, stderr)
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
import Test.Path (moduleSourcePath, unitTmpDir)
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree, testGroup)
import Types.Env (Env (..))
import Types.Args (Args (..))
import Types.CachedDeps (CachedLevelDeps (..), JsonFs (..))
import qualified Types.CachedDeps as Cached
import GHC.Unit.Types (unitIdString)
import GHC (moduleNameString)
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

-- | L: two splice functions, one taking an @Int@.
sourceL :: String
sourceL =
  unlines [
    "{-# LANGUAGE TemplateHaskell #-}",
    "module Unit1Module1 (mk, decls, Count (..), Tag (..)) where",
    "",
    "import Language.Haskell.TH",
    "",
    "mk :: Int -> Q [Dec]",
    "mk n = pure [SigD name (ConT ''Int), ValD (VarP name) (NormalB (LitE (IntegerL (fromIntegral n)))) []]",
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

-- | M, importing L at splice level only. The splice needs an instance at level -1, or only names.
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
      -- @Count Tag@, defined in L
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
    source BuildModule {deps, th, bindings, extDeps} = ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

allTasks :: [BuildTask]
allTasks = [metaTask unit1 unit1.depUnits, moduleTask unit1 moduleL, metaTask unit2 unit2.depUnits, moduleTask unit2 moduleM]

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

-- | Write the generated sources, then replace L's and M's with the hand-written ones.
writeSources :: SessionEnv -> Splice -> IO ()
writeSources env splice = do
  writeProjectSources env.sourceDir initialProject.modules
  writeFile (fromOsPath (env.sourceDir </> moduleSourcePath moduleL.key)) sourceL
  writeFile (fromOsPath (env.sourceDir </> moduleSourcePath moduleM.key)) (sourceM splice)

-- | Print M's edges, with their levels, from a worker's module graph.
dumpEdgesOfM :: String -> SessionEnv -> TestT IO ()
dumpEdgesOfM label env = do
  WorkerState {make = MakeState {moduleGraphNodes}} <- liftIO (readMVar env.env.state)
  let edges = [deps | ModuleNode deps info <- Map.elems moduleGraphNodes, mnKey info == keyM]
      render ModuleNodeEdge {edgeLevel, edgeTargetKey} = show edgeLevel ++ " -> " ++ showPprUnsafe (ppr edgeTargetKey)
      text = case edges of
        [] -> "no node for M"
        es -> unlines ["  " ++ render e | e <- concat es]
  liftIO (hPutStrLn stderr ("\n[" ++ label ++ "] edges of M:\n" ++ text))
  annotate text
  where
    keyM = ModNodeKeyWithUid (GWIB (mkModuleName "Unit2Module1") NotBoot) (stringToUnitId "unit2")

-- | M's splice-level dependencies in the build plan that the worker wrote in unit 2's metadata step.
writtenSpliceOfM :: SessionEnv -> IO (Maybe Aeson.Value)
writtenSpliceOfM env = do
  plan <- Aeson.decodeFileStrict (fromOsPath (env.tempDir </> unitTmpDir 2 </> toOsPath "dep.json"))
  pure do
    Aeson.Object top <- plan
    Aeson.Object cache <- KeyMap.lookup "cache" top
    Aeson.Object entry <- KeyMap.lookup "Unit2Module1" cache
    value <- KeyMap.lookup "splice" entry
    case value of
      Aeson.Null -> Nothing
      _ -> Just value

-- | The splice-level dependencies, as unit and module names.
spliceTargets :: Aeson.Value -> Maybe [(String, [String])]
spliceTargets value = case Aeson.fromJSON value of
  Aeson.Success CachedLevelDeps {packages} ->
    Just [(unitIdString depId.raw, moduleNameString . (.raw) <$> depModules) | Cached.CachedPackageDep {Cached.id = depId, Cached.modules = depModules} <- packages]
  Aeson.Error _ -> Nothing

-- | Codes of the diagnostics that failed a build.
failureCodes :: BuildResult -> [Integer]
failureCodes result = [fromIntegral c | UnexpectedDiagnostics cs <- Map.elems result.failures, c <- Set.toList cs]

-- | Case A: one build compiles both modules.
fresh :: Splice -> TestEnv -> TestT IO ()
fresh splice testEnv = do
  env <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  let buildSys = mkBuildSystem 1 env
  result <- liftIO do
    writeSources env splice
    buildSys.runInitialBuild (fst (sortSchedule allTasks))
  dumpEdgesOfM "A fresh" env
  assertNoFailures "fresh" result
  written <- liftIO (writtenSpliceOfM env)
  annotate ("written splice deps of M: " ++ show written)
  (spliceTargets =<< written) === Just [("unit1", ["Unit1Module1"])]

-- | Run the initial tasks in one worker, then the rest in a second worker that restores the first build's units.
resume :: (TestEnv -> TestEnv) -> Splice -> [BuildTask] -> String -> TestEnv -> TestT IO BuildResult
resume adjust splice initialTasks label testEnv = do
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
  dumpEdgesOfM label resumeEnv
  liftIO (hPutStrLn stderr ("[" ++ label ++ "] failure codes: " ++ show (failureCodes result)))
  pure result

-- | The tasks of the initial build when unit 2 is to be restored: everything but compiling M.
unit2Restored :: [BuildTask]
unit2Restored = [metaTask unit1 unit1.depUnits, moduleTask unit1 moduleL, metaTask unit2 unit2.depUnits]

-- | The tasks of the initial build when only unit 1 is to be restored.
unit1Restored :: [BuildTask]
unit1Restored = [metaTask unit1 unit1.depUnits, moduleTask unit1 moduleL]

levelError :: BuildResult -> Bool
levelError result = 28914 `elem` failureCodes result

noIncrModGraph :: TestEnv -> TestEnv
noIncrModGraph testEnv =
  testEnv {baseArgs = testEnv.baseArgs {features = testEnv.baseArgs.features {useIncrModGraph = False}}}

test_levelResume :: TestTree
test_levelResume =
  testGroup "explicit level imports in restored units" [
    withTestEnv \ getEnv -> unitTest "A: fresh, M compiles, and its build plan records the splice import" do
      fresh HomeInstance =<< liftIO getEnv,
    withTestEnv \ getEnv -> unitTest "B: M's unit restored, level error" do
      result <- resume id HomeInstance unit2Restored "B unit 2 restored" =<< liftIO getEnv
      assert (levelError result),
    withTestEnv \ getEnv -> unitTest "B': only L's unit restored, M compiles" do
      result <- resume id HomeInstance unit1Restored "B' unit 1 restored" =<< liftIO getEnv
      assertNoFailures "B'" result,
    withTestEnv \ getEnv -> unitTest "B0: as B with an instance from base (reports)" do
      _ <- resume id BaseInstance unit2Restored "B0 base instance" =<< liftIO getEnv
      pure (),
    withTestEnv \ getEnv -> unitTest "C: as B with a name only (reports)" do
      _ <- resume id NameOnly unit2Restored "C name only" =<< liftIO getEnv
      pure (),
    withTestEnv \ getEnv -> unitTest "D: as B with the incremental module graph off, level error" do
      result <- resume noIncrModGraph HomeInstance unit2Restored "D no incr" =<< liftIO getEnv
      assert (levelError result)
  ]

#else

import Test.Tasty (TestTree, testGroup)

-- | GHC 9.10 has no explicit level imports.
test_levelResume :: TestTree
test_levelResume = testGroup "explicit level imports in restored units" []

#endif
