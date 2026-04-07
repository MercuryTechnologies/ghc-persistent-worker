-- | Tests for Cabal-based project discovery in the standalone GHC server.
module Test.CabalTest where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GhcServer.Build (BuildResult (..), newBuildState, runBuild)
import GhcServer.Cabal (discoverCabalProject)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (BuildEvent (..), newBuildEvents, readEvents)
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Log (newLogger, withBuildLog)
import GhcServer.Path (osPath)
import Hedgehog (TestT, annotate, assert, diff, property, test, withTests, (===))
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import Test.BuildTest (
  TestProject (..),
  acquireTemp,
  assertHasCompiled,
  assertHasMetadata,
  assertSuccess,
  eventCompiled,
  eventCompiledUnits,
  eventMetadata,
  testTaskTimeout,
  timedBuild,
  writeProjectFile,
  )
import Test.Tasty (DependencyType (..), TestName, TestTree, dependentTestGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (emptyArgs)

-- ---------------------------------------------------------------------------
-- Cabal project helpers
-- ---------------------------------------------------------------------------

-- | Acquire a 'TestProject' by discovering a Cabal project.
acquireCabalProject :: IO FilePath -> IO TestProject
acquireCabalProject acquireRoot = do
  root <- acquireRoot
  let
    rootOs = osPath root
    outputDir = osPath (root ++ "/output")
    tmpDir = osPath (root ++ "/tmp")
  project <- withBuildLog \ logger ->
    discoverCabalProject logger rootOs outputDir tmpDir
  pure TestProject {root, rootOs, project, outputDir, tmpDir}

-- | Test combinator for Cabal-based projects.
cabalProjectTest :: FilePath -> (FilePath -> IO ()) -> TestName -> (TestProject -> TestT IO ()) -> TestTree
cabalProjectTest dirName create name body =
  withResource (acquireTemp dirName) (\ _ -> pure ()) \ acquire ->
    testProperty name $ withTests 1 $ property $ test do
      tp <- liftIO do
        root <- acquire
        create root
        acquireCabalProject (pure root)
      body tp

-- | Run a fresh build with the given schedule steps.
runCabalFresh :: TestProject -> [(UnitName, UnitRequest)] -> IO ([BuildEvent], GhcServer.Build.BuildResult)
runCabalFresh tp steps = timedBuild do
  stateVar <- newBuildState
  log <- newLogger False
  events <- newBuildEvents
  let env = BuildEnv {
        baseArgs = emptyArgs Map.empty,
        projectRoot = tp.rootOs,
        outputDir = tp.outputDir,
        tmpDir = tp.tmpDir,
        stateVar,
        project = tp.project,
        log,
        events
      }
  result <- runBuild 4 testTaskTimeout env ScheduleRequest {steps, recompile = False, rebuild = False}
  evs <- readEvents events
  pure (evs, result)

-- ---------------------------------------------------------------------------
-- Small 2-library Cabal project
-- ---------------------------------------------------------------------------

-- | Create a project with a Cabal file containing two sub-libraries.
--
-- @
-- lib-a/
--   A.hs
-- lib-b/
--   B.hs     (imports A from lib-a)
-- test-project.cabal
-- @
createSmallCabalProject :: FilePath -> IO ()
createSmallCabalProject root = do
  createDirectoryIfMissing True (root ++ "/lib-a")
  createDirectoryIfMissing True (root ++ "/lib-b")

  writeProjectFile root "lib-a/A.hs" $ unlines
    [ "module A where"
    , ""
    , "hello :: String"
    , "hello = \"hello from lib-a\""
    ]

  writeProjectFile root "lib-b/B.hs" $ unlines
    [ "module B where"
    , ""
    , "import A (hello)"
    , ""
    , "greeting :: String"
    , "greeting = hello ++ \" world\""
    ]

  writeProjectFile root "test-project.cabal" $ unlines
    [ "cabal-version: 3.0"
    , "name: test-project"
    , "version: 0.1"
    , "build-type: Simple"
    , ""
    , "library lib-a"
    , "  hs-source-dirs: lib-a"
    , "  exposed-modules: A"
    , "  build-depends: base"
    , "  default-language: GHC2021"
    , ""
    , "library lib-b"
    , "  hs-source-dirs: lib-b"
    , "  exposed-modules: B"
    , "  build-depends: base, test-project:lib-a"
    , "  default-language: GHC2021"
    ]

-- ---------------------------------------------------------------------------
-- 4-library Cabal project (mirrors createLargeProject)
-- ---------------------------------------------------------------------------

createLargeCabalProject :: FilePath -> IO ()
createLargeCabalProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  createDirectoryIfMissing True (root ++ "/unit1")
  createDirectoryIfMissing True (root ++ "/unit2")
  createDirectoryIfMissing True (root ++ "/unit3")

  -- unit0: leaf, no deps
  writeModule root "unit0" "A0" [] "a0 = \"a0\""
  writeModule root "unit0" "B0" [] "b0 = \"b0\""
  writeModule root "unit0" "C0" [] "c0 = \"c0\""
  writeModule root "unit0" "D0" [] "d0 = \"d0\""

  -- unit1: depends on unit0
  writeModule root "unit1" "A1" ["A0"] "a1 = a0 ++ \"_a1\""
  writeModule root "unit1" "B1" [] "b1 = \"b1\""
  writeModule root "unit1" "C1" [] "c1 = \"c1\""
  writeModule root "unit1" "D1" [] "d1 = \"d1\""

  -- unit2: depends on unit0
  writeModule root "unit2" "A2" ["B0"] "a2 = b0 ++ \"_a2\""
  writeModule root "unit2" "B2" [] "b2 = \"b2\""
  writeModule root "unit2" "C2" [] "c2 = \"c2\""
  writeModule root "unit2" "D2" [] "d2 = \"d2\""

  -- unit3: depends on unit1 + unit2
  writeModule root "unit3" "A3" ["A1"] "a3 = a1 ++ \"_a3\""
  writeModule root "unit3" "B3" ["A2"] "b3 = a2 ++ \"_b3\""
  writeModule root "unit3" "C3" [] "c3 = \"c3\""
  writeModule root "unit3" "D3" [] "d3 = \"d3\""

  writeProjectFile root "test-project.cabal" $ unlines
    [ "cabal-version: 3.0"
    , "name: test-project"
    , "version: 0.1"
    , "build-type: Simple"
    , ""
    , "library unit0"
    , "  hs-source-dirs: unit0"
    , "  exposed-modules: A0, B0, C0, D0"
    , "  build-depends: base"
    , "  default-language: GHC2021"
    , ""
    , "library unit1"
    , "  hs-source-dirs: unit1"
    , "  exposed-modules: A1, B1, C1, D1"
    , "  build-depends: base, test-project:unit0"
    , "  default-language: GHC2021"
    , ""
    , "library unit2"
    , "  hs-source-dirs: unit2"
    , "  exposed-modules: A2, B2, C2, D2"
    , "  build-depends: base, test-project:unit0"
    , "  default-language: GHC2021"
    , ""
    , "library unit3"
    , "  hs-source-dirs: unit3"
    , "  exposed-modules: A3, B3, C3, D3"
    , "  build-depends: base, test-project:unit1, test-project:unit2"
    , "  default-language: GHC2021"
    ]
  where
    lcFirst (c : cs) = toLower c : cs
    lcFirst [] = []

    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

    writeModule base unitName modName imports body =
      writeProjectFile base (unitName ++ "/" ++ modName ++ ".hs") $ unlines $
        ["module " ++ modName ++ " where"]
        ++ ["import " ++ imp ++ " (" ++ lcFirst imp ++ ")" | imp <- imports]
        ++ ["", lcFirst modName ++ " :: String", body]

-- ---------------------------------------------------------------------------
-- Unit test: Cabal discovery
-- ---------------------------------------------------------------------------

smallCabalTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
smallCabalTest =
  cabalProjectTest "ghc-server-cabal-small" createSmallCabalProject

largeCabalTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
largeCabalTest =
  cabalProjectTest "ghc-server-cabal-large" createLargeCabalProject

-- | Test that Cabal discovery produces the correct project structure.
test_cabalDiscovery :: TestTree
test_cabalDiscovery =
  smallCabalTest "cabal discovery: units and deps" \ tp -> do
    let unitNames = Set.fromList (Map.keys tp.project.units)
    annotate ("Units: " ++ show unitNames)
    diff (UnitName "lib-a") Set.member unitNames
    diff (UnitName "lib-b") Set.member unitNames

    case Map.lookup (UnitName "lib-b") tp.project.units of
      Nothing -> annotate "lib-b not found" >> assert False
      Just u -> do
        annotate ("lib-b deps: " ++ show u.depUnits)
        diff (UnitName "lib-a") elem u.depUnits

-- | Test that a Cabal-based project can be fully built.
test_cabalBuildAll :: TestTree
test_cabalBuildAll =
  smallCabalTest "cabal build: full project" \ tp -> do
    (evs, result) <- liftIO (runCabalFresh tp [])
    assertSuccess "cabal build" result
    assertHasMetadata "lib-a" evs
    assertHasMetadata "lib-b" evs
    assertHasCompiled "lib-a" evs
    assertHasCompiled "lib-b" evs

-- | Test the large 4-library Cabal project builds.
test_cabalLargeBuild :: TestTree
test_cabalLargeBuild =
  largeCabalTest "cabal build: 4-library project" \ tp -> do
    (evs, result) <- liftIO (runCabalFresh tp [])
    assertSuccess "cabal large build" result
    ["unit0", "unit1", "unit2", "unit3"] === eventMetadata evs
    ["unit0", "unit1", "unit2", "unit3"] === eventCompiledUnits evs
    16 === length (eventCompiled evs)

-- | Test that Cabal discovery resolves deps correctly in the 4-library project.
test_cabalLargeDiscovery :: TestTree
test_cabalLargeDiscovery =
  largeCabalTest "cabal discovery: 4-library deps" \ tp -> do
    let units = tp.project.units
    case Map.lookup (UnitName "unit3") units of
      Nothing -> annotate "unit3 not found" >> assert False
      Just u -> do
        let depNames = Set.fromList u.depUnits
        annotate ("unit3 deps: " ++ show depNames)
        diff (UnitName "unit1") Set.member depNames
        diff (UnitName "unit2") Set.member depNames

test_cabalTests :: TestTree
test_cabalTests =
  dependentTestGroup "Cabal project support" AllFinish
    [ test_cabalDiscovery
    , test_cabalBuildAll
    , test_cabalLargeBuild
    , test_cabalLargeDiscovery
    ]
