{-# LANGUAGE PatternSynonyms #-}

--
-- Create build plan file for an example with multiple units which
-- has both intra-unit and inter-unit module dependencies
-- Check make mode and one-shot mode.
--
module BuildPlanTest.Test1 where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (DynFlags (..), Ghc, GhcMode (..), ModuleName, Target (..), getSession, mkModuleName)
import GHC.Driver.Env (HscEnv (..))
import GHC.Unit (UnitId, stringToUnitId)
import Hedgehog (TestT, evalMaybe, (===))
import Internal.BuildPlan (buildPlanForTargets)
import Internal.DynFlags (modifyActiveUnitFlags)
import Internal.Metadata (prepareMetadataSession)
import Internal.Session (sessionWithDebugLog, withDynFlags)
import Internal.State (newState, updateMakeStateVar)
import Internal.State.Make (insertUnitEnv, storeModuleGraph)
import Prelude hiding (log)
import System.FilePath ((</>))
import System.OsPath.Extra (takeFileName, toOsPath)
import Test.PackageDb (ModuleSpec (..), UnitSpec (..), createEmptyHomeUnitDb, moduleSpec)
import Test.Run (persistentSession, transientSession, unitTest, withTemp)
import Test.Target (fileUnitTargets, ghcOptions, pureUnitTargets)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..), BuildPlanField (..), buildPlanAll, emptyArgs)
import Types.BuildPlan (BuildPlan (..), BuildPlanJson (..), BuildPlanSchema (..))
import Types.CachedDeps (CachedModule (..), CachedPackageDep (..), JsonFs (..))

jmn :: String -> JsonFs ModuleName
jmn = JsonFs . mkModuleName

jui :: String -> JsonFs UnitId
jui = JsonFs . stringToUnitId

unit1 :: UnitId
unit1 = stringToUnitId "unit1"

unit2 :: UnitId
unit2 = stringToUnitId "unit2"

-- The order here is important due to a bug in GHC that's fixed in 9.14, though yet unclear which commit is responsible.
-- TODO: we should have a separate test for the case with boot modules.
-- NOTE: If U1M1 and U1M2 have cyclic deps and the boot module is specified first,
--       @U1M2@ will not get the dependency on @U1M1@.
unit1Modules :: NonEmpty ModuleSpec
unit1Modules =
  [
    moduleSpec "U1M1" [
      "module U1M1 where",
      "u1m1 :: Int",
      "u1m1 = 5"
    ],
    moduleSpec "U1M2" [
      "module U1M2 where",
      "import U1M1",
      "u1m2 :: Int",
      "u1m2 = u1m1 + 5"
    ]
  ]

unit2Modules :: NonEmpty ModuleSpec
unit2Modules =
  [
    moduleSpec "U2M1" ["module U2M1 where", "u2m1 :: Int", "u2m1 = 12"],
    moduleSpec "U2M2" [
      "module U2M2 where",
      "import U2M1",
      "import U1M2",
      "u2m2 :: Int",
      "u2m2 = u1m2 + u2m1 + 5"
    ]
  ]

unit1Spec :: UnitSpec
unit1Spec =
  UnitSpec {name = "unit1", deps = [], modules = unit1Modules}

unit2Spec :: UnitSpec
unit2Spec =
  UnitSpec {name = "unit2", deps = ["unit1"], modules = unit2Modules}

fields :: Set BuildPlanField
fields = Set.fromList (toList buildPlanAll)

writeDummy :: FilePath -> TestT IO FilePath
writeDummy tmp = do
  liftIO $ writeFile dummyFile ""
  pure dummyFile
  where
    dummyFile = tmp </> "Dummy.hs"

expected1 :: BuildPlanSchema
expected1 =
  BuildPlanSchema {
    exposed_modules = Just ["U1M1", "U1M2"],
    module_graph = Just [
      ("U1M1", []),
      ("U1M2", ["U1M1"])
    ],
    package_deps = Just [
      ("U1M1", [("base", [jmn "Prelude"])]),
      ("U1M2", [("base", [jmn "Prelude"])])
    ],
    project_deps = Just [
      ("U1M1", []),
      ("U1M2", [])
    ],
    toolchain_deps = Just [
      ("U1M1", [("base", [jmn "Prelude"])]),
      ("U1M2", [("base", [jmn "Prelude"])])
    ],
    th_modules = Just [],
    cache = Just [
      ("U1M1", CachedModule {
        source = toOsPath "U1M1.hs",
        modules = [],
        packages = [],
        flags = []
      }),
      ("U1M2", CachedModule {
        source = toOsPath "U1M2.hs",
        modules = [jmn "U1M1"],
        packages = [],
        flags = []
      })
    ]
  }

expected2 :: Bool -> BuildPlanSchema
expected2 oneshot =
  BuildPlanSchema {
    exposed_modules = Just ["U2M1", "U2M2"],
    module_graph = Just [
      ("U2M1", []),
      ("U2M2", ["U2M1"])
    ],
    package_deps = Just [
      ("U2M1", [("base", [jmn "Prelude"])]),
      ("U2M2", [
        ("base", [jmn "Prelude"]),
        ("unit1", [jmn "U1M2"])
      ])
    ],
    project_deps = Just [
      ("U2M1", []),
      ("U2M2", if oneshot then [] else [("unit1", [jmn "U1M2"])])
    ],
    toolchain_deps = Just [
      ("U2M1", [("base", [jmn "Prelude"])]),
      ("U2M2", [("base", [jmn "Prelude"])] <> if oneshot then [("unit1", [jmn "U1M2"])] else [])
    ],
    th_modules = Just [],
    cache = Just [
      ("U2M1", CachedModule {
        source = toOsPath "Dummy.hs",
        modules = [],
        packages = [],
        flags = []
      }),
      ("U2M2", CachedModule {
        source = toOsPath "Dummy.hs",
        modules = [jmn "U2M1"],
        packages = if oneshot then [] else [CachedPackageDep {id = jui "unit1", modules = [jmn "U1M2"]}],
        flags = []
      })
    ]
  }

runBuildPlan :: NonEmpty Target -> Ghc (BuildPlan, HscEnv)
runBuildPlan targets = do
  modifyActiveUnitFlags \ d -> d {ghcMode = MkDepend}
  plan <- buildPlanForTargets fields mempty (toList targets)
  hsc_env <- getSession
  pure (plan, hsc_env)

normalize :: BuildPlanJson -> BuildPlanSchema
normalize BuildPlanJson {schema} =
  schema {cache = fmap normalizeModule <$> schema.cache}
  where
    normalizeModule CachedModule {..} = CachedModule {source = takeFileName source, ..}

-- | Simulate build plan JSON generation for two units with dependencies to ensure that toolchain and project deps are
-- present in the result.
--
-- The first unit requires file backed targets because module lookup of external dependencies requires either source
-- files or interface files.
--
-- In between units, we store the module graph and unit env in the state to satisfy the @downsweep@ requirements.
test_buildPlan_make :: TestTree
test_buildPlan_make =
  withTemp "build-plan-make" \ tmpResource ->
    unitTest "build plan JSON with persistent state" do
      tmp <- liftIO tmpResource
      dummyFile <- writeDummy tmp
      state <- liftIO $ newState
      testUnit1 tmp state
      testUnit2 dummyFile state
  where
    testUnit1 tmp state = do
      targets <- liftIO $ fileUnitTargets (tmp </> "src") unit1Spec
      (plan1, hsc_env1) <- persistentSession state (ghcOptions unit1 []) (runBuildPlan targets)
      expected1 === normalize plan1.json
      persist state plan1 hsc_env1

    testUnit2 dummyFile state = do
      (plan2, _) <- evalMaybe =<< liftIO do
        sessionWithDebugLog state (emptyArgs []) {ghcOptions = ghcOptions unit2 [(unit1, Nothing)]} \ env ->
          withDynFlags env \ dflags _ -> do
            _ <- prepareMetadataSession env dflags
            runBuildPlan (pureUnitTargets dummyFile unit2Spec)
      expected2 False === normalize plan2.json

    persist state plan hsc_env =
      liftIO do
        updateMakeStateVar state (storeModuleGraph plan.graph)
        updateMakeStateVar state (insertUnitEnv hsc_env)

-- | Like 'test_buildPlan_make', but simulates oneshot mode orchestration without persistent worker state.
-- Modules are exposed in a package DB file that we generate between units.
test_buildPlan_oneshot :: TestTree
test_buildPlan_oneshot =
  withTemp "build-plan-oneshot" \ tmpResource ->
    unitTest "build plan JSON with package DBs" do
      tmp <- liftIO tmpResource
      dummyFile <- writeDummy tmp
      testUnit1 tmp
      unit1Db <- setupDb tmp
      testUnit2 dummyFile unit1Db
  where
    testUnit1 tmp = do
      targets <- liftIO $ fileUnitTargets (tmp </> "src") unit1Spec
      (plan1, _) <- transientSession (ghcOptions unit1 []) (runBuildPlan targets)
      expected1 === normalize plan1.json

    testUnit2 dummyFile unit1Db = do
      (plan2, _) <-
        transientSession (ghcOptions unit2 [(unit1, Just unit1Db)]) (runBuildPlan (pureUnitTargets dummyFile unit2Spec))
      expected2 True === normalize plan2.json

    setupDb tmp =
      liftIO $ createEmptyHomeUnitDb unit1Spec (tmp </> "unit1") ["U1M1", "U1M2"]

test_buildPlan :: TestTree
test_buildPlan =
  testGroup "build plan JSON" [
    test_buildPlan_make,
    test_buildPlan_oneshot
  ]
