-- | Unit tests for 'GhcServer.Build.Schedule' pure functions:
-- 'resolveFromCachedUnit' and 'nodeDepsToTaskKeys'.
module Test.ScheduleTest where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (ModuleName, mkModuleName, moduleNameString)
import GHC.Unit.Module.Graph (ModNodeKeyWithUid (..), ModuleGraphNode (..), NodeKey (..))
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), UnitId, stringToUnitId)
import GhcServer.Build.Schedule (
  BuildStatus (..),
  ModuleInfo (..),
  ModuleKey (..),
  Resolutions,
  TaskKey (..),
  nodeDepsToTaskKeys,
  resolutionsFromModuleMap,
  resolveFromCachedUnit,
  )
import GhcServer.Data.Unit (UnitName (..))
import GhcServer.Path (osPath)
import GhcServer.Scheduler (Phase (..))
import Hedgehog (TestT, property, test, withTests, (===))
import Test.Tasty (DependencyType (..), TestName, TestTree, dependentTestGroup, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Types.CachedDeps (CachedModule (..), CachedPackageDep (..), CachedUnit (..), JsonFs (..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

unitTest :: TestName -> TestT IO () -> TestTree
unitTest desc t =
  testProperty desc (withTests 1 (property (test t)))

jfs :: String -> JsonFs ModuleName
jfs = JsonFs . mkModuleName

jfsUid :: String -> JsonFs UnitId
jfsUid = JsonFs . stringToUnitId


mkCachedModule :: String -> [String] -> [CachedPackageDep] -> CachedModule
mkCachedModule source mods pkgs =
  CachedModule {
    source,
    modules = map jfs mods,
    packages = pkgs
  }

mkCachedUnit :: [(String, CachedModule)] -> CachedUnit
mkCachedUnit entries =
  CachedUnit {
    cache = Just (Map.fromList [(jfs name, cm) | (name, cm) <- entries]),
    build_plan = Nothing,
    unit_args = Nothing,
    unit_buck_args = Nothing,
    dep_units = Nothing
  }

mkPkgDep :: String -> [String] -> CachedPackageDep
mkPkgDep uid mods =
  CachedPackageDep {
    id = jfsUid uid,
    modules = map jfs mods
  }

mkNodeKey :: String -> String -> NodeKey
mkNodeKey uid modName =
  NodeKey_Module ModNodeKeyWithUid {
    mnkModuleName = GWIB {gwib_mod = mkModuleName modName, gwib_isBoot = NotBoot},
    mnkUnitId = stringToUnitId uid
  }

-- | Helper to build a resolved compile key.
rk :: String -> String -> TaskKey 'Resolved
rk unit modName = ResolvedModule (UnitName unit) (mkModuleName modName)

-- | Helper to build a pending compile key.
pk :: String -> String -> TaskKey 'Pending
pk unit src = PendingSource (UnitName unit) (osPath src)

-- ---------------------------------------------------------------------------
-- nodeDepsToTaskKeys spec
-- ---------------------------------------------------------------------------

-- | A home module known to the test: @(unitId, moduleName, sourcePath)@.
data HomeModule =
  HomeModule {
    uid :: String,
    modName :: String,
    src :: String
  }

-- | Specification for a @nodeDepsToTaskKeys@ test.
data NodeDepsSpec =
  NodeDepsSpec {
    -- | All unit ids that should appear in the name map.
    unitIds :: [(String, String)]
    ,
    -- | Home modules (populates srcMap).
    homeModules :: [HomeModule]
    ,
    -- | Node keys the module depends on (as @(unitId, modName)@ pairs).
    deps :: [(String, String)]
    ,
    expected :: Set (TaskKey 'Pending)
  }

runNodeDepsSpec :: NodeDepsSpec -> TestT IO ()
runNodeDepsSpec spec = do
  let
    nameMap = Map.fromList [(stringToUnitId uid, UnitName n) | (uid, n) <- spec.unitIds]
    srcMap = Map.fromList
      [((stringToUnitId hm.uid, mkModuleName hm.modName), osPath hm.src) | hm <- spec.homeModules]
    depKeys = [mkNodeKey uid m | (uid, m) <- spec.deps]
    node = ModuleNode depKeys undefined
  spec.expected === nodeDepsToTaskKeys nameMap srcMap node

-- ---------------------------------------------------------------------------
-- Tests for 'nodeDepsToTaskKeys'
-- ---------------------------------------------------------------------------

test_nodeDepsNoDeps :: TestTree
test_nodeDepsNoDeps =
  unitTest "module with no deps" do
    runNodeDepsSpec NodeDepsSpec {
      unitIds = [("u0", "u0")],
      homeModules = [],
      deps = [],
      expected = Set.empty
    }

test_nodeDepsHomeDep :: TestTree
test_nodeDepsHomeDep =
  unitTest "module depending on home module" do
    runNodeDepsSpec NodeDepsSpec {
      unitIds = [("u0", "u0"), ("u1", "u1")],
      homeModules = [HomeModule "u0" "A" "u0/A.hs"],
      deps = [("u0", "A")],
      expected = Set.singleton (pk "u0" "u0/A.hs")
    }

test_nodeDepsExternalDepsExcluded :: TestTree
test_nodeDepsExternalDepsExcluded =
  unitTest "external package deps are excluded" do
    runNodeDepsSpec NodeDepsSpec {
      unitIds = [("u0", "u0")],
      homeModules = [],
      deps = [("base-4.20", "Prelude")],
      expected = Set.empty
    }

test_nodeDepsMixed :: TestTree
test_nodeDepsMixed =
  unitTest "mixed home and external deps" do
    runNodeDepsSpec NodeDepsSpec {
      unitIds = [("u0", "u0"), ("u1", "u1")],
      homeModules = [HomeModule "u0" "A" "u0/A.hs"],
      deps = [("u0", "A"), ("base-4.20", "Prelude")],
      expected = Set.singleton (pk "u0" "u0/A.hs")
    }

-- | Home dep not in home set — should be excluded.
test_nodeDepsNotHome :: TestTree
test_nodeDepsNotHome =
  unitTest "dep not in source map is excluded" do
    let
      nameMap = Map.singleton (stringToUnitId "u0") (UnitName "u0")
      srcMap = Map.empty
      node = ModuleNode [mkNodeKey "u0" "A"] undefined
    Set.empty === nodeDepsToTaskKeys nameMap srcMap node

test_nodeDepsMultiUnit :: TestTree
test_nodeDepsMultiUnit =
  unitTest "deps from multiple home units" do
    runNodeDepsSpec NodeDepsSpec {
      unitIds = [("u0", "u0"), ("u1", "u1")],
      homeModules =
        [ HomeModule "u0" "A" "u0/A.hs"
        , HomeModule "u1" "B" "u1/B.hs"
        ],
      deps = [("u0", "A"), ("u1", "B")],
      expected = Set.fromList
        [ pk "u0" "u0/A.hs"
        , pk "u1" "u1/B.hs"
        ]
    }

-- ---------------------------------------------------------------------------
-- resolveFromCachedUnit spec
-- ---------------------------------------------------------------------------

-- | Specification for a @resolveFromCachedUnit@ test.
data ResolveSpec =
  ResolveSpec {
    unitName :: String
    ,
    cachedUnit :: CachedUnit
    ,
    priorModules :: Map ModuleKey ModuleInfo
  }

runResolve :: ResolveSpec -> Resolutions
runResolve spec =
  resolutionsFromModuleMap spec.priorModules newModules
  where
    newModules = resolveFromCachedUnit (UnitName spec.unitName) (osPath "output") spec.cachedUnit

defaultResolveSpec :: ResolveSpec
defaultResolveSpec =
  ResolveSpec {
    unitName = "u0",
    cachedUnit = mkCachedUnit [],
    priorModules = Map.empty
  }

-- | Helper to build a prior module entry for cross-unit dep tests.
mkPriorModule :: String -> String -> String -> (ModuleKey, ModuleInfo)
mkPriorModule unitName modName src =
  ( ModuleKey {unit = UnitName unitName, name = mkModuleName modName}
  , ModuleInfo {task = pk unitName src, deps = Set.empty, hiPath = ""}
  )

-- | Look up a resolved task in the resolutions map by pending key.
lookupResolution :: String -> String -> Resolutions -> Maybe (TaskKey 'Resolved, BuildStatus, Set (TaskKey 'Pending))
lookupResolution unit src =
  Map.lookup (PendingSource (UnitName unit) (osPath src))

-- ---------------------------------------------------------------------------
-- Tests for 'resolveFromCachedUnit'
-- ---------------------------------------------------------------------------

test_resolveCachedNoDeps :: TestTree
test_resolveCachedNoDeps =
  unitTest "single cached module with no deps" do
    let
      spec = defaultResolveSpec {
        cachedUnit = mkCachedUnit [("A", mkCachedModule "u0/A.hs" [] [])]
      }
      result = runResolve spec
    Map.size result === 1
    case lookupResolution "u0" "u0/A.hs" result of
      Just (ResolvedModule name modName, BuildStatus {}, deps) -> do
        name === UnitName "u0"
        moduleNameString modName === "A"
        deps === Set.empty
      _ -> fail "expected resolution for A"

test_resolveCachedIntraDep :: TestTree
test_resolveCachedIntraDep =
  unitTest "intra-unit dep resolved from local index" do
    let
      spec = defaultResolveSpec {
        cachedUnit = mkCachedUnit
          [ ("A", mkCachedModule "u0/A.hs" [] [])
          , ("B", mkCachedModule "u0/B.hs" ["A"] [])
          ]
      }
      result = runResolve spec
    Map.size result === 2
    case lookupResolution "u0" "u0/B.hs" result of
      Just (_, _, deps) -> deps === Set.singleton (pk "u0" "u0/A.hs")
      _ -> fail "expected resolution for B"

test_resolveCachedCrossUnitDep :: TestTree
test_resolveCachedCrossUnitDep =
  unitTest "cross-unit dep resolved from module index" do
    let
      spec = defaultResolveSpec {
        unitName = "u1",
        cachedUnit = mkCachedUnit
          [("B", mkCachedModule "u1/B.hs" [] [mkPkgDep "u0" ["A"]])],
        priorModules = Map.fromList [mkPriorModule "u0" "A" "u0/A.hs"]
      }
      result = runResolve spec
    case lookupResolution "u1" "u1/B.hs" result of
      Just (_, _, deps) -> deps === Set.singleton (pk "u0" "u0/A.hs")
      _ -> fail "expected resolution for B"

test_resolveCachedExternalPkgIgnored :: TestTree
test_resolveCachedExternalPkgIgnored =
  unitTest "external package deps are ignored" do
    let
      spec = defaultResolveSpec {
        cachedUnit = mkCachedUnit
          [("A", mkCachedModule "u0/A.hs" [] [mkPkgDep "base" ["Prelude"]])]
      }
      result = runResolve spec
    case lookupResolution "u0" "u0/A.hs" result of
      Just (_, _, deps) -> deps === Set.empty
      _ -> fail "expected resolution for A"

test_resolveCachedMixedDeps :: TestTree
test_resolveCachedMixedDeps =
  unitTest "mixed intra-unit and cross-unit deps" do
    let
      spec = defaultResolveSpec {
        unitName = "u1",
        cachedUnit = mkCachedUnit
          [ ("A", mkCachedModule "u1/A.hs" [] [])
          , ("B", mkCachedModule "u1/B.hs" ["A"] [mkPkgDep "u0" ["X"]])
          ],
        priorModules = Map.fromList [mkPriorModule "u0" "X" "u0/X.hs"]
      }
      result = runResolve spec
    case lookupResolution "u1" "u1/B.hs" result of
      Just (_, _, deps) -> deps === Set.fromList [pk "u1" "u1/A.hs", pk "u0" "u0/X.hs"]
      _ -> fail "expected resolution for B"

test_resolveCachedFromBuildPlan :: TestTree
test_resolveCachedFromBuildPlan =
  unitTest "falls back to build_plan when cache is Nothing" do
    let
      spec = defaultResolveSpec {
        cachedUnit = CachedUnit {
          cache = Nothing,
          build_plan = Just (Map.singleton (jfs "A") (mkCachedModule "u0/A.hs" [] [])),
          unit_args = Nothing,
          unit_buck_args = Nothing,
          dep_units = Nothing
        }
      }
      result = runResolve spec
    Map.size result === 1

test_resolveCachedEmpty :: TestTree
test_resolveCachedEmpty =
  unitTest "empty cached unit produces empty resolutions" do
    let
      spec = defaultResolveSpec {
        cachedUnit = CachedUnit {
          cache = Nothing,
          build_plan = Nothing,
          unit_args = Nothing,
          unit_buck_args = Nothing,
          dep_units = Nothing
        }
      }
      result = runResolve spec
    Map.null result === True

test_resolveCachedCrossUnitMissing :: TestTree
test_resolveCachedCrossUnitMissing =
  unitTest "cross-unit dep not in module index is excluded" do
    let
      spec = defaultResolveSpec {
        unitName = "u1",
        cachedUnit = mkCachedUnit
          [("B", mkCachedModule "u1/B.hs" [] [mkPkgDep "u0" ["A"]])]
      }
      result = runResolve spec
    case lookupResolution "u1" "u1/B.hs" result of
      Just (_, _, deps) -> deps === Set.empty
      _ -> fail "expected resolution for B"

-- ---------------------------------------------------------------------------
-- Test tree
-- ---------------------------------------------------------------------------

test_schedule :: TestTree
test_schedule =
  dependentTestGroup "GhcServer.Build.Schedule" AllFinish
    [ dependentTestGroup "nodeDepsToTaskKeys" AllFinish
        [ test_nodeDepsNoDeps
        , test_nodeDepsHomeDep
        , test_nodeDepsExternalDepsExcluded
        , test_nodeDepsMixed
        , test_nodeDepsNotHome
        , test_nodeDepsMultiUnit
        ]
    , dependentTestGroup "resolveFromCachedUnit" AllFinish
        [ test_resolveCachedNoDeps
        , test_resolveCachedIntraDep
        , test_resolveCachedCrossUnitDep
        , test_resolveCachedExternalPkgIgnored
        , test_resolveCachedMixedDeps
        , test_resolveCachedFromBuildPlan
        , test_resolveCachedEmpty
        , test_resolveCachedCrossUnitMissing
        ]
    ]
