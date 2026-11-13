-- | The build plan's cache records each module's dependencies on units outside
-- the build, and its imports at splice and quote level, keeps those at splice
-- or quote level only out of its plain lists, and reads entries written
-- before it did.
module CacheUnitsTest (test_cacheUnits) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import GHC (ModuleName (..), mkModuleName)
import GHC.Unit.Types (UnitId (..), stringToUnitId, unitIdString)
import Hedgehog ((===))
import Internal.BuildPlan.Json (ModuleLevelDeps (..), fieldCache)
import System.OsPath.Extra (toOsPath)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)
import Types.BuildPlan (BuildPlanModule (..), PackageDep (..), Preprocessor (..))
import Types.CachedDeps (
  CachedLevelDeps (..),
  CachedModule (..),
  CachedPackageDep (..),
  JsonFs (..),
  OtherLevel (..),
  )

planModule :: String -> BuildPlanModule
planModule source =
  BuildPlanModule {
    source = toOsPath source,
    sources = [],
    boot = False,
    modules = [],
    modulesBoot = [],
    packages = [],
    thEnabled = False,
    preprocessor = Preprocessor Nothing,
    flags = []
  }

test_cacheUnits :: TestTree
test_cacheUnits =
  testGroup "external units in the cache" [
    unitTest "records each module's external units" do
      let external =
            Map.fromList [
              ("A", Map.fromList [
                (stringToUnitId "q", ("q", [mkModuleName "Q"])),
                (stringToUnitId "p", ("p", [mkModuleName "P"]))
              ])
            ]
          cache = fieldCache external Map.empty planModules
          units key = map (unitIdString . (.raw)) . (.units) <$> Map.lookup key cache
      units "A" === Just ["p", "q"]
      units "B" === Just [],
    unitTest "reads an entry without units" do
      let decoded = Aeson.eitherDecode "{\"source\": \"A.hs\", \"modules\": [], \"packages\": []}"
      fmap (map (unitIdString . (.raw)) . (.units)) (decoded :: Either String CachedModule)
        === Right [],
    unitTest "records each module's imports at splice level" do
      let spliceDeps =
            CachedLevelDeps {
              modules = [JsonFs (mkModuleName "B")],
              packages = [],
              units = [JsonFs (stringToUnitId "p")]
            }
          levels =
            Map.fromList [
              (
                "A",
                ModuleLevelDeps {
                  levels = Map.fromList [(SpliceImport, spliceDeps)],
                  otherOnly = spliceDeps
                }
              )
            ]
          cache = fieldCache Map.empty levels planModules
      ((.splice) <$> Map.lookup "A" cache) === Just (Just spliceDeps)
      ((.quote) <$> Map.lookup "A" cache) === Just Nothing
      ((.splice) <$> Map.lookup "B" cache) === Just Nothing
      Aeson.eitherDecode (Aeson.encode (Map.lookup "A" cache)) === Right (Map.lookup "A" cache),
    -- A imports B and q's Q1 normally, B also at splice level, and C, q's Q2
    -- and the external unit p only at splice level. It imports the external
    -- unit r normally, and s, a static unit, only at splice level.
    unitTest "leaves out of the plain lists the imports at splice level only" do
      let planA =
            BuildPlanModule {
              source = toOsPath "A.hs",
              sources = [],
              boot = False,
              modules = [("B", name "B"), ("C", name "C")],
              modulesBoot = [],
              packages = [
                PackageDep {id = unit "q", name = "q", modules = [name "Q1", name "Q2"]},
                PackageDep {id = unit "s", name = "s", modules = [name "S"]}
              ],
              thEnabled = True,
              preprocessor = Preprocessor Nothing,
              flags = []
            }
          external =
            Map.fromList [
              ("A", Map.fromList [
                (stringToUnitId "p", ("p", [mkModuleName "P"])),
                (stringToUnitId "r", ("r", [mkModuleName "R"])),
                (stringToUnitId "s", ("s", [mkModuleName "S"]))
              ])
            ]
          spliceDeps =
            CachedLevelDeps {
              modules = [name "B", name "C"],
              packages = [CachedPackageDep {id = unit "q", modules = [name "Q2"]}],
              units = [unit "p", unit "s"]
            }
          otherOnly =
            CachedLevelDeps {
              modules = [name "C"],
              packages = [CachedPackageDep {id = unit "q", modules = [name "Q2"]}],
              units = [unit "p", unit "s"]
            }
          levels =
            Map.fromList [
              ("A", ModuleLevelDeps {levels = Map.fromList [(SpliceImport, spliceDeps)], otherOnly})
            ]
          entry = Map.lookup "A" (fieldCache external levels (Map.fromList [("A", planA)]))
      ((.modules) <$> entry) === Just [name "B"]
      ((.packages) <$> entry) === Just [CachedPackageDep {id = unit "q", modules = [name "Q1"]}]
      ((.units) <$> entry) === Just [unit "r"]
      ((.splice) <$> entry) === Just (Just spliceDeps),
    unitTest "reads an entry without levels" do
      let decoded = Aeson.eitherDecode "{\"source\": \"A.hs\", \"modules\": [], \"packages\": []}"
      fmap (\ m -> (m.splice, m.quote)) (decoded :: Either String CachedModule)
        === Right (Nothing, Nothing)
  ]
  where
    planModules = Map.fromList [("A", planModule "A.hs"), ("B", planModule "B.hs")]
    name = JsonFs . mkModuleName
    unit = JsonFs . stringToUnitId
