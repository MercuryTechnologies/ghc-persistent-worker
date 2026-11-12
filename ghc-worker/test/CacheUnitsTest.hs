-- | The build plan's cache records each module's dependencies on units outside the build, and reads entries written
-- before it did.
module CacheUnitsTest (test_cacheUnits) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import GHC (mkModuleName)
import GHC.Unit.Types (stringToUnitId, unitIdString)
import Hedgehog ((===))
import Internal.BuildPlan.Json (fieldCache)
import System.OsPath.Extra (toOsPath)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)
import Types.BuildPlan (BuildPlanModule (..), Preprocessor (..))
import Types.CachedDeps (CachedModule (..), JsonFs (..))

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
          cache = fieldCache external (Map.fromList [("A", planModule "A.hs"), ("B", planModule "B.hs")])
          units key = map (unitIdString . (.raw)) . (.units) <$> Map.lookup key cache
      units "A" === Just ["p", "q"]
      units "B" === Just [],
    unitTest "reads an entry without units" do
      let decoded = Aeson.eitherDecode "{\"source\": \"A.hs\", \"modules\": [], \"packages\": []}"
      fmap (map (unitIdString . (.raw)) . (.units)) (decoded :: Either String CachedModule) === Right []
  ]
