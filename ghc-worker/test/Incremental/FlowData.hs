{-# LANGUAGE PatternSynonyms #-}

module Incremental.FlowData where

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC (ModuleName (..), mkModuleName, moduleNameString, ms_mod_name)
import GHC.Unit (GenWithIsBoot (..), UnitId (..), unitIdString)
import GHC.Unit.Module.Graph (ModNodeKeyWithUid (..), ModuleGraph, ModuleGraphNode, NodeKey (..), mgModSummaries')
import GHC.Unit.Module.ModSummary (ms_unitid)
import Internal.Compat.FixedNodes (pattern CompileNode, pattern FixedNode, depsCompile, depsFixed, key, summary)
import Internal.Compat.GHC914 (edgeTarget)
import Test.PackageDb (UnitSpec (..), moduleSpec)
import Types.Args (BuildPlanField, buildPlanAll)
import Types.BuildPlan (ModuleKey)
import Types.CachedDeps (JsonFs (..))

utf8 :: String -> ByteString
utf8 = encodeUtf8 . Text.pack

jmn :: String -> JsonFs ModuleName
jmn = JsonFs . mkModuleName

modName :: Int -> Int -> String
modName unit index =
  mconcat [
    "U",
    show unit,
    "M",
    show index
  ]

data Dep =
  Dep {
    unit :: String,
    unitId :: JsonFs UnitId,
    name :: String,
    mname :: JsonFs ModuleName,
    mkey :: ModuleKey
  }
  deriving stock (Eq, Show)

data PDep =
  PDep {
    unit :: String,
    unitId :: JsonFs UnitId,
    mods :: [Dep]
  }
  deriving stock (Eq, Show)

data Mod =
  Mod {
    index :: Int,
    unit :: String,
    name :: String,
    mkey :: ModuleKey,
    home :: [Dep],
    package :: [PDep]
  }
  deriving stock (Eq, Show)

allDeps :: Mod -> [(String, [String])]
allDeps Mod {..} =
  (unit, (.name) <$> home) : [(dunit, (.name) <$> mods) | PDep {unit = dunit, mods} <- package]

data Unit =
  Unit {
    index :: Int,
    name :: String,
    id :: JsonFs UnitId,
    mods :: [Mod]
  }
  deriving stock (Eq, Show)

mkDep :: String -> JsonFs UnitId -> Int -> Int -> Dep
mkDep unit unitId uindex index =
  Dep {
    unit,
    unitId,
    name,
    mname = fromString name,
    mkey = fromString name
  }
  where
    name = modName uindex index

mkPDep :: (Int, [Int]) -> PDep
mkPDep (uindex, mods) =
  PDep {
    unit,
    unitId,
    mods = mkDep unit unitId uindex <$> mods
  }
  where
    unitId = fromString unit

    unit = "unit" ++ show uindex

mkMod :: Unit -> (Int, ([Int], [(Int, [Int])])) -> Mod
mkMod unit (index, (home, package)) =
  Mod {
    index,
    unit = unit.name,
    name,
    mkey = fromString name,
    home = mkDep unit.name unit.id unit.index <$> home,
    package = mkPDep <$> package
  }
  where
    name = modName unit.index index

mkUnit :: Int -> [(Int, ([Int], [(Int, [Int])]))] -> Unit
mkUnit index mods =
  unit
  where
    unit = Unit {
      index,
      name,
      id = unitId,
      mods = mkMod unit <$> mods
    }
    name = "unit" ++ show index
    unitId = fromString name

unit1 :: Unit
unit1 = mkUnit 1 [(m, (if m == 5 then [4] else [], [])) | m <- [0 .. 9]]

unit2 :: Unit
unit2 = mkUnit 2 $ zip [0 ..] [
    ([], [(1, [0])]),
    ([], [(1, [1])]),
    ([], [(1, [2])]),
    ([0], [(1, [3])]),
    ([3], [(1, [4])]),
    ([2, 3, 4], [(1, [5])]),
    ([5], [(1, [6])]),
    ([6], [(1, [7])]),
    ([5, 7], [(1, [8])])
  ]

u2m5_modified :: Mod
u2m5_modified =
  mkMod unit2 (5, ([0, 3, 4], [(1, [5, 8])]))

u2m9_added :: Mod
u2m9_added =
  mkMod unit2 (9, ([4, 5], [(1, [9])]))

unit2_modified :: Unit
unit2_modified =
  Unit {id = uid, mods = mapMaybe modifyMods mods ++ [u2m9_added], ..}
  where
    modifyMods = \case
      Mod {index  = i} | i == 1 || i == 2 -> Nothing
      Mod {index = 5} -> Just u2m5_modified
      spec -> Just spec

    Unit {id = uid, ..} = unit2

bpFields :: Set BuildPlanField
bpFields = Set.fromList (toList buildPlanAll)

modLines :: Mod -> [ByteString]
modLines spec =
  fmap utf8 $
  ("module " ++ spec.name ++ " where")
  :
  ["import " ++ m | (_, mods) <- allDeps spec, m <- mods]

unitSpec :: Int -> [Mod] -> UnitSpec
unitSpec u mods =
  UnitSpec {
    name = "unit" ++ show u,
    deps = [],
    modules = NonEmpty.fromList [moduleSpec spec.name (modLines spec) | spec <- mods]
  }

keyNames :: NodeKey -> Maybe (String, String)
keyNames = \case
  NodeKey_Module (ModNodeKeyWithUid (GWIB {gwib_mod}) uid) -> Just (unitIdString uid, moduleNameString gwib_mod)
  _ -> Nothing

nodeNames :: ModuleGraphNode -> Maybe (String, String, Bool, [(String, String)])
nodeNames = \case
  CompileNode {depsCompile, summary} ->
    Just (unitIdString (ms_unitid summary), moduleNameString (ms_mod_name summary), False, depNames depsCompile)
  FixedNode {depsFixed, key = ModNodeKeyWithUid (GWIB {gwib_mod}) unit} ->
    Just (unitIdString unit, moduleNameString gwib_mod, True, depNames depsFixed)
  _ ->
    Nothing
  where
    depNames = mapMaybe (keyNames . edgeTarget)

graphNames :: ModuleGraph -> [(String, String, Bool, [(String, String)])]
graphNames graph =
  sort (mapMaybe nodeNames (mgModSummaries' graph))
