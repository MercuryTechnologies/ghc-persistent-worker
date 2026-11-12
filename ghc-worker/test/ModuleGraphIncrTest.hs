{-# LANGUAGE CPP #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The incremental module graph answers every query as GHC's own construction does.
--
-- Random graphs are added batch by batch, one batch per unit, each batch depending only on itself and on earlier
-- batches, as Buck2 schedules units. After each batch, the incrementally extended graph is compared with
-- 'mkModuleGraph' over the same nodes, query by query. The answers are compared, not the vertex numbering, which
-- differs.
module ModuleGraphIncrTest (test_moduleGraphIncr) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Control.Monad (foldM_, forM)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName, moduleNameString)
import GHC.Unit.Module.Graph (
  ImportLevel (..),
  ModNodeKeyWithUid (..),
  ModuleGraph,
  ModuleGraphNode (..),
  ModuleNodeEdge (..),
  ModuleNodeInfo (..),
  NodeKey (..),
  ZeroScopeKey (..),
  mgQuery,
  mgQueryZero,
  mgReachableLoop,
  mkModuleGraph,
  mkNodeKey,
  mnKey,
  )
import GHC.Unit.Module.Location (pattern ModLocation)
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), UnitId, stringToUnitId, unitIdString)
import Hedgehog (Gen, Property, PropertyT, annotate, failure, forAllWith, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Internal.State.Make (extendModuleGraphIncr, mergeModuleGraphNodes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Types.State.Make (EModuleGraph (..), emptyEModuleGraph)

-- | The external units of all batches. Each depends on the one before it.
externalUnits :: [Int]
externalUnits = [0 .. 3]

externalUnitId :: Int -> UnitId
externalUnitId e = stringToUnitId ("ext" <> show e)

-- | The external unit and the ones it depends on, transitively, as @UnitNode@s.
externalUnitClosure :: Int -> [ModuleGraphNode]
externalUnitClosure e =
  [UnitNode [externalUnitId (d - 1) | d > 0] (externalUnitId d) | d <- [0 .. e]]

unitId :: Int -> UnitId
unitId b = stringToUnitId ("unit" <> show b)

moduleKey :: Int -> Int -> IsBootInterface -> ModNodeKeyWithUid
moduleKey b i boot = ModNodeKeyWithUid (GWIB (mkModuleName ("M" <> show b <> "_" <> show i)) boot) (unitId b)

moduleNode :: ModNodeKeyWithUid -> [ModuleNodeEdge] -> ModuleGraphNode
moduleNode key edges =
  ModuleNode edges (ModuleNodeFixed key (ModLocation Nothing "M.hi" "M.dyn_hi" "M.o" "M.dyn_o" "M.hie"))

genLevel :: Gen ImportLevel
genLevel = Gen.element [NormalLevel, SpliceLevel, QuoteLevel]

-- | Edges to some of the given modules and some external units, each at a random level.
genEdges :: [ModNodeKeyWithUid] -> Gen [ModuleNodeEdge]
genEdges candidates = do
  modules <- Gen.subsequence candidates
  externals <- Gen.subsequence externalUnits
  moduleEdges <- forM modules \ k -> (\ l -> ModuleNodeEdge l (NodeKey_Module k)) <$> genLevel
  externalEdges <- forM externals \ e -> (\ l -> ModuleNodeEdge l (NodeKey_ExternalUnit (externalUnitId e))) <$> genLevel
  pure (moduleEdges <> externalEdges)

-- | The nodes of one unit, given the modules of the units before it.
--
-- With @withBoot@, the unit may have an hs-boot loop: module @a@ imports the boot file of a later module @j@, and @j@
-- imports @a@, and maybe its own boot file.
genBatch :: Bool -> [ModNodeKeyWithUid] -> Int -> Gen [ModuleGraphNode]
genBatch withBoot earlier b = do
  n <- Gen.int (Range.linear 1 6)
  let keys = [moduleKey b i NotBoot | i <- [0 .. n - 1]]
  loop <-
    if withBoot && n >= 2
    then Gen.maybe do
      j <- Gen.int (Range.linear 1 (n - 1))
      a <- Gen.int (Range.linear 0 (j - 1))
      ownBoot <- Gen.bool
      pure (a, j, ownBoot)
    else pure Nothing
  bootNodes <- case loop of
    Just (a, j, _) -> do
      edges <- genEdges (earlier <> take a keys)
      pure [moduleNode (moduleKey b j IsBoot) edges]
    Nothing -> pure []
  moduleNodes <- forM (zip [0 ..] keys) \ (i, key) -> do
    edges <- genEdges (earlier <> take i keys)
    let loopEdges = case loop of
          Just (a, j, ownBoot)
            | i == a -> [ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey b j IsBoot))]
            | i == j ->
              ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey b a NotBoot))
              : [ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey b j IsBoot)) | ownBoot]
          _ -> []
    pure (moduleNode key (edges <> loopEdges))
  pure (bootNodes <> moduleNodes <> externalUnitClosure (last externalUnits))

genBatches :: Bool -> Gen [[ModuleGraphNode]]
genBatches withBoot = do
  units <- Gen.int (Range.linear 1 5)
  go units 0 []
  where
    go units b earlier
      | b >= units = pure []
      | otherwise = do
        batch <- genBatch withBoot earlier b
        let modules = [k | ModuleNode _ (ModuleNodeFixed k@(ModNodeKeyWithUid (GWIB _ NotBoot) _) _) <- batch]
        (batch :) <$> go units (b + 1) (earlier <> modules)

renderKey :: NodeKey -> String
renderKey = \case
  NodeKey_Module (ModNodeKeyWithUid (GWIB m boot) _) ->
    moduleNameString m <> (case boot of IsBoot -> "-boot"; NotBoot -> "")
  NodeKey_ExternalUnit u -> unitIdString u
  _ -> "?"

renderLevel :: ImportLevel -> String
renderLevel = \case
  NormalLevel -> ""
  SpliceLevel -> "splice "
  QuoteLevel -> "quote "

renderNode :: ModuleGraphNode -> String
renderNode = \case
  ModuleNode edges info ->
    renderKey (NodeKey_Module (mnKey info)) <> " -> " <>
    intercalate ", " [renderLevel l <> renderKey k | ModuleNodeEdge l k <- edges]
  UnitNode deps u -> unitIdString u <> " -> " <> intercalate ", " (map unitIdString deps)
  _ -> "?"

renderBatches :: [[ModuleGraphNode]] -> String
renderBatches batches =
  unlines [unlines (("unit " <> show b) : map (("  " <>) . renderNode) batch) | (b, batch) <- zip [0 :: Int ..] batches]

renderZero :: ZeroScopeKey -> String
renderZero = \case
  ModuleScope k l -> renderLevel l <> renderKey (NodeKey_Module k)
  UnitScope u -> unitIdString u

-- | Fail with the queries whose answers differ, if any.
expectSame :: String -> [String] -> PropertyT IO ()
expectSame what = \case
  [] -> pure ()
  diffs -> do
    annotate (what <> " differs:\n" <> unlines (take 20 diffs))
    failure

-- | Compare the incremental graph with GHC's construction over the same nodes: @mgQuery@ over @mg_graph@,
-- @mgReachableLoop@ over @mg_loop_graph@, and @mgQueryZero@ over @mg_zero_graph@.
compareGraphs :: [ModuleGraphNode] -> ModuleGraph -> ModuleGraph -> PropertyT IO ()
compareGraphs nodes actual expected = do
  let keys = map mkNodeKey nodes
      zeroKeys =
        [ModuleScope (mnKey info) l | ModuleNode _ info <- nodes, l <- [SpliceLevel, QuoteLevel, NormalLevel]]
        <> [UnitScope u | UnitNode _ u <- nodes]
      loopSet g k = Set.fromList (map mkNodeKey (mgReachableLoop g [k]))
  expectSame "mgQuery" [
    renderKey a <> " -> " <> renderKey b <> ": " <> show (mgQuery actual a b) <> ", GHC: " <> show (mgQuery expected a b)
    | a <- keys, b <- keys, mgQuery actual a b /= mgQuery expected a b
    ]
  expectSame "mgReachableLoop" [
    renderKey k <> ": " <> show (map renderKey (Set.toList (loopSet actual k))) <>
    ", GHC: " <> show (map renderKey (Set.toList (loopSet expected k)))
    | k <- keys, loopSet actual k /= loopSet expected k
    ]
  expectSame "mgQueryZero" [
    renderZero a <> " -> " <> renderZero b <> ": " <> show (mgQueryZero actual a b) <>
    ", GHC: " <> show (mgQueryZero expected a b)
    | a <- zeroKeys, b <- zeroKeys, mgQueryZero actual a b /= mgQueryZero expected a b
    ]

-- | Add the batches one by one, and compare the graph with GHC's after each.
equivalence :: Bool -> Property
equivalence withBoot =
  withTests 200 $ property do
    batches <- forAllWith renderBatches (genBatches withBoot)
    let step (nodes0, egr0) batch = do
          let nodes1 = mergeModuleGraphNodes batch nodes0
              egr1 = extendModuleGraphIncr nodes1 egr0
          compareGraphs (Map.elems nodes1) egr1.moduleGraph (mkModuleGraph (Map.elems nodes1))
          pure (nodes1, egr1)
    foldM_ step (Map.empty, emptyEModuleGraph) batches

test_moduleGraphIncr :: TestTree
test_moduleGraphIncr =
  testGroup "incremental module graph" [
    testProperty "equals GHC's, without hs-boot loops" (equivalence False),
    testProperty "equals GHC's, with hs-boot loops" (equivalence True)
  ]

#else

import Test.Tasty (TestTree, testGroup)

-- | GHC 9.10's module graph has one index, and no import levels.
test_moduleGraphIncr :: TestTree
test_moduleGraphIncr = testGroup "incremental module graph" []

#endif
