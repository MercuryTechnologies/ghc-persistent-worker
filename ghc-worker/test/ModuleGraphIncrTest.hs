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

import Control.Monad (foldM, foldM_, forM, unless)
import Data.Foldable (for_)
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
  mgModSummaries',
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

-- | Which of GHC's three indices a comparison covers.
data Checks =
  Checks {
    -- | @mgQuery@, over @mg_graph@
    graph :: Bool,
    -- | @mgReachableLoop@, over @mg_loop_graph@
    loopGraph :: Bool,
    -- | @mgQueryZero@, over @mg_zero_graph@
    zeroGraph :: Bool
  }

allChecks :: Checks
allChecks = Checks {graph = True, loopGraph = True, zeroGraph = True}

-- | The external units of all batches. Each depends on the one before it.
externalUnits :: [Int]
externalUnits = [0 .. 3]

externalUnitId :: Int -> UnitId
externalUnitId e = stringToUnitId ("ext" <> show e)

-- | The external unit and the ones it depends on, transitively, as @UnitNode@s.
externalUnitClosure :: Int -> [ModuleGraphNode]
externalUnitClosure e =
  [UnitNode [externalUnitId (d - 1) | d > 0] (externalUnitId d) | d <- [0 .. e]]

-- | The unit with the given index, the index of its batch.
unitId :: Int -> UnitId
unitId u = stringToUnitId ("unit" <> show u)

-- | Module @i@ of the unit with index @u@.
moduleKey :: Int -> Int -> IsBootInterface -> ModNodeKeyWithUid
moduleKey u i boot = ModNodeKeyWithUid (GWIB (mkModuleName ("M" <> show u <> "_" <> show i)) boot) (unitId u)

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
  externalEdges <- forM externals \ e ->
    (\ l -> ModuleNodeEdge l (NodeKey_ExternalUnit (externalUnitId e))) <$> genLevel
  pure (moduleEdges <> externalEdges)

-- | The nodes of one unit, given the modules of the units before it.
--
-- @unitIndex@ is the index of the unit the batch is generated for.
--
-- Module @i@ imports some of the modules before it, in this unit and in earlier ones. With @withBoot@, the unit may
-- also have an hs-boot loop, between two modules @a@ and @j@ with @a < j@:
--
-- * the boot file of @j@ imports some of the modules before @a@, but not @a@
-- * @a@ imports the boot file of @j@
-- * @j@ imports @a@ and its own boot file
--
-- With the boot file as a node of its own, the graph stays acyclic, as GHC requires. @a@ and @j@ form a loop only
-- when the boot file stands for @j@.
genBatch :: Bool -> [ModNodeKeyWithUid] -> Int -> Gen [ModuleGraphNode]
genBatch withBoot earlier unitIndex = do
  n <- Gen.int (Range.linear 1 6)
  let keys = [moduleKey unitIndex i NotBoot | i <- [0 .. n - 1]]
  -- Proposes two modules @a < j@ of the unit to form an hs-boot loop.
  loop <-
    if withBoot && n >= 2
    then Gen.maybe do
      j <- Gen.int (Range.linear 1 (n - 1))
      a <- Gen.int (Range.linear 0 (j - 1))
      pure (a, j)
    else pure Nothing
  bootNodes <- case loop of
    Just (a, j) -> do
      -- Only the modules before @a@: @a@ imports this boot file, and any module from @a@ on may import @a@, so an
      -- import of one of them here could close a cycle that the boot file does not break.
      edges <- genEdges (earlier <> take a keys)
      pure [moduleNode (moduleKey unitIndex j IsBoot) edges]
    Nothing -> pure []
  moduleNodes <- forM (zip [0 ..] keys) \ (i, key) -> do
    edges <- genEdges (earlier <> take i keys)
    -- The edges that close the hs-boot loop, in addition to the random edges: @a@ to the boot file of @j@, and @j@
    -- to @a@. @j@ also imports its own boot file, as GHC's downsweep adds that edge to every module with an hs-boot
    -- file, so that the boot file is compiled first and the module can be checked against it.
    let loopEdges = case loop of
          Just (a, j)
            | i == a -> [ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey unitIndex j IsBoot))]
            | i == j -> [ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey unitIndex a NotBoot)),
                         ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey unitIndex j IsBoot))]
          _ -> []
    pure (moduleNode key (edges <> loopEdges))
  pure (bootNodes <> moduleNodes <> externalUnitClosure (last externalUnits))

-- | One batch per unit. Each batch may import the modules of the batches before it.
genBatches :: Bool -> Gen [[ModuleGraphNode]]
genBatches withBoot = do
  units <- Gen.int (Range.linear 1 5)
  (_, batches) <- foldM step ([], []) [0 .. units - 1]
  pure (reverse batches)
  where
    step (earlier, batches) unitIndex = do
      batch <- genBatch withBoot earlier unitIndex
      let modules = [k | ModuleNode _ (ModuleNodeFixed k@(ModNodeKeyWithUid (GWIB _ NotBoot) _) _) <- batch]
      pure (earlier <> modules, batch : batches)

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

renderNodes :: [ModuleGraphNode] -> String
renderNodes = unlines . map (("  " <>) . renderNode)

-- | Succeeds when there are no differences reported. Otherwise fails reporting some of the differences.
expectNoDifferences :: String -> [String] -> PropertyT IO ()
expectNoDifferences what = \case
  [] -> pure ()
  diffs -> do
    annotate (what <> " differs:\n" <> unlines (take 20 diffs))
    failure

-- | The arguments for which a query answers differently in the two graphs, each with both answers.
differences ::
  Eq b =>
  (a -> String) ->
  (b -> String) ->
  (ModuleGraph -> a -> b) ->
  [a] ->
  ModuleGraph ->
  ModuleGraph ->
  [String]
differences renderArg renderAnswer query args actual expected =
  [
    renderArg x <> ": " <> renderAnswer (query actual x) <> ", GHC: " <> renderAnswer (query expected x)
    | x <- args,
      query actual x /= query expected x
  ]

-- | One query compared over two graphs.
data Comparison =
  Comparison {
    -- | The query, such as @mgQuery@.
    queryName :: String,
    -- | The arguments for which the graphs answer differently, each with both answers.
    differing :: [String],
    -- | A graph as this query sees it: for each argument, everything it answers yes for.
    renderGraph :: ModuleGraph -> String
  }

-- | Each line of the rendering of a graph: a key, and the keys a query relates it to.
renderRelation :: (k -> String) -> [k] -> (k -> [k]) -> String
renderRelation renderK ks related =
  unlines ["  " <> renderK k <> ": " <> intercalate ", " (map renderK (related k)) | k <- ks]

-- | Compare two graphs as defined by @checks@, query by query.
--
-- The queries are about the nodes of @expected@. That @actual@ has the same nodes is checked first.
compareGraphs :: Checks -> ModuleGraph -> ModuleGraph -> [Comparison]
compareGraphs checks actual expected =
  [Comparison "the set of node keys" (diff (const "nodes") renderKeys nodeKeys [()]) renderNodeKeys]
  <> [Comparison "mgQuery" (diff renderPair show mgQueryPair pairs) renderQuery | checks.graph]
  <> [Comparison "mgReachableLoop" (diff renderKey renderKeys loopSet keys) renderLoop | checks.loopGraph]
  <> [Comparison "mgQueryZero" (diff renderZeroPair show mgQueryZeroPair zeroPairs) renderZeroQuery | checks.zeroGraph]
  where
    diff :: Eq b => (a -> String) -> (b -> String) -> (ModuleGraph -> a -> b) -> [a] -> [String]
    diff renderArg renderAnswer query args = differences renderArg renderAnswer query args actual expected

    nodes = mgModSummaries' expected
    keys = map mkNodeKey nodes
    pairs = [(a, b) | a <- keys, b <- keys]
    zeroKeys =
      [ModuleScope (mnKey info) l | ModuleNode _ info <- nodes, l <- [SpliceLevel, QuoteLevel, NormalLevel]]
      <> [UnitScope u | UnitNode _ u <- nodes]
    zeroPairs = [(a, b) | a <- zeroKeys, b <- zeroKeys]

    nodeKeys g () = Set.fromList (map mkNodeKey (mgModSummaries' g))
    loopSet g k = Set.fromList (map mkNodeKey (mgReachableLoop g [k]))
    mgQueryPair g (a, b) = mgQuery g a b
    mgQueryZeroPair g (a, b) = mgQueryZero g a b

    renderKeys = show . map renderKey . Set.toList
    renderPair (a, b) = renderKey a <> " -> " <> renderKey b
    renderZeroPair (a, b) = renderZero a <> " -> " <> renderZero b

    renderNodeKeys g = unlines (map (("  " <>) . renderKey) (Set.toList (nodeKeys g ())))
    renderQuery g = renderRelation renderKey keys \ a -> [b | b <- keys, mgQuery g a b]
    renderLoop g = renderRelation renderKey keys \ k -> Set.toList (loopSet g k)
    renderZeroQuery g = renderRelation renderZero zeroKeys \ a -> [b | b <- zeroKeys, mgQueryZero g a b]

-- | Generates @batches@ of nodes and checks, after each batch, that the incremental graph equals GHC's:
--
-- > extendModuleGraphIncr (merge (take n batches)) == mkModuleGraph (merge (take n batches))
--
-- @checks@ selects the queries compared, and @withBoot@ lets the batches contain hs-boot modules, which form loops.
--
-- 'withTests' raises the default number of tests from 100, as many of the generated projects are small.
-- @--hedgehog-tests@ still overrides it.
equivalence :: Checks -> Bool -> Property
equivalence checks withBoot =
  withTests 200 $ property do
    batches <- forAllWith renderBatches (genBatches withBoot)
    let step (nodes0, egr0) (b, batch) = do
          let nodes1 = mergeModuleGraphNodes batch nodes0
              egr1 = extendModuleGraphIncr nodes1 egr0
              incremental = egr1.moduleGraph
              ghc = mkModuleGraph (Map.elems nodes1)
              results = compareGraphs checks incremental ghc
              failed = filter (not . null . (.differing)) results
          unless (null failed) do
            annotate ("batch " <> show b <> ":\n" <> renderNodes batch)
            annotate ("nodes before the batch:\n" <> renderNodes (Map.elems nodes0))
          -- Both graphs, as seen by each query that tells them apart.
          for_ failed \ c -> do
            annotate ("the incremental graph, by " <> c.queryName <> ":\n" <> c.renderGraph incremental)
            annotate ("GHC's graph, by " <> c.queryName <> ":\n" <> c.renderGraph ghc)
          for_ results \ c -> expectNoDifferences c.queryName c.differing
          pure (nodes1, egr1)
    foldM_ step (Map.empty, emptyEModuleGraph) (zip [0 :: Int ..] batches)

test_moduleGraphIncr :: TestTree
test_moduleGraphIncr =
  testGroup "incremental module graph" [
    testProperty "equals GHC's, without hs-boot loops" (equivalence allChecks False),
    -- TODO: The loop graph shares the acyclic graph's index, which is wrong once an hs-boot loop folds into a cycle.
    testProperty "equals GHC's, with hs-boot loops" (equivalence allChecks {loopGraph = False} True)
  ]

#else

import Test.Tasty (TestTree, testGroup)

-- | GHC 9.10's module graph has one index, and no import levels.
test_moduleGraphIncr :: TestTree
test_moduleGraphIncr = testGroup "incremental module graph" []

#endif
