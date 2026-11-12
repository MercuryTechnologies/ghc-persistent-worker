{-# LANGUAGE CPP #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The incremental module graph answers every query as GHC's own construction does.
--
-- Random graphs are added batch by batch, one batch per unit, each batch depending only on itself and on earlier
-- batches, as Buck2 schedules units. After each batch, the incrementally extended graph is compared with
-- 'mkModuleGraph' over the same nodes, query by query.
--
-- The vertices are also numbered as GHC numbers them: the acyclic graph's from
-- 1 and the level graph's from 0, after a node's index where GHC takes its
-- position in its list. The two agree for a first batch, whose nodes the
-- worker indexes in the order of their keys, as 'mkModuleGraph' gets them, so
-- there the vertex numbers are compared too, with GHC 9.10's acyclic graph as
-- well. A later batch's keys may sort among the earlier ones, which moves
-- GHC's positions but not the worker's indices.
module ModuleGraphIncrTest (test_moduleGraphIncr) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Control.Monad (foldM, foldM_, forM, unless)
import Data.Foldable (for_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName, moduleNameString)
import GHC.Data.Graph.Directed (node_key)
import GHC.Unit.Module.Graph (
  ImportLevel (..),
  ModNodeKeyWithUid (..),
  ModuleGraph (..),
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
-- * @j@ imports @a@, and maybe its own boot file
--
-- With the boot file as a node of its own, the graph stays acyclic, as GHC requires. @a@ and @j@ form a loop only
-- when the boot file stands for @j@.
genBatch :: Bool -> [ModNodeKeyWithUid] -> Int -> Gen [ModuleGraphNode]
genBatch withBoot earlier unitIndex = do
  n <- Gen.int (Range.linear 1 6)
  let keys = [moduleKey unitIndex i NotBoot | i <- [0 .. n - 1]]
  -- Proposes two modules @a < j@ of the unit to form an hs-boot loop, and whether @j@ imports its own boot file.
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
      -- Only the modules before @a@: @a@ imports this boot file, and any module from @a@ on may import @a@, so an
      -- import of one of them here could close a cycle that the boot file does not break.
      edges <- genEdges (earlier <> take a keys)
      pure [moduleNode (moduleKey unitIndex j IsBoot) edges]
    Nothing -> pure []
  moduleNodes <- forM (zip [0 ..] keys) \ (i, key) -> do
    edges <- genEdges (earlier <> take i keys)
    -- The edges that close the hs-boot loop, in addition to the random edges: @a@ to the boot file of @j@, and @j@
    -- to @a@. With @ownBoot@, @j@ also imports its own boot file, as GHC's downsweep adds that edge to a module with
    -- an hs-boot file, so that the boot file is compiled first and the module can be checked against it. Without it,
    -- the folding of the boot file into @j@ is tested without that edge's help.
    let loopEdges = case loop of
          Just (a, j, ownBoot)
            | i == a -> [ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey unitIndex j IsBoot))]
            | i == j ->
              ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey unitIndex a NotBoot))
              : [ModuleNodeEdge NormalLevel (NodeKey_Module (moduleKey unitIndex j IsBoot)) | ownBoot]
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

-- | Compare two graphs query by query: @mgQuery@ over @mg_graph@, @mgReachableLoop@ over @mg_loop_graph@, and
-- @mgQueryZero@ over @mg_zero_graph@.
--
-- The queries are about the nodes of @expected@. That @actual@ has the same nodes is checked first.
compareGraphs :: ModuleGraph -> ModuleGraph -> [Comparison]
compareGraphs actual expected = [
    Comparison "the set of node keys" (diff (const "nodes") renderKeys nodeKeys [()]) renderNodeKeys,
    Comparison "mgQuery" (diff renderPair show mgQueryPair pairs) renderQuery,
    Comparison "mgReachableLoop" (diff renderKey renderKeys loopSet keys) renderLoop,
    Comparison "mgQueryZero" (diff renderZeroPair show mgQueryZeroPair zeroPairs) renderZeroQuery
  ]
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
-- @withBoot@ lets the batches contain hs-boot modules, which form loops.
--
-- 'withTests' raises the default number of tests from 100, as many of the generated projects are small.
-- @--hedgehog-tests@ still overrides it.
equivalence :: Bool -> Property
equivalence withBoot =
  withTests 200 $ property do
    batches <- forAllWith renderBatches (genBatches withBoot)
    let step (nodes0, egr0) (b, batch) = do
          let nodes1 = mergeModuleGraphNodes batch nodes0
              egr1 = extendModuleGraphIncr nodes1 egr0
              incremental = egr1.moduleGraph
              ghc = mkModuleGraph (Map.elems nodes1)
              results = compareGraphs incremental ghc
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

-- | Generates a first batch, with hs-boot loops, and checks that every key has
-- the same vertex in the incremental graph as in GHC's, in the acyclic graph
-- and in the level graph.
numbering :: Property
numbering =
  withTests 200 $ property do
    batch <- forAllWith renderNodes (genBatch True [] 0)
    let nodes = mergeModuleGraphNodes batch Map.empty
        incremental = (extendModuleGraphIncr nodes emptyEModuleGraph).moduleGraph
        ghc = mkModuleGraph (Map.elems nodes)
        zeroKeys =
          [ ModuleScope (mnKey info) l
          | ModuleNode _ info <- Map.elems nodes,
            l <- [SpliceLevel, QuoteLevel, NormalLevel]
          ]
          <> [UnitScope u | UnitNode _ u <- Map.elems nodes]
        vertex g k = node_key <$> snd (mg_graph g) k
        zeroVertex g k = node_key <$> snd (mg_zero_graph g) k
    expectNoDifferences "the acyclic graph's vertex numbering"
      [ renderKey k <> ": " <> show (vertex incremental k) <> ", GHC: " <> show (vertex ghc k)
      | k <- Map.keys nodes,
        vertex incremental k /= vertex ghc k
      ]
    expectNoDifferences "the level graph's vertex numbering"
      [ renderZero k <> ": " <> show (zeroVertex incremental k) <> ", GHC: "
          <> show (zeroVertex ghc k)
      | k <- zeroKeys,
        zeroVertex incremental k /= zeroVertex ghc k
      ]

test_moduleGraphIncr :: TestTree
test_moduleGraphIncr =
  testGroup "incremental module graph" [
    testProperty "equals GHC's, without hs-boot loops" (equivalence False),
    testProperty "equals GHC's, with hs-boot loops" (equivalence True),
    testProperty "numbers the vertices as GHC's" numbering
  ]

#else

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import GHC (mkModuleName, moduleNameString)
import GHC.Data.Graph.Directed (node_key)
import GHC.Unit.Module.Graph (
  ModNodeKeyWithUid (..),
  ModuleGraph (..),
  ModuleGraphNode (..),
  ModuleNodeInfo (..),
  NodeKey (..),
  mkModuleGraph,
  )
import GHC.Unit.Module.Location (pattern ModLocation)
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), stringToUnitId, unitIdString)
import Hedgehog (Gen, Property, annotate, failure, forAllWith, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Internal.State.Make (extendModuleGraphIncr, mergeModuleGraphNodes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Types.State.Make (EModuleGraph (..), emptyEModuleGraph)

-- | Module @i@ of the one unit, or its hs-boot file.
moduleKey :: Int -> IsBootInterface -> ModNodeKeyWithUid
moduleKey i boot = ModNodeKeyWithUid (GWIB (mkModuleName ("M" <> show i)) boot) (stringToUnitId "unit")

moduleNode :: ModNodeKeyWithUid -> [NodeKey] -> ModuleGraphNode
moduleNode key deps =
  ModuleNode deps (ModuleNodeFixed key (ModLocation Nothing "M.hi" "M.dyn_hi" "M.o" "M.dyn_o" "M.hie"))

-- | The nodes of one unit: modules that import some of the modules before
-- them, maybe an hs-boot file of the last one, which the first module imports,
-- and a link node that depends on every module.
genBatch :: Gen [ModuleGraphNode]
genBatch = do
  n <- Gen.int (Range.linear 1 6)
  withBoot <- Gen.bool
  let keys = [moduleKey i NotBoot | i <- [0 .. n - 1]]
      boot = moduleKey (n - 1) IsBoot
  modules <- forM (zip [0 ..] keys) \ (i, key) -> do
    deps <- Gen.subsequence (take i keys)
    pure (moduleNode key (map NodeKey_Module (deps <> [boot | withBoot, i == 0, n > 1])))
  pure $
    [moduleNode boot [] | withBoot, n > 1]
    <> modules
    <> [LinkNode (map NodeKey_Module keys) (stringToUnitId "unit")]

renderKey :: NodeKey -> String
renderKey = \case
  NodeKey_Module (ModNodeKeyWithUid (GWIB m boot) _) ->
    moduleNameString m <> (case boot of IsBoot -> "-boot"; NotBoot -> "")
  NodeKey_Link u -> "link " <> unitIdString u
  _ -> "?"

renderNodes :: [ModuleGraphNode] -> String
renderNodes nodes =
  unlines
    [ "  " <> name <> " -> " <> unwords (map renderKey deps)
    | node <- nodes,
      (name, deps) <- case node of
        ModuleNode deps (ModuleNodeFixed key _) -> [(renderKey (NodeKey_Module key), deps)]
        LinkNode deps u -> [("link " <> unitIdString u, deps)]
        _ -> []
    ]

-- | Generates a first batch and checks that every module has the same vertex
-- in the incremental graph as in GHC's.
--
-- The worker leaves link nodes out of its GHC 9.10 graph, so only modules are
-- compared. A link node still takes an index, after every module, as its key
-- sorts after theirs.
numbering :: Property
numbering =
  withTests 200 $ property do
    batch <- forAllWith renderNodes genBatch
    let nodes = mergeModuleGraphNodes batch Map.empty
        incremental = (extendModuleGraphIncr nodes emptyEModuleGraph).moduleGraph
        ghc = mkModuleGraph (Map.elems nodes)
        vertex g k = node_key <$> snd (mg_graph g) k
    case
        [ renderKey k <> ": " <> show (vertex incremental k) <> ", GHC: " <> show (vertex ghc k)
        | k@(NodeKey_Module _) <- Map.keys nodes,
          vertex incremental k /= vertex ghc k
        ]
      of
        [] -> pure ()
        diffs -> do
          annotate ("the acyclic graph's vertex numbering differs:\n" <> unlines (take 20 diffs))
          failure

-- | GHC 9.10's module graph has one index, the acyclic graph's, and no import
-- levels.
test_moduleGraphIncr :: TestTree
test_moduleGraphIncr =
  testGroup "incremental module graph" [
    testProperty "numbers the vertices as GHC's" numbering
  ]

#endif
