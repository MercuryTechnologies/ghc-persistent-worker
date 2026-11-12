{-# LANGUAGE CPP #-}
{-# LANGUAGE FieldSelectors #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

module Internal.Compat.ModuleGraph.GHC914 (
  extendMG,
  extendReachIndex,
) where

import Data.Graph (SCC (..), stronglyConnComp)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability (ReachabilityIndex (..))
import GHC.Data.Maybe
import GHC.Types.SourceFile (isHsigFile)
import GHC.Unit.Module.Graph (
  ImportLevel (..),
  ModNodeKeyWithUid (..),
  ModuleGraph (..),
  ModuleGraphNode (..),
  ModuleNodeEdge (..),
  NodeKey (..),
  SummaryNode,
  ZeroScopeKey(..),
  isBootModuleNodeInfo,
  mgNodeDependencies,
  mgNodeIsModule,
  mnKey,
  moduleNodeInfoHscSource,
  )
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..))
import Internal.Compat.ModuleGraph.Reachability (
  graphReachabilityIncr,
  mkFromTo,
  )
import Types.State.Make (EModuleGraph (..), KeyIndexNodeMap (..))

type ZeroSummaryNode = Node Int ZeroScopeKey


-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
moduleGraphNodesIncr ::
  (NodeKey, (Int, ModuleGraphNode)) ->
  KeyIndexNodeMap ModuleGraphNode ->
  (KeyIndexNodeMap ModuleGraphNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodesIncr (k, (i, node)) kinMap = (kinMap', lookup_node)
  where
    key2idx = keyIdxMap kinMap
    key2inode = keyINodeMap kinMap

    -- TODO: GHC 9.14 holds cyclic dependency modgraph, but we are not supporting boot files well yet.
    --       Revisit this when we support boot files.
    snode = DigraphNode node i (out_edge_idxs (mgNodeDependencies False {- = drop_hs_boot_nodes -} node))

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node = flip Map.lookup node_map

    lookup_idx :: NodeKey -> Maybe Int
    lookup_idx = flip Map.lookup key2idx

    node_map :: Map.Map NodeKey SummaryNode
    node_map = Map.insert k snode key2inode

    out_edge_idxs :: [NodeKey] -> [Int]
    out_edge_idxs = mapMaybe lookup_idx

    kinMap' = kinMap { keyINodeMap = node_map }


-- | The level graph, GHC's @mg_zero_graph@, has a vertex per module and import level, and one per external unit. A
-- module's vertex at a level points at the normal-level vertices of the modules it imports at that level, and at the
-- external units it depends on at that level. So the modules visible at a level of a module are the ones reachable from
-- its vertex for that level.
--
-- The worker numbers these vertices after the node's own vertex @i@ in the acyclic graph: @3i@ to @3i + 2@ for a
-- module, one per level, and @3i@ for an external unit. A node's vertices are then fixed once the node is added, and
-- need no map of their own.
zeroVertex :: KeyIndexNodeMap ModuleGraphNode -> ZeroScopeKey -> Maybe Int
zeroVertex kinMap = \case
  ModuleScope k l -> (\ i -> 3 * i + levelOffset l) <$> Map.lookup (NodeKey_Module k) (keyIdxMap kinMap)
  UnitScope u -> (3 *) <$> Map.lookup (NodeKey_ExternalUnit u) (keyIdxMap kinMap)
  where
    levelOffset = \case
      SpliceLevel -> 0
      QuoteLevel -> 1
      NormalLevel -> 2

-- | The vertices of a node in the level graph, each with its key and out-edges. Out-edges to keys not in the graph are
-- dropped, as GHC drops them.
zeroVertices :: KeyIndexNodeMap ModuleGraphNode -> ModuleGraphNode -> [(Int, ZeroScopeKey, [Int])]
zeroVertices kinMap = \case
  ModuleNode edges info ->
    [ (v, key, mapMaybe (zeroVertex kinMap) (mapMaybe (classify l) edges))
    | l <- [SpliceLevel, QuoteLevel, NormalLevel]
    , let key = ModuleScope (mnKey info) l
    , Just v <- [zeroVertex kinMap key]
    ]
  UnitNode deps u ->
    [ (v, key, mapMaybe (zeroVertex kinMap . UnitScope) deps)
    | let key = UnitScope u
    , Just v <- [zeroVertex kinMap key]
    ]
  _ -> []
  where
    -- An import at a level reaches the normal level of the module it imports.
    classify l = \case
      ModuleNodeEdge il (NodeKey_Module k) | il == l -> Just (ModuleScope k NormalLevel)
      ModuleNodeEdge il (NodeKey_ExternalUnit u) | il == l -> Just (UnitScope u)
      _ -> Nothing

-- | Extend the level graph for the nodes with the given vertices in the acyclic graph, taking the previous index as it
-- is.
--
-- Like 'graphReachabilityIncr', this relies on two properties: the level graph is acyclic, as its edges follow the
-- acyclic graph's, and no existing node gains an edge, so the sets of existing vertices stay valid. A new vertex's set
-- is its out-edges and their sets, taken from the previous index or from the new entries.
extendZeroGraph ::
  KeyIndexNodeMap ModuleGraphNode ->
  [Int] ->
  ReachabilityIndex ZeroSummaryNode ->
  (ReachabilityIndex ZeroSummaryNode, ZeroScopeKey -> Maybe ZeroSummaryNode)
extendZeroGraph kinMap newIdxs old =
  (ReachabilityIndex {index = reach, from_vertex = fromVertex, to_vertex = Just . node_key}, lookupNode)
  where
    newVertices =
      [ (v, edges)
      | i <- newIdxs
      , Just node <- [IM.lookup i (idxNodeMap kinMap)]
      , (v, _, edges) <- zeroVertices kinMap node
      ]

    -- Lazy in the values, so a new set may read the sets of the new vertices it points at.
    reach = index old `IM.union` IM.fromList [(v, setOf edges) | (v, edges) <- newVertices]

    setOf edges = IS.unions (IS.fromList edges : mapMaybe (`IM.lookup` reach) edges)

    fromVertex v = fromMaybe (error "extendZeroGraph: unknown vertex") do
      node <- IM.lookup (v `div` 3) (idxNodeMap kinMap)
      listToMaybe [DigraphNode key v edges | (w, key, edges) <- zeroVertices kinMap node, w == v]

    lookupNode key = fromVertex <$> zeroVertex kinMap key

-- | The loop graph, GHC's @mg_loop_graph@, folds each hs-boot node into the node of its module: the boot node has no
-- vertex, its module's vertex also gets the boot node's out-edges, and an edge to a boot node goes to its module
-- instead. That may close a cycle, as an hs-boot file exists to break one. The linker reads this graph for splices.
--
-- The worker numbers it as the acyclic graph, without the boot nodes' vertices. Its index is extended for the new
-- nodes only, by the strongly connected components (SCCs) of the new vertices in topological order, as GHC's
-- @cyclicGraphReachability@ does over the whole graph. This relies on no existing node gaining an edge, so that every
-- cycle lies among the new vertices, which holds as an hs-boot file is only imported from its own unit.
--
-- A new vertex whose set in the acyclic graph has no folded vertex, and that is not folded itself, reaches the same
-- vertices in both graphs. It shares the acyclic graph's set, so a graph without hs-boot files keeps one set per vertex.
extendLoopGraph ::
  KeyIndexNodeMap ModuleGraphNode ->
  [Int] ->
  ReachabilityIndex SummaryNode ->
  (NodeKey -> Maybe SummaryNode) ->
  ((ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode), IS.IntSet)
extendLoopGraph kinMap newIdxs old acyclicLookup =
  ((ReachabilityIndex {index = reach, from_vertex = from, to_vertex = to}, lookupNode), folded)
  where
    (from, to) = mkFromTo kinMap

    keyIdx = keyIdxMap kinMap

    nodeAt i = IM.lookup i (idxNodeMap kinMap)

    acyclicSet v = IM.findWithDefault IS.empty v (reachabilityMap kinMap)

    newNodes = [(i, node) | i <- newIdxs, Just node <- [nodeAt i]]

    newVertices = [(i, node) | (i, node) <- newNodes, not (isBootNode node)]

    folded =
      foldedIdxs kinMap `IS.union` IS.fromList (
        [i | (i, node) <- newNodes, isBootNode node]
        <> [j | (_, node) <- newNodes, isBootNode node, Just k <- [otherFile node], Just j <- [Map.lookup k keyIdx]]
        <> [i | (i, node) <- newVertices, Just k <- [otherFile node], Map.member k keyIdx]
      )

    -- The key of a module's hs-boot node, or of an hs-boot node's module.
    otherFile node = do
      info <- mgNodeIsModule node
      let ModNodeKeyWithUid (GWIB m boot) u = mnKey info
          other = case boot of
            IsBoot -> NotBoot
            NotBoot -> IsBoot
      pure (NodeKey_Module (ModNodeKeyWithUid (GWIB m other) u))

    -- Out-edges with hs-boot nodes folded, as GHC's @moduleGraphNodes True@.
    loopEdges node = mapMaybe (`Map.lookup` keyIdx) (bootDeps <> mgNodeDependencies True node)
      where
        bootDeps = fromMaybe [] do
          k <- otherFile node
          boot <- nodeAt =<< Map.lookup k keyIdx
          pure (mgNodeDependencies True boot)

    edgesOf = IM.fromList [(i, loopEdges node) | (i, node) <- newVertices]

    isNew v = IM.member v edgesOf

    sccs = stronglyConnComp [(v, v, filter isNew edges) | (v, edges) <- IM.toList edgesOf]

    shareable v = not (IS.member v folded) && IS.disjoint (acyclicSet v) folded

    -- Lazy in the values, so a new set may read the sets of the new vertices it points at.
    reach = index old `IM.union` IM.fromList (concatMap entries sccs)

    entries = \case
      AcyclicSCC v -> [(v, if shareable v then acyclicSet v else outside [v])]
      CyclicSCC vs -> [(v, IS.delete v (IS.fromList vs) `IS.union` outside vs) | v <- vs]

    -- What a component reaches outside itself: its out-edges leaving it, and their sets.
    outside vs =
      let leaving = IS.fromList (concatMap (\ v -> IM.findWithDefault [] v edgesOf) vs) IS.\\ IS.fromList vs
       in IS.unions (leaving : mapMaybe (`IM.lookup` reach) (IS.toList leaving))

    -- An hs-boot node has no vertex in the loop graph.
    lookupNode = \case
      NodeKey_Module (ModNodeKeyWithUid (GWIB _ IsBoot) _) -> Nothing
      key -> acyclicLookup key

isBootNode :: ModuleGraphNode -> Bool
isBootNode node = maybe False ((== IsBoot) . isBootModuleNodeInfo) (mgNodeIsModule node)


-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: (NodeKey, (Int, ModuleGraphNode)) -> EModuleGraph -> EModuleGraph
extendMG kinode@(_, (_, node)) EModuleGraph {moduleGraph = mg, keyIndexNodeMap = kinMap} =
  EModuleGraph {
    moduleGraph = mg',
    keyIndexNodeMap = kinMap'
  }
  where
    (kinMap', lookup_node) = moduleGraphNodesIncr kinode kinMap
    (badReachIndex, _) = mg_graph mg
    mg' = ModuleGraph
      { mg_mss = node : mg_mss mg,
        mg_graph =  (badReachIndex, lookup_node),
        mg_loop_graph = mg_loop_graph mg,
        mg_zero_graph = mg_zero_graph mg,
        mg_has_holes = mg_has_holes mg || maybe False isHsigFile (moduleNodeInfoHscSource =<< mgNodeIsModule node)
      }

-- | Recompute the reachability index over the nodes added since the last
-- call, and store it in both the 'ModuleGraph' and the 'KeyIndexNodeMap'.
--
-- 'extendMG' deliberately leaves a stale index in place as it adds nodes,
-- so that a batch of inserts pays for the index once instead of once per
-- node. This repairs it, and must be called after a batch of 'extendMG'
-- and before anything queries reachability.
--
-- The incremental work is in 'graphReachabilityIncr', 'extendLoopGraph'
-- and 'extendZeroGraph'. See them for the acyclicity and monotonicity they
-- assume.
extendReachIndex :: EModuleGraph -> EModuleGraph
extendReachIndex emg0 =
  let mg0 = emg0.moduleGraph
      kinMap0 = emg0.keyIndexNodeMap
      kinMap1 = graphReachabilityIncr kinMap0

      -- The nodes added since the last call. They are numbered after every node before them.
      newIdxs = case IM.lookupMax (reachabilityMap kinMap0) of
        Just (oldMax, _) -> IM.keys (snd (IM.split oldMax (idxKeyMap kinMap0)))
        Nothing -> IM.keys (idxKeyMap kinMap0)
      reachGraph = reachabilityMap kinMap1
      (from, to) = mkFromTo kinMap1
      reachIndex = ReachabilityIndex {index = reachGraph, from_vertex = from, to_vertex = to}
      (_, lookup_node) = mg_graph mg0

      (loopGraph, folded) = extendLoopGraph kinMap1 newIdxs (fst (mg_loop_graph mg0)) lookup_node

      mg1 = mg0 {
        mg_graph = (reachIndex, lookup_node),
        mg_loop_graph = loopGraph,
        mg_zero_graph = extendZeroGraph kinMap1 newIdxs (fst (mg_zero_graph mg0))
      }
   in EModuleGraph {
        moduleGraph = mg1,
        keyIndexNodeMap = kinMap1 {foldedIdxs = folded}
      }

#else

module Internal.Compat.ModuleGraph.GHC914 where

#endif
