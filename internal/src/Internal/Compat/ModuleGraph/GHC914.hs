{-# LANGUAGE CPP #-}
{-# LANGUAGE FieldSelectors #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

module Internal.Compat.ModuleGraph.GHC914 (
  extendMG,
  extendReachIndex,
) where

import Data.Bifunctor
import qualified Data.Map as Map
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability (ReachabilityIndex (..))
import GHC.Data.Maybe
import GHC.Types.SourceFile (isHsigFile)
import GHC.Unit.Module.Graph (
  ImportLevel (..),
  ModuleGraph (..),
  ModuleGraphNode (..),
  ModuleNodeEdge (..),
  NodeKey (..),
  SummaryNode,
  ZeroScopeKey(..),
  mgNodeDependencies,
  mgNodeIsModule,
  mnKey,
  moduleNodeInfoHscSource,
  )
import Internal.Compat.ModuleGraph.Reachability (
  -- TODO: use cyclic reachability when it is supported.
  {- , cyclicGraphReachability -}
  graphReachability,
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


-- | Turn a list of graph nodes into an efficient queriable graph.
-- This graph only has edges between level-0 imports
--
-- This query answers the question. If I am looking at level n in module M then which
-- modules are visible?
--
-- If you are looking at level -1  then the reachable modules are those imported at splice and
-- then any modules those modules import at zero. (Ie the zero scope for those modules)
moduleGraphNodesZero ::
     [ModuleGraphNode]
  -> (Graph ZeroSummaryNode, ZeroScopeKey -> Maybe ZeroSummaryNode)
moduleGraphNodesZero summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    nodes = mapMaybe go numbered_summaries

      where
        go :: (((ModuleGraphNode, ImportLevel)), Int) -> Maybe ZeroSummaryNode
        go (((ModuleNode nks ms), s), key) = Just $
               DigraphNode (ModuleScope (mnKey ms) s) key $ out_edge_keys $
                    mapMaybe (classifyDeps s) nks
        go (((UnitNode uids uid), _s), key) =
          Just $ DigraphNode (UnitScope uid) key (mapMaybe lookup_key $ map UnitScope uids)
        go _ = Nothing

    -- This is the key part, a dependency edge also depends on the NormalLevel scope of an import.
    classifyDeps s (ModuleNodeEdge il (NodeKey_Module k)) | s == il = Just (ModuleScope k NormalLevel)
    classifyDeps s (ModuleNodeEdge il (NodeKey_ExternalUnit u)) | s == il = Just (UnitScope u)
    classifyDeps _ _ = Nothing

    numbered_summaries :: [((ModuleGraphNode, ImportLevel), Int)]
    numbered_summaries = zip (([(s, l) | s <- summaries, l <- [SpliceLevel, QuoteLevel, NormalLevel]])) [0..]

    lookup_node :: ZeroScopeKey -> Maybe ZeroSummaryNode
    lookup_node key = Map.lookup key node_map

    lookup_key :: ZeroScopeKey -> Maybe Int
    lookup_key = fmap node_key . lookup_node

    node_map :: Map.Map ZeroScopeKey ZeroSummaryNode
    node_map =
      Map.fromList [ (s, node)
                   | node <- nodes
                   , let s = node_payload node
                   ]

    out_edge_keys :: [ZeroScopeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key

-- | Transitive dependencies, but only following "normal" level 0 imports.
-- This graph can be used to query what the transitive dependencies of a particular
-- level are within a module.
mkTransZeroDeps :: [ModuleGraphNode] -> (ReachabilityIndex ZeroSummaryNode, ZeroScopeKey -> Maybe ZeroSummaryNode)
mkTransZeroDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodesZero


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
        mg_loop_graph = (badReachIndex, lookup_node),
        -- TODO: This is not yet incrementalized.
        mg_zero_graph = mkTransZeroDeps (node : mg_mss mg),
        mg_has_holes = mg_has_holes mg || maybe False isHsigFile (moduleNodeInfoHscSource =<< mgNodeIsModule node)
      }

extendReachIndex :: EModuleGraph -> EModuleGraph
extendReachIndex emg0 =
  let mg0 = emg0.moduleGraph
      kinMap0 = emg0.keyIndexNodeMap
      kinMap1 = graphReachabilityIncr kinMap0
      reachGraph = reachabilityMap kinMap1
      (from, to) = mkFromTo kinMap1
      reachIndex = ReachabilityIndex {index = reachGraph, from_vertex = from, to_vertex = to}
      (_, lookup_node) = mg_graph mg0

      mg1 = mg0 {
        mg_graph = (reachIndex, lookup_node),
        mg_loop_graph = (reachIndex, lookup_node)
        -- TODO: This is not yet incrementalized.
        -- mg_zero_graph
      }
   in EModuleGraph {
        moduleGraph = mg1,
        keyIndexNodeMap = kinMap1
      }

#else

module Internal.Compat.ModuleGraph.GHC914 where

#endif
