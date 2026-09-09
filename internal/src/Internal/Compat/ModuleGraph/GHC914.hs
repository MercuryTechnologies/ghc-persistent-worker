{-# LANGUAGE CPP #-}
{-# LANGUAGE FieldSelectors #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

module Internal.Compat.ModuleGraph.GHC914 (
  mkModuleGraph,
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
  emptyMG,
  isBootModuleNodeInfo,
  mgNodeDependencies,
  mgNodeIsModule,
  mkNodeKey,
  mnKey,
  moduleNodeInfoHscSource,
  moduleNodeInfoModule,
  summaryNodeKey,
  summaryNodeSummary,
  )
import GHC.Unit.Types
import GHC.Utils.Misc ( partitionWith )
import Internal.Compat.ModuleGraph.Reachability (graphReachability, cyclicGraphReachability)

type ZeroSummaryNode = Node Int ZeroScopeKey

-- | Construct a module graph. This function should be the only entry point for
-- building a 'ModuleGraph', since it is supposed to be built once and never modified.
--
-- If you ever find the need to build a 'ModuleGraph' iteratively, don't
-- add insert and update functions to the API since they become footguns.
-- Instead, design an API that allows iterative construction without posterior
-- modification, perhaps like what is done for building arrays from mutable
-- arrays.
mkModuleGraph :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG) emptyMG

-- | Turn a list of graph nodes into an efficient queriable graph.
-- The first boolean parameter indicates whether nodes corresponding to hs-boot files
-- should be collapsed into their relevant hs nodes.
moduleGraphNodes :: Bool
  -> [ModuleGraphNode]
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    (boot_summaries, nodes) = bimap Map.fromList id $ partitionWith go numbered_summaries

      where
        go (s, key) =
          case s of
                ModuleNode __deps ms | isBootModuleNodeInfo ms == IsBoot, drop_hs_boot_nodes
                  -- Using nodeDependencies here converts dependencies on other
                  -- boot files to dependencies on dependencies on non-boot files.
                  -> Left (moduleNodeInfoModule ms, mgNodeDependencies drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key = moduleNodeInfoModule <$> mgNodeIsModule s
                  extra = (lkup_key >>= \key -> Map.lookup key boot_summaries)

              in Right $ DigraphNode s key $ out_edge_keys $
                      (fromMaybe [] extra
                        ++ mgNodeDependencies drop_hs_boot_nodes s)

    numbered_summaries = zip summaries [1..]

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node key = Map.lookup key (unNodeMap node_map)

    lookup_key :: NodeKey -> Maybe Int
    lookup_key = fmap summaryNodeKey . lookup_node

    node_map :: NodeMap SummaryNode
    node_map = NodeMap $
      Map.fromList [ (mkNodeKey s, node)
                   | node <- nodes
                   , let s = summaryNodeSummary node
                   ]

    out_edge_keys :: [NodeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False

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
    lookup_key = fmap zeroSummaryNodeKey . lookup_node

    node_map :: Map.Map ZeroScopeKey ZeroSummaryNode
    node_map =
      Map.fromList [ (s, node)
                   | node <- nodes
                   , let s = zeroSummaryNodeSummary node
                   ]

    out_edge_keys :: [ZeroScopeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key

newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

-- | Transitive dependencies, including SOURCE edges
mkTransDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodes False

-- | Transitive dependencies, ignoring SOURCE edges
mkTransLoopDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransLoopDeps = first cyclicGraphReachability . moduleGraphNodes True

-- | Transitive dependencies, but only following "normal" level 0 imports.
-- This graph can be used to query what the transitive dependencies of a particular
-- level are within a module.
mkTransZeroDeps :: [ModuleGraphNode] -> (ReachabilityIndex ZeroSummaryNode, ZeroScopeKey -> Maybe ZeroSummaryNode)
mkTransZeroDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodesZero


zeroSummaryNodeKey :: ZeroSummaryNode -> Int
zeroSummaryNodeKey = node_key

zeroSummaryNodeSummary :: ZeroSummaryNode -> ZeroScopeKey
zeroSummaryNodeSummary = node_payload

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG ModuleGraph{..} node =
  ModuleGraph
    { mg_mss = node : mg_mss
    , mg_graph =  mkTransDeps (node : mg_mss)
    , mg_loop_graph = mkTransLoopDeps (node : mg_mss)
    , mg_zero_graph = mkTransZeroDeps (node : mg_mss)
    , mg_has_holes = mg_has_holes || maybe False isHsigFile (moduleNodeInfoHscSource =<< mgNodeIsModule node)
    }



#else

module Internal.Compat.ModuleGraph.GHC914 where

#endif
