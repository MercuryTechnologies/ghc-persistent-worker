{-# LANGUAGE CPP #-}
{-# LANGUAGE FieldSelectors #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

module Internal.Compat.ModuleGraph.GHC910 where

#else

module Internal.Compat.ModuleGraph.GHC910 (
  extendMG',
  extendReachIndex,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability (ReachabilityIndex (..))
import GHC.Data.Maybe
import GHC.Types.SourceFile (isHsigFile)
import GHC.Unit.Module.Graph (
  ModuleGraph (..),
  ModuleGraphNode (..),
  ModuleNameHomeMap,
  NodeKey (..),
  SummaryNode,
  moduleNodeInfoHscSource,
  moduleNodeInfoModule,
  moduleNodeInfoModuleName,
  nodeDependencies,
  )
import GHC.Unit.Types
import Internal.Compat.ModuleGraph.Reachability (
  graphReachabilityIncr,
  mkFromTo,
  )
import Types.State.Make (EModuleGraph (..), KeyIndexNodeMap (..))

extendMGInst :: ModuleGraph -> UnitId -> InstantiatedUnit -> ModuleGraph
extendMGInst mg uid depUnitId = mg
  { mg_mss = InstantiationNode uid depUnitId : mg_mss mg
  }

extendMGLink :: ModuleGraph -> UnitId -> [NodeKey] -> ModuleGraph
extendMGLink mg uid nks = mg { mg_mss = LinkNode nks uid : mg_mss mg }

-- this incremental extendMG' temporarily results in inconsistent module graph state.
extendMG' :: (NodeKey, (Int, ModuleGraphNode)) -> EModuleGraph -> EModuleGraph
extendMG' kinode@(_, (_, node)) (EModuleGraph {moduleGraph = mg,  keyIndexNodeMap = kinMap}) =
  case node of
    InstantiationNode uid depUnitId ->
      EModuleGraph {
         moduleGraph = extendMGInst mg uid depUnitId,
         keyIndexNodeMap = kinMap
       }
    LinkNode deps uid ->
      EModuleGraph {
         moduleGraph = extendMGLink mg uid deps,
         keyIndexNodeMap = kinMap
       }
    ModuleNode deps mni ->
      let (kinMap', lookup_node) = moduleGraphNodesIncr kinode kinMap
          -- NOTE: This badReachIndex is not consistent. This is only transiently existent.
          (badReachIndex, _) = mg_graph mg
          mg' = mg
            { mg_mss = node : mg_mss mg,
              mg_graph = (badReachIndex, lookup_node),
              mg_home_map = mkHomeModuleMapIncr (ModuleNode deps mni) mg,
              mg_has_holes = mg_has_holes mg || maybe False isHsigFile (moduleNodeInfoHscSource mni)
            }
       in EModuleGraph {
            moduleGraph = mg',
            keyIndexNodeMap = kinMap'
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
        mg_graph = (reachIndex, lookup_node)
      }
   in EModuleGraph {
        moduleGraph = mg1,
        keyIndexNodeMap = kinMap1
      }

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

    snode = DigraphNode node i (out_edge_idxs (nodeDependencies False node))

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node = flip Map.lookup node_map

    lookup_idx :: NodeKey -> Maybe Int
    lookup_idx = flip Map.lookup key2idx

    node_map :: Map.Map NodeKey SummaryNode
    node_map = Map.insert k snode key2inode

    out_edge_idxs :: [NodeKey] -> [Int]
    out_edge_idxs = mapMaybe lookup_idx

    kinMap' = kinMap { keyINodeMap = node_map }

mkHomeModuleMapIncr :: ModuleGraphNode -> ModuleGraph -> ModuleNameHomeMap
mkHomeModuleMapIncr node mg =
  (complete_units, provider_map)
  where
    nodes = node : mg_mss mg
    -- TODO: further incrementalize this part
    provider_map =
      Map.fromListWith Set.union
        [ (moduleNodeInfoModuleName ms, Set.singleton (toUnitId (moduleUnit (moduleNodeInfoModule ms))))
        | ModuleNode _ ms <- nodes
        ]
    -- TODO: further incrementalize this part
    complete_units =
      Set.fromList
        [ toUnitId (moduleUnit (moduleNodeInfoModule ms))
        | ModuleNode _ ms <- nodes
        ]

#endif
