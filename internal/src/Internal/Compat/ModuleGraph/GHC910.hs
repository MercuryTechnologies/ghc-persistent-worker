{-# LANGUAGE CPP #-}
{-# LANGUAGE FieldSelectors #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

module Internal.Compat.ModuleGraph.GHC910 where

#else

module Internal.Compat.ModuleGraph.GHC910 (
  extendMG',
) where

import Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Data.Graph.Directed
import GHC.Data.Graph.Directed.Reachability (ReachabilityIndex (..))
import GHC.Data.Maybe
import GHC.Types.SourceFile (isHsigFile)
import GHC.Unit.Module
import GHC.Unit.Module.Graph (
  ModuleGraph (..),
  ModuleGraphNode (..),
  ModuleNameHomeMap,
  ModuleNodeInfo (..),
  NodeKey (..),
  SummaryNode,
  isBootModuleNodeInfo,
  mgNodeIsModule,
  mkNodeKey,
  moduleNodeInfoHscSource,
  moduleNodeInfoModule,
  moduleNodeInfoModuleName,
  nodeDependencies,
  summaryNodeSummary,
  )
import GHC.Unit.Module.ModSummary
import GHC.Utils.Misc (partitionWith)
import Internal.Compat.ModuleGraph.Reachability (graphReachability)

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey = node_key

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> [NodeKey] -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} deps ms = ModuleGraph
  { mg_mss = new_mss
  , mg_graph = mkTransDeps new_mss
  , mg_home_map = mkHomeModuleMap new_mss
  , mg_has_holes = False
  }
  where
    new_mss = ModuleNode deps (ModuleNodeCompile ms) : mg_mss

extendMGInst :: ModuleGraph -> UnitId -> InstantiatedUnit -> ModuleGraph
extendMGInst mg uid depUnitId = mg
  { mg_mss = InstantiationNode uid depUnitId : mg_mss mg
  }

extendMGLink :: ModuleGraph -> UnitId -> [NodeKey] -> ModuleGraph
extendMGLink mg uid nks = mg { mg_mss = LinkNode nks uid : mg_mss mg }

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' mg = \case
  InstantiationNode uid depUnitId -> extendMGInst mg uid depUnitId
  ModuleNode deps (ModuleNodeCompile ms) -> extendMG mg deps ms
  ModuleNode deps mni -> mg
    { mg_mss = ModuleNode deps mni : mg_mss mg
    , mg_graph = mkTransDeps (ModuleNode deps mni : mg_mss mg)
    , mg_home_map = mkHomeModuleMap (ModuleNode deps mni : mg_mss mg)
    , mg_has_holes = mg_has_holes mg || maybe False isHsigFile (moduleNodeInfoHscSource mni)
    }
  LinkNode deps uid   -> extendMGLink mg uid deps

moduleGraphNodes :: Bool
  -> [ModuleGraphNode]
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    -- Map from module to extra boot summary dependencies which need to be merged in
    (!boot_summaries, !nodes) = bimap Map.fromList id $ partitionWith go numbered_summaries

      where
        go (!s, !key) =
          case s of
                ModuleNode __deps ms | isBootModuleNodeInfo ms == IsBoot, drop_hs_boot_nodes
                  -- Using nodeDependencies here converts dependencies on other
                  -- boot files to dependencies on dependencies on non-boot files.
                  -> Left (moduleNodeInfoModule ms, nodeDependencies drop_hs_boot_nodes s)
                _ -> normal_case
          where
           normal_case =
              let lkup_key = moduleNodeInfoModule <$> mgNodeIsModule s
                  extra = (lkup_key >>= \key -> Map.lookup key boot_summaries)

              in Right $ DigraphNode s key $ out_edge_keys $
                      (fromMaybe [] extra
                        ++ nodeDependencies drop_hs_boot_nodes s)

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
newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

mkTransDeps :: [ModuleGraphNode] -> (ReachabilityIndex SummaryNode, NodeKey -> Maybe SummaryNode)
mkTransDeps = first graphReachability {- module graph is acyclic -} . moduleGraphNodes False


mkHomeModuleMap :: [ModuleGraphNode] -> ModuleNameHomeMap
mkHomeModuleMap nodes =
  (complete_units, provider_map)
  where
    provider_map =
      Map.fromListWith Set.union
        [ (moduleNodeInfoModuleName ms, Set.singleton (toUnitId (moduleUnit (moduleNodeInfoModule ms))))
        | ModuleNode _ ms <- nodes
        ]
    complete_units =
      Set.fromList
        [ toUnitId (moduleUnit (moduleNodeInfoModule ms))
        | ModuleNode _ ms <- nodes
        ]

#endif
