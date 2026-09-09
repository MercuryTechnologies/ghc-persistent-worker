module Internal.Compat.ModuleGraph.Reachability where

import Data.Array ((!))
import Data.Graph ( Vertex, SCC(..) )

import qualified Data.Graph as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import GHC.Data.Graph.Directed.Internal (Graph (..), scc)
import GHC.Data.Graph.Directed.Reachability (ReachabilityIndex (..))
import GHC.Data.Maybe

-- | Construct a 'ReachabilityIndex' from an acyclic 'Graph'.
-- If the graph can have cycles, use 'cyclicGraphReachability'
graphReachability :: Graph node -> ReachabilityIndex node
graphReachability (Graph g from to) =
  ReachabilityIndex{index = reachableGraph, from_vertex = from, to_vertex = to}
    where
      reachableGraph :: IM.IntMap IS.IntSet
      reachableGraph = IM.fromList [(v, do_one v) | v <- G.vertices g]

      do_one v = IS.unions (IS.fromList (g ! v) : mapMaybe (flip IM.lookup reachableGraph) (g ! v))

-- | Construct a 'ReachabilityIndex' from a 'Graph' which may have cycles.
-- If this reachability index is just going to be used once, it may make sense
-- to use 'reachablesG' instead, which will traverse the reachable nodes without
-- constructing the index -- which may be faster.
cyclicGraphReachability :: Graph node -> ReachabilityIndex node
cyclicGraphReachability (Graph g from to) =
  ReachabilityIndex{index = reachableGraphCyclic, from_vertex = from, to_vertex = to}
    where
      reachableGraphCyclic :: IM.IntMap IS.IntSet
      reachableGraphCyclic = foldl' add_one_comp mempty comps

      neighboursOf v = g!v

      comps = scc g

      -- To avoid divergence on cyclic input, we build the result
      -- strongly connected component by component, in topological
      -- order. For each SCC, we know that:
      --
      --   * All vertices in the component can reach all other vertices
      --     in the component ("local" reachables)
      --
      --   * Other reachable vertices ("remote" reachables) must come
      --     from earlier components, either via direct neighbourhood, or
      --     transitively from earlier reachability map
      --
      -- This allows us to build the extension of the reachability map
      -- directly, without any self-reference, thereby avoiding a loop.
      add_one_comp :: IM.IntMap IS.IntSet -> SCC Vertex -> IM.IntMap IS.IntSet
      add_one_comp earlier (AcyclicSCC v) = IM.insert v all_remotes earlier
        where
          earlier_neighbours = neighboursOf v
          earlier_further = mapMaybe (flip IM.lookup earlier) earlier_neighbours
          all_remotes = IS.unions (IS.fromList earlier_neighbours : earlier_further)
      add_one_comp earlier (CyclicSCC vs) = IM.union (IM.fromList [(v, local v `IS.union` all_remotes) | v <- vs]) earlier
        where
          all_locals = IS.fromList vs
          local v = IS.delete v all_locals
              -- Arguably, for a cyclic SCC we should include each
              -- vertex in its own reachable set. However, this could
              -- lead to a lot of extra pain in client code to avoid
              -- looping when traversing the reachability map.
          all_neighbours = IS.fromList (concatMap neighboursOf vs)
          earlier_neighbours = all_neighbours IS.\\ all_locals
          earlier_further = mapMaybe (flip IM.lookup earlier) (IS.toList earlier_neighbours)
          all_remotes = IS.unions (earlier_neighbours : earlier_further)
