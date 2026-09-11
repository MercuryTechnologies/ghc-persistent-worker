module Internal.Compat.ModuleGraph.Reachability where

import Data.Array ((!))
import Data.Graph ( Vertex, SCC(..) )

import qualified Data.Graph as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List ((\\))
import qualified Data.Map as Map
import GHC.Data.Graph.Directed (Node (node_key, node_dependencies))
import GHC.Data.Graph.Directed.Internal (Graph (..), scc)
import GHC.Data.Graph.Directed.Reachability (ReachabilityIndex (..))
import GHC.Data.Maybe
import GHC.Unit.Module.Graph (SummaryNode)
import Types.State.Make (KeyIndexNodeMap (..))

-- | Construct a 'ReachabilityIndex' from an acyclic 'Graph'.
-- If the graph can have cycles, use 'cyclicGraphReachability'
graphReachability :: Graph node -> ReachabilityIndex node
graphReachability (Graph g from to) =
  ReachabilityIndex{index = reachableGraph, from_vertex = from, to_vertex = to}
    where
      reachableGraph :: IM.IntMap IS.IntSet
      reachableGraph = IM.fromList [(v, do_one v) | v <- G.vertices g]

      do_one v = IS.unions (IS.fromList (g ! v) : mapMaybe (flip IM.lookup reachableGraph) (g ! v))

-- NOTE: Our Vertex = Index
mkFromTo :: KeyIndexNodeMap node -> (G.Vertex -> Node Int node, Node Int node -> Maybe G.Vertex)
mkFromTo kinMap = (from, to)
  where
    k2i = keyIdxMap kinMap
    i2k = idxKeyMap kinMap
    k2s = keyINodeMap kinMap
    from i = fromMaybe (error "graphReachabilityIncr") do
      k <- IM.lookup i i2k
      Map.lookup k k2s
    to = Just . node_key

graphReachabilityIncr ::
  KeyIndexNodeMap node ->
  KeyIndexNodeMap node
graphReachabilityIncr kinMap = kinMap {reachabilityMap = reachGraph}
    where
      k2i = keyIdxMap kinMap
      i2k = idxKeyMap kinMap
      k2s = keyINodeMap kinMap
      from i = fromMaybe (error "graphReachabilityIncr") do
        k <- IM.lookup i i2k
        Map.lookup k k2s
      to = Just . node_key

      reachGraph0 = reachabilityMap kinMap

      allIdxs = IM.keys i2k
      oldIdxs = IM.keys reachGraph0
      newIdxs = allIdxs \\ oldIdxs

      reachGraph :: IM.IntMap IS.IntSet
      reachGraph = reachGraph0 `IM.union` IM.fromList [(i, do_one i) | !i <- newIdxs]

      allIdxSize = length allIdxs
      allIdxSize2 = Map.size k2s
      oldIdxSize = length oldIdxs
      newIdxSize = length newIdxs
      reachGraphSize = (IM.size reachGraph, sum (fmap IS.size reachGraph))

      getDeps i = fromMaybe [] do
        k <- IM.lookup i i2k
        s <- Map.lookup k k2s
        pure (node_dependencies s)

      do_one i =
        let deps = getDeps i
            lookupBoth k =
              case IM.lookup k reachGraph0 of
                Just vs -> Just vs
                Nothing -> IM.lookup k reachGraph
            transitives = mapMaybe lookupBoth deps
        in IS.unions (IS.fromList deps : transitives)

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
