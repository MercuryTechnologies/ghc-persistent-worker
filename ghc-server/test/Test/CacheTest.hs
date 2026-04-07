module Test.CacheTest where

import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd, reachablesG)
import qualified GHC.Data.Graph.Directed as Graph (Node (..))
import GhcServer.Cache (depLoadOrder)
import qualified Hedgehog as H
import Hedgehog (Property, property, withTests, (/==))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Build a 'Graph.Node' with an 'Int' key and '()' payload.
node :: Int -> [Int] -> Graph.Node Int ()
node k deps = Graph.DigraphNode {node_payload = (), node_key = k, node_dependencies = deps}

-- | Diamond DAG: 3 → {1, 2} → 0.
--
-- Edges (dependent → dependency):
-- > 3 → 1, 3 → 2, 1 → 0, 2 → 0
diamondGraph :: [(Int, Graph.Node Int ())]
diamondGraph =
  [ (0, node 0 [])
  , (1, node 1 [0])
  , (2, node 2 [0])
  , (3, node 3 [1, 2])
  ]

-- | 'depLoadOrder' produces a different (and correct) order than reversed 'reachablesG'
-- for the diamond DAG, where node 0 is a shared ancestor of both 1 and 2.
--
-- 'reachablesG' DFS pre-order from node 3: [3, 1, 0, 2].
-- Reversed: [2, 0, 1] — node 2 before node 0, which is wrong.
-- 'depLoadOrder' from node 3: [0, 1, 2] — node 0 before both 1 and 2.
prop_depLoadOrderDiffersFromReversedReachables :: Property
prop_depLoadOrderDiffersFromReversedReachables = withTests 1 $ property do
  let graph = graphFromEdgedVerticesOrd (map snd diamondGraph)
      root  = node 3 [1, 2]
      reachableKeys = map (.node_key) (drop 1 (reachablesG graph [root]))
      loadOrderKeys = map (.node_key) (depLoadOrder graph root)
  H.annotateShow reachableKeys
  H.annotateShow loadOrderKeys
  -- The two orderings must differ: reversed DFS pre-order is not valid for DAGs.
  reachableKeys /== reverse loadOrderKeys
  -- depLoadOrder must put 0 before both 1 and 2 (0 is a shared dep of both).
  let pos k = length (takeWhile (/= k) loadOrderKeys)
  H.assert (pos 0 < pos 1)
  H.assert (pos 0 < pos 2)

test_depLoadOrder :: TestTree
test_depLoadOrder =
  testGroup "depLoadOrder"
    [ testProperty "differs from reversed reachablesG on diamond DAG" prop_depLoadOrderDiffersFromReversedReachables
    ]
