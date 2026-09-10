{-# Language CPP #-}

module Internal.Compat.ModuleGraph (
  extendMG',
) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey)
import Internal.Compat.ModuleGraph.GHC914 (extendMG)
import Types.State.Make (EModuleGraph)

extendMG' :: (NodeKey, (Int, ModuleGraphNode)) -> EModuleGraph -> EModuleGraph
extendMG' = extendMG

#else

import Internal.Compat.ModuleGraph.GHC910 (extendMG')

#endif
