{-# Language CPP #-}

module Internal.Compat.ModuleGraph (
  extendMG',
) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.Graph (ModuleGraph, ModuleGraphNode)
import Internal.Compat.ModuleGraph.GHC914 (extendMG)

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' = extendMG

#else

import Internal.Compat.ModuleGraph.GHC910 (extendMG')

#endif
