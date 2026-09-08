{-# Language CPP #-}

module Internal.Compat.ModuleGraph (
  mkModuleGraph,
) where

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import Internal.Compat.ModuleGraph.GHC914 (mkModuleGraph)

#else

import Internal.Compat.ModuleGraph.GHC910 (mkModuleGraph)

#endif
