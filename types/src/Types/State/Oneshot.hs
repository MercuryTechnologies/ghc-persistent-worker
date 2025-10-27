{-# LANGUAGE CPP #-}

module Types.State.Oneshot where

import Data.Map.Strict (Map)
import GHC.Data.FastString (FastString)
import GHC.Linker.Loader (LoaderState)
import GHC.Ptr (Ptr)
import GHC.Types.Name.Cache (OrigNameCache)
import GHC.Types.Unique.FM (UniqFM)
import GHC.Unit (emptyModuleEnv)
import GHC.Unit.External (ExternalUnitCache, initExternalUnitCache)
import Types.State.Finder (FinderState, emptyFinderState)
import Types.State.Stats (CacheStats)
import Types.Target (Target)

data OneshotCacheFeatures =
  OneshotCacheFeatures {
    enable :: Bool,
    loader :: Bool,
    names :: Bool,
    finder :: Bool,
    eps :: Bool
  }
  deriving stock (Eq, Show)

newOneshotCacheFeatures :: OneshotCacheFeatures
newOneshotCacheFeatures = OneshotCacheFeatures {enable = True, loader = True, names = True, finder = True, eps = True}

type SymbolMap = UniqFM FastString (Ptr ())

newtype SymbolCache =
  SymbolCache { symbols :: SymbolMap }
  deriving newtype (Semigroup, Monoid)

data InterpCache =
  InterpCache {
    loaderState :: LoaderState,
    symbols :: SymbolCache
  }

data OneshotState =
  OneshotState {
    features :: OneshotCacheFeatures,
    interpCache :: Maybe InterpCache,
    names :: OrigNameCache,
    finder :: FinderState,
    eps :: ExternalUnitCache,
    stats :: Map Target CacheStats
  }

newOneshotStateWith :: OneshotCacheFeatures -> IO OneshotState
newOneshotStateWith features = do
  finder <- emptyFinderState
  eps <- initExternalUnitCache
  pure OneshotState {
    features,
    interpCache = Nothing,
    names = emptyModuleEnv,
    stats = mempty,
    finder,
    eps
  }

newOneshotState :: Bool -> IO OneshotState
newOneshotState enable = do
  newOneshotStateWith newOneshotCacheFeatures {enable}
