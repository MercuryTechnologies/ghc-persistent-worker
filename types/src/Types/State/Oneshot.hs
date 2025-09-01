{-# LANGUAGE CPP #-}

module Types.State.Oneshot where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import GHC.Data.FastString (FastString)
import GHC.Linker.Loader (LoaderState)
import GHC.Ptr (Ptr)
import GHC.Types.Name.Cache (OrigNameCache)
import GHC.Types.Unique.FM (UniqFM)
import GHC.Unit (emptyModuleEnv)
import GHC.Unit.External (ExternalUnitCache, initExternalUnitCache)
import GHC.Unit.Finder (FinderCache)
import Types.State.Stats (CacheStats)
import Types.Target (Target)

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

import Data.IORef (IORef, newIORef)
import qualified Data.Map.Lazy as LazyMap
import GHC.Fingerprint (Fingerprint, getFileHash)
import GHC.IORef (atomicModifyIORef')
import GHC.Unit (
  InstalledModule,
  emptyInstalledModuleEnv,
  extendInstalledModuleEnv,
  lookupInstalledModuleEnv,
  moduleName,
  )
import GHC.Utils.Panic (panic)
import Internal.State.Stats (FinderStats (..))

#else

import GHC.Unit.Finder (initFinderCache)

#endif

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

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

data FinderState =
  FinderState {
     modules :: IORef (InstalledModuleEnv InstalledFindResult),
     files :: IORef (Map String Fingerprint)
  }

emptyFinderState :: MonadIO m => m FinderState
emptyFinderState =
  liftIO do
    modules <- newIORef emptyInstalledModuleEnv
    files <- newIORef LazyMap.empty
    pure FinderState {modules, files}

#else

data FinderState =
  FinderState {
    cache :: FinderCache
  }

emptyFinderState :: MonadIO m => m FinderState
emptyFinderState =
  liftIO do
    cache <- initFinderCache
    pure FinderState {cache}

#endif

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
