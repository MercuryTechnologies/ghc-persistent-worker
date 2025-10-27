{-# LANGUAGE CPP #-}

module Types.State.Finder where

import Control.Monad.IO.Class (MonadIO, liftIO)

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

import Data.IORef (IORef, newIORef)
import qualified Data.Map.Lazy as LazyMap
import Data.Map.Strict (Map)
import GHC.Fingerprint (Fingerprint)
import GHC.Unit (InstalledModuleEnv, emptyInstalledModuleEnv)
import GHC.Unit.Finder (InstalledFindResult)

#else

import GHC.Unit.Finder (FinderCache, initFinderCache)

#endif

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
