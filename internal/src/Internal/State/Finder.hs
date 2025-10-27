{-# LANGUAGE CPP #-}

module Internal.State.Finder where

import Data.IORef (readIORef)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Unit.Finder.Types (FinderCache (..))
import GHC.Unit.Module.Env (InstalledModuleEnv, installedModuleEnvElts)
import GHC.Utils.Outputable (SDoc, ppr, text, vcat, (<+>))
import Types.State.Finder (FinderState (..))

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

import qualified Data.Map.Lazy as LazyMap
import Data.Maybe (isJust)
import GHC.Fingerprint (Fingerprint, getFileHash)
import GHC.IORef (atomicModifyIORef')
import GHC.Unit (InstalledModule, extendInstalledModuleEnv, lookupInstalledModuleEnv)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Utils.Panic (panic)

#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

-- | This replacement of the Finder implementation has the sole purpose of recording some cache stats, for now.
-- While its mutable state is allocated separately and shared across sessions, this doesn't really make a difference at
-- the moment since we're also initializing each compilation session with a shared @HscEnv@.
-- Ultimately this might be used to exert some more control over what modules GHC is allowed to access by using Buck's
-- deps, or some additional optimization.
newFinderCache ::
  -- | This function is called whenever a module was queried.
  -- The 'Bool' indicates whether it was found.
  (InstalledModule -> Bool -> IO ()) ->
  FinderState ->
  IO FinderCache
newFinderCache cacheHook FinderState {modules, files} = do
  let flushFinderCaches :: UnitEnv -> IO ()
      flushFinderCaches _ = panic "GHC attempted to flush finder caches, which shouldn't happen in worker mode"

      addToFinderCache :: InstalledModule -> InstalledFindResult -> IO ()
      addToFinderCache key val = do
        atomicModifyIORef' modules $ \c ->
          case (lookupInstalledModuleEnv c key, val) of
            (Just InstalledFound{}, InstalledNotFound{}) -> (c, ())
            _ -> (extendInstalledModuleEnv c key val, ())

      lookupFinderCache :: InstalledModule -> IO (Maybe InstalledFindResult)
      lookupFinderCache key = do
        c <- readIORef modules
        let result = lookupInstalledModuleEnv c key
        cacheHook key (isJust result)
        pure $! result

      lookupFileCache :: FilePath -> IO Fingerprint
      lookupFileCache key = do
         fc <- readIORef files
         case LazyMap.lookup key fc of
           Nothing -> do
             hash <- getFileHash key
             atomicModifyIORef' files $ \c -> (LazyMap.insert key hash c, ())
             return hash
           Just fp -> return fp
  return FinderCache {..}
  where

finderEnv :: FinderState -> IO (InstalledModuleEnv InstalledFindResult)
finderEnv FinderState {modules} =
  readIORef modules

#else

newFinderCache ::
  a ->
  FinderState ->
  IO FinderCache
newFinderCache _ FinderState {cache} = pure cache

finderEnv :: FinderState -> IO (InstalledModuleEnv InstalledFindResult)
finderEnv FinderState {cache = FinderCache {fcModuleCache}} =
  readIORef fcModuleCache

#endif

pprInstalledFindResult :: InstalledFindResult -> SDoc
pprInstalledFindResult = \case
  InstalledFound {} -> text "found"
  InstalledNoPackage _ -> text "no package"
  InstalledNotFound _ _ -> text "not found"

showFinder :: FinderState -> IO SDoc
showFinder state = do
  modules <- finderEnv state
  pure $ vcat [(ppr name) <+> text "->" <+> (pprInstalledFindResult result) | (name, result) <- installedModuleEnvElts modules]
