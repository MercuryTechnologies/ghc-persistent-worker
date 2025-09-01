{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.State.Oneshot where

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import GHC.Driver.Env (HscEnv (..))
import GHC.Linker.Types (LinkerEnv (..), Loader (..), LoaderState (..))
import GHC.Runtime.Interpreter (Interp (..))
import GHC.Types.Name.Cache (NameCache (..), OrigNameCache)
import GHC.Types.Unique.DFM (plusUDFM)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.External (ExternalUnitCache (..), initExternalUnitCache)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Unit.Finder.Types (FinderCache (..))
import GHC.Unit.Module.Env (InstalledModuleEnv, emptyModuleEnv, plusModuleEnv)
import Internal.State.Stats (
  CacheStats (..),
  LinkerStats (..),
  LoaderStats (..),
  StatsUpdate (..),
  SymbolsStats (..),
  basicLinkerStats,
  basicLoaderStats,
  basicSymbolsStats,
  emptyLinkerStats,
  emptyStats,
  )
import Types.State (SymbolCache (..), SymbolMap)
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

#if !defined(MWB)

import Control.Concurrent.MVar (newMVar)

#endif

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

finderEnv :: FinderState -> IO (InstalledModuleEnv InstalledFindResult)
finderEnv FinderState {modules} =
  readIORef modules

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

finderEnv :: FinderState -> IO (InstalledModuleEnv InstalledFindResult)
finderEnv FinderState {cache = FinderCache {fcModuleCache}} =
  readIORef fcModuleCache

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

restoreLinkerEnv :: LinkerEnv -> LinkerEnv -> (LinkerEnv, LinkerStats)
restoreLinkerEnv cached session =
  (merged, basicLinkerStats session cached)
  where
    merged =
      LinkerEnv {
        -- UniqFM, <> right-biased
        closure_env =
          cached.closure_env
          <>
          session.closure_env,
        -- UniqFM, <> right-biased
        itbl_env = cached.itbl_env <> session.itbl_env,
        -- UniqFM, <> right-biased
        addr_env = cached.addr_env <> session.addr_env
      }

restoreLoaderState ::
  LoaderState ->
  LoaderState ->
  IO (LoaderState, Maybe LoaderStats)
restoreLoaderState cached session =
  pure (merged, Just (basicLoaderStats session cached linkerStats))
  where
    merged =
      LoaderState {
        linker_env,
        -- ModuleEnv, left-biased
        bcos_loaded = plusModuleEnv session.bcos_loaded cached.bcos_loaded,
        -- ModuleEnv, left-biased
        objs_loaded = plusModuleEnv session.objs_loaded cached.objs_loaded,
        -- UniqDFM, depends on the elements in the maps
        pkgs_loaded = plusUDFM cached.pkgs_loaded session.pkgs_loaded,
        temp_sos = session.temp_sos
      }

    (linker_env, linkerStats) = restoreLinkerEnv cached.linker_env session.linker_env

modifyStats :: Target -> (CacheStats -> CacheStats) -> OneshotState -> OneshotState
modifyStats target f cache =
  cache {stats = Map.alter (Just . f . fromMaybe emptyStats) target cache.stats}

pushStats :: Bool -> Target -> Maybe LoaderStats -> SymbolsStats -> OneshotState -> OneshotState
pushStats restoring target (Just new) symbols =
  modifyStats target add
  where
    add old | restoring = old {restore = old.restore {loaderStats = new, symbols}}
            | otherwise = old {update = old.update {loaderStats = new, symbols}}
pushStats _ _ _ _ =
  id

loadCache ::
  Target ->
  Maybe LoaderState ->
  SymbolCache ->
  OrigNameCache ->
  OneshotState ->
  IO (OrigNameCache, (SymbolCache, (Maybe LoaderState, OneshotState)))
loadCache target initialLoaderState initialSymbolCache initialNames cache
  | Just InterpCache {..} <- cache.interpCache
  = do
    (restoredLs, loaderStats) <- case initialLoaderState of
      Just sessionLs ->
        restoreLoaderState loaderState sessionLs
      Nothing ->
        pure (loaderState, Nothing)
    let
      newSymbols = initialSymbolCache <> symbols
      symbolsStats = basicSymbolsStats initialSymbolCache symbols
      newCache = pushStats True target loaderStats symbolsStats cache
      -- this overwrites entire modules, since OrigNameCache is a three-level map.
      -- eventually we'll want to merge properly.
      names = plusModuleEnv initialNames cache.names
    pure (names, (newSymbols, (Just restoredLs, newCache)))

  | otherwise
  = pure (initialNames, (initialSymbolCache, (initialLoaderState, cache)))

initState ::
  LoaderState ->
  SymbolCache ->
  OrigNameCache ->
  OneshotState ->
  IO OneshotState
initState loaderState symbols names OneshotState {names = _, ..} =
  pure OneshotState {interpCache = Just InterpCache {..}, ..}

updateLinkerEnv :: LinkerEnv -> LinkerEnv -> (LinkerEnv, LinkerStats)
updateLinkerEnv cached session =
  (merged, basicLinkerStats cached session)
  where
    merged =
      LinkerEnv {
        -- UniqFM, <> right-biased
        closure_env =
          cached.closure_env
          <>
          session.closure_env,
        -- UniqFM, <> right-biased
        itbl_env = cached.itbl_env <> session.itbl_env,
        -- UniqFM, <> right-biased
        addr_env = cached.addr_env <> session.addr_env
      }

updateLoaderState ::
  LoaderState ->
  LoaderState ->
  IO (LoaderState, Maybe LoaderStats)
updateLoaderState cached session = do
  pure (merged, Just stats {linker = linkerStats})
  where
    merged =
      LoaderState {
        linker_env,
        -- ModuleEnv, left-biased
        bcos_loaded = plusModuleEnv session.bcos_loaded cached.bcos_loaded,
        -- ModuleEnv, left-biased
        objs_loaded = plusModuleEnv session.objs_loaded cached.objs_loaded,
        -- UniqDFM, depends on the elements in the maps
        pkgs_loaded = plusUDFM cached.pkgs_loaded session.pkgs_loaded,
        temp_sos = session.temp_sos
      }

    (linker_env, linkerStats) = updateLinkerEnv cached.linker_env session.linker_env

    stats = basicLoaderStats cached session emptyLinkerStats

updateState ::
  Target ->
  InterpCache ->
  LoaderState ->
  SymbolCache ->
  OrigNameCache ->
  OneshotState ->
  IO OneshotState
updateState target InterpCache {..} newLoaderState newSymbols newNames cache = do
  (updatedLs, stats) <- updateLoaderState loaderState newLoaderState
  pure $ pushStats False target stats symbolsStats cache {
    interpCache = Just InterpCache {
      loaderState = updatedLs,
      symbols = symbols <> newSymbols
    },
    -- for now: when a module is compiled, its names are definitely complete, so when a downstream module uses it as a
    -- dep, we don't want to overwrite the previous entry.
    -- but when we recompile parts of the tree this is different, so wel'll want to merge properly.
    names = plusModuleEnv newNames cache.names
  }
  where
    symbolsStats = basicSymbolsStats symbols newSymbols

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

-- | This replacement of the Finder implementation has the sole purpose of recording some cache stats, for now.
-- While its mutable state is allocated separately and shared across sessions, this doesn't really make a difference at
-- the moment since we're also initializing each compilation session with a shared @HscEnv@.
-- Ultimately this might be used to exert some more control over what modules GHC is allowed to access by using Buck's
-- deps, or some additional optimization.
newFinderCache ::
  ((OneshotState -> OneshotState) -> IO ()) ->
  OneshotState ->
  Target ->
  IO FinderCache
newFinderCache updateOneshot OneshotState {finder = FinderState {modules, files}} target = do
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
        case result of
          Just _ -> cacheHit key
          Nothing -> cacheMiss key
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
    cacheHit m =
      updateStats \ FinderStats {hits, ..} -> FinderStats {hits = incStat m hits, ..}

    cacheMiss m =
      updateStats \ FinderStats {misses, ..} -> FinderStats {misses = incStat m misses, ..}

    incStat m = Map.alter (Just . succ . fromMaybe 0) (moduleName m)

    updateStats f =
      updateOneshot $ modifyStats target \ CacheStats {..} -> CacheStats {finder = f finder, ..}

#else

newFinderCache ::
  a ->
  OneshotState ->
  Target ->
  IO FinderCache
newFinderCache _ OneshotState {finder = FinderState {cache}} _ = pure cache

#endif

withHscState :: HscEnv -> (MVar OrigNameCache -> MVar (Maybe LoaderState) -> MVar SymbolMap -> IO a) -> IO (Maybe a)
withHscState HscEnv {hsc_interp, hsc_NC = NameCache {nsNames}} use =
#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) || defined(MWB)
  for hsc_interp \ Interp {interpLoader = Loader {loader_state}, interpLookupSymbolCache} ->
    liftIO $ use nsNames loader_state interpLookupSymbolCache
#else
  for hsc_interp \ Interp {interpLoader = Loader {loader_state}} ->
    liftIO do
    symbolCacheVar <- newMVar mempty
    use nsNames loader_state symbolCacheVar
#endif

-- | Restore cache parts that depend on the 'Target'.
setTarget ::
  ((OneshotState -> OneshotState) -> IO ()) ->
  OneshotState ->
  Target ->
  HscEnv ->
  IO HscEnv
setTarget update cache target hsc_env = do
  -- The Finder cache is already shared by the base session, but with this, we can additionally inject the target file
  -- for stats collection.
  -- Only has an effect if the patch that abstracts the 'FinderCache' interface is in GHC, so >= 9.12.
  if cache.features.finder
  then restoreFinderCache hsc_env
  else pure hsc_env
  where
    restoreFinderCache e = do
      hsc_FC <- newFinderCache update cache target
      pure e {hsc_FC}

-- | Restore cache parts related to oneshot mode.
restoreOneshotCache :: OneshotState -> HscEnv -> IO HscEnv
restoreOneshotCache cache hsc_env = do
  -- If the feature is enabled, restore the EPS.
  -- This is only relevant for the oneshot worker, but even there the base session should already be sharing the EPS
  -- across modules.
  -- Might be removed soon.
  pure
    if cache.features.eps
    then restoreCachedEps hsc_env
    else hsc_env
  where
    restoreCachedEps e = e {hsc_unit_env = e.hsc_unit_env {ue_eps = cache.eps}}

loadState ::
  ((OneshotState -> OneshotState) -> IO ()) ->
  Target ->
  HscEnv ->
  OneshotState ->
  IO (OneshotState, (HscEnv, Bool))
loadState update target hsc_env0 state0 = do
  result <-
    if state0.features.enable
    then do
      hsc_env1 <- restoreOneshotCache state0 =<< setTarget update state0 target hsc_env0
      if state0.features.loader
      then do
        withHscState hsc_env1 \ nsNames loaderStateVar symbolCacheVar -> do
          cache1 <- modifyMVar loaderStateVar \ initialLoaderState ->
            modifyMVar symbolCacheVar \ initialSymbolCache ->
              (first coerce) <$>
              modifyMVar nsNames \ names ->
                loadCache target initialLoaderState (SymbolCache initialSymbolCache) names state0
          pure (hsc_env1, cache1)
      else pure (Just (hsc_env1, state0))
    else pure Nothing
  let (hsc_env1, cache1) = fromMaybe (hsc_env0, state0 {features = state0.features {loader = False}}) result
  pure (cache1, (hsc_env1, cache1.features.enable))

storeState ::
  HscEnv ->
  Target ->
  OneshotState ->
  IO OneshotState
storeState hsc_env target state = do
  if state.features.enable
  then do
    if state.features.loader
    then do
      fromMaybe state . join <$> withHscState hsc_env \ nsNames loaderStateVar symbolCacheVar ->
        readMVar loaderStateVar >>= traverse \ newLoaderState -> do
          newSymbols <- readMVar symbolCacheVar
          newNames <- readMVar nsNames
          let store = maybe initState (updateState target) state.interpCache
          store newLoaderState (SymbolCache newSymbols) newNames state
    else pure state
  else pure state
