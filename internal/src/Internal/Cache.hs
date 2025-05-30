{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.Cache where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.IORef (readIORef)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!?))
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Traversable (for)
import GHC (Ghc, ModIface, ModuleName, mi_module, moduleName, moduleNameString, setSession)
import GHC.Data.FastString (FastString)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad (withSession)
import GHC.Linker.Types (Linkable, LinkerEnv (..), Loader (..), LoaderState (..))
import GHC.Ptr (Ptr)
import GHC.Runtime.Interpreter (Interp (..))
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Types.Name.Cache (NameCache (..), OrigNameCache)
import GHC.Types.Unique.DFM (plusUDFM)
import GHC.Types.Unique.FM (UniqFM, minusUFM, nonDetEltsUFM, sizeUFM)
import GHC.Unit.Env (
  HomeUnitEnv (..),
  HomeUnitGraph,
  UnitEnv (..),
  unitEnv_insert,
  unitEnv_lookup,
  unitEnv_singleton,
  unitEnv_union,
  )
import GHC.Unit.External (ExternalUnitCache (..), initExternalUnitCache)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Unit.Finder.Types (FinderCache (..))
import GHC.Unit.Module.Env (InstalledModuleEnv, emptyModuleEnv, moduleEnvKeys, plusModuleEnv)
import GHC.Unit.Module.Graph (ModuleGraph)
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Outputable (SDoc, comma, doublePrec, fsep, hang, nest, punctuate, text, vcat, ($$), (<+>))
import Internal.Log (Log, logd)
import System.Environment (lookupEnv)
import Types.Args (TargetId (..))

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

import Data.IORef (IORef, newIORef)
import qualified Data.Map.Lazy as LazyMap
import GHC.Fingerprint (Fingerprint, getFileHash)
import GHC.IORef (atomicModifyIORef')
import GHC.Unit (InstalledModule, emptyInstalledModuleEnv, extendInstalledModuleEnv, lookupInstalledModuleEnv)
import GHC.Unit.Finder (FinderCache (..), InstalledFindResult (..))
import GHC.Utils.Panic (panic)

#else

import GHC.Unit.Finder (initFinderCache)

#endif

#if defined(MWB)

import GHC.Unit.Module.Graph (ModuleGraphNode (..), mgModSummaries', mkModuleGraph, mkNodeKey)

#else

import GHC.Unit.Module.Graph (unionMG)

#endif

data ModuleArtifacts =
  ModuleArtifacts {
    iface :: ModIface,
    bytecode :: Maybe Linkable
  }

instance Show ModuleArtifacts where
  show ModuleArtifacts {iface} =
    "ModuleArtifacts { iface = " ++ moduleNameString (moduleName (mi_module iface)) ++ " }"

type SymbolMap = UniqFM FastString (Ptr ())

newtype SymbolCache =
  SymbolCache { get :: SymbolMap }
  deriving newtype (Semigroup, Monoid)

data LinkerStats =
  LinkerStats {
    newClosures :: Int,
    newItables :: Int
  }
  deriving stock (Eq, Show)

emptyLinkerStats :: LinkerStats
emptyLinkerStats =
  LinkerStats {
    newClosures = 0,
    newItables = 0
  }

data LoaderStats =
  LoaderStats {
    newBcos :: [String],
    sameBcos :: Int,
    linker :: LinkerStats
  }
  deriving stock (Eq, Show)

emptyLoaderStats :: LoaderStats
emptyLoaderStats =
  LoaderStats {
    newBcos = mempty,
    sameBcos = 0,
    linker = emptyLinkerStats
  }

data SymbolsStats =
  SymbolsStats {
    new :: Int
  }
  deriving stock (Eq, Show)

data NamesStats =
  NamesStats {
    new :: Int
  }
  deriving stock (Eq, Show)

data StatsUpdate =
  StatsUpdate {
    loaderStats :: LoaderStats,
    symbols :: SymbolsStats,
    names :: NamesStats
  }
  deriving stock (Eq, Show)

emptyStatsUpdate :: StatsUpdate
emptyStatsUpdate =
  StatsUpdate {
    loaderStats = emptyLoaderStats,
    symbols = SymbolsStats {new = 0},
    names = NamesStats {new = 0}
  }

data FinderStats =
  FinderStats {
    hits :: Map ModuleName Int,
    misses :: Map ModuleName Int
  }
  deriving stock (Eq, Show)

emptyFinderStats :: FinderStats
emptyFinderStats =
  FinderStats {
    hits = mempty,
    misses = mempty
  }

data CacheStats =
  CacheStats {
    restore :: StatsUpdate,
    update :: StatsUpdate,
    finder :: FinderStats
  }
  deriving stock (Eq, Show)

emptyStats :: CacheStats
emptyStats =
  CacheStats {
    restore = emptyStatsUpdate,
    update = emptyStatsUpdate,
    finder = emptyFinderStats
  }

data InterpCache =
  InterpCache {
    loaderState :: LoaderState,
    symbols :: SymbolCache
  }

newtype Target =
  Target { get :: String }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

data BinPath =
  BinPath {
    initial :: Maybe String,
    extra :: Set String
  }
  deriving stock (Eq, Show)

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

data CacheFeatures =
  CacheFeatures {
    enable :: Bool,
    loader :: Bool,
    names :: Bool,
    finder :: Bool,
    eps :: Bool,
    hpt :: Bool
  }
  deriving stock (Eq, Show)

newCacheFeatures :: CacheFeatures
newCacheFeatures = CacheFeatures {enable = True, loader = True, names = True, finder = True, eps = True, hpt = False}

-- TODO the name cache could in principle be shared directly – try it out
data Cache =
  Cache {
    features :: CacheFeatures,
    interp :: Maybe InterpCache,
    names :: OrigNameCache,
    stats :: Map Target CacheStats,
    path :: BinPath,
    finder :: FinderState,
    eps :: ExternalUnitCache,
    hug :: Maybe HomeUnitGraph,
    moduleGraph :: Maybe ModuleGraph,
    baseSession :: Maybe HscEnv,
    options :: Options
  }

data Options =
  Options {
    extraGhcOptions :: String
  }

emptyCacheWith :: CacheFeatures -> IO (MVar Cache)
emptyCacheWith features = do
  initialPath <- lookupEnv "PATH"
  finder <- emptyFinderState
  eps <- initExternalUnitCache
  newMVar Cache {
    features,
    interp = Nothing,
    names = emptyModuleEnv,
    stats = mempty,
    path = BinPath {
      initial = initialPath,
      extra = mempty
    },
    finder,
    eps,
    hug = Nothing,
    moduleGraph = Nothing,
    baseSession = Nothing,
    options = defaultOptions
  }

emptyCache :: Bool -> IO (MVar Cache)
emptyCache enable = do
  emptyCacheWith newCacheFeatures {enable}

defaultOptions :: Options
defaultOptions =
  Options {
    extraGhcOptions = ""
  }

basicLinkerStats :: LinkerEnv -> LinkerEnv -> LinkerStats
basicLinkerStats base update =
  LinkerStats {
    newClosures = Set.size (updateClosures \\ baseClosures),
    newItables = Set.size (updateItables \\ baseItables)
  }
  where
    updateClosures = names update.closure_env
    baseClosures = names base.closure_env
    updateItables = names update.itbl_env
    baseItables = names base.itbl_env

    names = Set.fromList . fmap fst . nonDetEltsUFM

basicLoaderStats ::
  LoaderState ->
  LoaderState ->
  LinkerStats ->
  LoaderStats
basicLoaderStats base update linker =
  LoaderStats {
    newBcos = modStr <$> Set.toList (updateBcos \\ baseBcos),
    sameBcos = Set.size bcoSame,
    linker
  }
  where
    modStr = moduleNameString . moduleName
    bcoSame = Set.intersection updateBcos baseBcos
    updateBcos = Set.fromList (moduleEnvKeys update.bcos_loaded)
    baseBcos = Set.fromList (moduleEnvKeys base.bcos_loaded)

basicSymbolsStats :: SymbolCache -> SymbolCache -> SymbolsStats
basicSymbolsStats base update =
  SymbolsStats {
    new = sizeUFM (minusUFM update.get base.get)
  }

-- TODO complicated
basicNamesStats :: OrigNameCache -> OrigNameCache -> NamesStats
basicNamesStats _ _ =
  NamesStats {
    new = 0
  }
  where

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

modifyStats :: Target -> (CacheStats -> CacheStats) -> Cache -> Cache
modifyStats target f cache =
  cache {stats = Map.alter (Just . f . fromMaybe emptyStats) target cache.stats}

pushStats :: Bool -> Target -> Maybe LoaderStats -> SymbolsStats -> NamesStats -> Cache -> Cache
pushStats restoring target (Just new) symbols names =
  modifyStats target add
  where
    add old | restoring = old {restore = old.restore {loaderStats = new, symbols, names}}
            | otherwise = old {update = old.update {loaderStats = new, symbols, names}}
pushStats _ _ _ _ _ =
  id

restoreCache ::
  Target ->
  Maybe LoaderState ->
  SymbolCache ->
  OrigNameCache ->
  Cache ->
  IO (OrigNameCache, (SymbolCache, (Maybe LoaderState, Cache)))
restoreCache target initialLoaderState initialSymbolCache initialNames cache
  | Just InterpCache {..} <- cache.interp
  = do
    (restoredLs, loaderStats) <- case initialLoaderState of
      Just sessionLs ->
        restoreLoaderState loaderState sessionLs
      Nothing ->
        pure (loaderState, Nothing)
    let
      newSymbols = initialSymbolCache <> symbols
      symbolsStats = basicSymbolsStats initialSymbolCache symbols
      namesStats = basicNamesStats initialNames cache.names
      newCache = pushStats True target loaderStats symbolsStats namesStats cache
      -- this overwrites entire modules, since OrigNameCache is a three-level map.
      -- eventually we'll want to merge properly.
      names = plusModuleEnv initialNames cache.names
    pure (names, (newSymbols, (Just restoredLs, newCache)))

  | otherwise
  = pure (initialNames, (initialSymbolCache, (initialLoaderState, cache)))

-- TODO filter all cached items to include only external Names if possible
initCache ::
  LoaderState ->
  SymbolCache ->
  OrigNameCache ->
  Cache ->
  IO Cache
initCache loaderState symbols names Cache {names = _, ..} =
  pure Cache {interp = Just InterpCache {..}, ..}

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

updateCache ::
  Target ->
  InterpCache ->
  LoaderState ->
  SymbolCache ->
  OrigNameCache ->
  Cache ->
  IO Cache
updateCache target InterpCache {..} newLoaderState newSymbols newNames cache = do
  (updatedLs, stats) <- updateLoaderState loaderState newLoaderState
  pure $ pushStats False target stats symbolsStats namesStats cache {
    interp = Just InterpCache {
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
    namesStats = basicNamesStats cache.names newNames

moduleColumns :: Show a => Map ModuleName a -> SDoc
moduleColumns m =
  vcat [text n Outputable.<> text ":" $$ nest offset (text (show h)) | (n, h) <- kvs]
  where
    offset = length (fst (last kvs)) + 2
    kvs = sortBy (comparing (length . fst)) (first moduleNameString <$> Map.toList m)

-- | Assemble log messages about cache statistics.
statsMessages :: CacheStats -> SDoc
statsMessages CacheStats {restore, update, finder} =
  hang (text "Restore:") 2 restoreStats $$
  hang (text "Update:") 2 updateStats $$
  hang (text "Finder:") 2 finderStats
  where
      restoreStats =
        text (show (length restore.loaderStats.newBcos)) <+> text "BCOs" $$
        text (show restore.loaderStats.linker.newClosures) <+> text "closures" $$
        text (show restore.symbols.new) <+> text "symbols" $$
        text (show restore.loaderStats.sameBcos) <+> text "BCOs already in cache"

      newBcos = text <$> update.loaderStats.newBcos

      updateStats =
        (if null newBcos then text "No new BCOs" else text "New BCOs:" <+> fsep (punctuate comma newBcos)) $$
        text (show update.loaderStats.linker.newClosures) <+> text "new closures" $$
        text (show update.symbols.new) <+> text "new symbols" $$
        text (show update.loaderStats.sameBcos) <+> text "BCOs already in cache"

      finderStats =
        hang (text "Hits:") 2 (moduleColumns finder.hits) $$
        hang (text "Misses:") 2 (moduleColumns finder.misses)

-- | Assemble report messages, consisting of:
--
-- - Cache statistics, if the feature is enabled
-- - Current RTS memory usage
reportMessages :: Target -> Cache -> Double -> SDoc
reportMessages target Cache {stats, features} memory =
  statsPart $$
  memoryPart
  where
    statsPart =
      if features.enable
      then maybe (text "Cache unused for this module.") statsMessages (stats !? target)
      else text "Cache disabled."

    memoryPart = text "Memory:" <+> doublePrec 2 memory <+> text "MB"

-- | Log a report for a completed compilation, using 'reportMessages' to assemble the content.
report ::
  MonadIO m =>
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  Target ->
  Cache ->
  m ()
report logVar workerId target cache = do
  s <- liftIO getRTSStats
  let memory = fromIntegral (s.gc.gcdetails_mem_in_use_bytes) / 1000000
  logd logVar (hang header 2 (reportMessages target cache memory))
  where
    header = text target.get Outputable.<> maybe (text "") workerDesc workerId Outputable.<> text ":"

    workerDesc wid = text (" (" ++ wid.string ++ ")")

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

-- | This replacement of the Finder implementation has the sole purpose of recording some cache stats, for now.
-- While its mutable state is allocated separately and shared across sessions, this doesn't really make a difference at
-- the moment since we're also initializing each compilation session with a shared @HscEnv@.
-- Ultimately this might be used to exert some more control over what modules GHC is allowed to access by using Buck's
-- deps, or some additional optimization.
newFinderCache :: MVar Cache -> Cache -> Target -> IO FinderCache
newFinderCache cacheVar Cache {finder = FinderState {modules, files}} target = do
  let flushFinderCaches :: UnitEnv -> IO ()
      flushFinderCaches _ = panic "GHC attempted to flush finder caches, which shouldn't happen in worker mode"

      addToFinderCache :: InstalledModule -> InstalledFindResult -> IO ()
      addToFinderCache key val =
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
      modifyMVar_ cacheVar $ pure . modifyStats target \ CacheStats {..} -> CacheStats {finder = f finder, ..}

#else

newFinderCache :: MVar Cache -> Cache -> Target -> IO FinderCache
newFinderCache _ Cache {finder = FinderState {cache}} _ = pure cache

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

mergeHugs ::
  HomeUnitEnv ->
  HomeUnitEnv ->
  HomeUnitEnv
mergeHugs old new =
  new {homeUnitEnv_hpt = plusUDFM old.homeUnitEnv_hpt new.homeUnitEnv_hpt}

-- | Restore cache parts that depend on the 'Target'.
setTarget :: MVar Cache -> Cache -> Target -> HscEnv -> IO HscEnv
setTarget cacheVar cache target hsc_env = do
  -- The Finder cache is already shared by the base session, but with this, we can additionally inject the target file
  -- for stats collection.
  -- Only has an effect if the patch that abstracts the 'FinderCache' interface is in GHC, so >= 9.12.
  if cache.features.finder
  then restoreFinderCache hsc_env
  else pure hsc_env
  where
    restoreFinderCache e = do
      hsc_FC <- newFinderCache cacheVar cache target
      pure e {hsc_FC}

-- | Restore cache parts related to make mode.
restoreHptCache :: Cache -> HscEnv -> IO HscEnv
restoreHptCache cache hsc_env = do
  let
    -- If the cache contains a module graph stored by @compileModuleWithDepsInHpt@, restore it
    hsc_env1 = maybe id restoreHug cache.hug hsc_env

    -- If the cache contains a module graph stored by @computeMetadata@, restore it
    hsc_env2 = maybe id restoreModuleGraph cache.moduleGraph hsc_env1
  pure hsc_env2
  where
    restoreModuleGraph mg e = e {hsc_mod_graph = mg}

    restoreHug hug e = e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = hug}}

-- | Restore cache parts related to oneshot mode.
restoreOneshotCache :: Cache -> HscEnv -> IO HscEnv
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

-- | Merge the given module graph into the cached graph, or initialize it it doesn't exist yet.
-- This is used by the make mode worker after the metadata step has computed the module graph.
updateModuleGraph :: MVar Cache -> ModuleGraph -> IO ()
updateModuleGraph cacheVar new =
  modifyMVar_ cacheVar \ cache -> do
#if defined(MWB)
    pure cache {moduleGraph = Just (maybe new merge cache.moduleGraph)}
  where
    merge old =
      mkModuleGraph (Map.elems (Map.unionWith mergeNodes oldMap newMap))
      where
        mergeNodes = \cases
          (ModuleNode oldDeps _) (ModuleNode newDeps summ) -> ModuleNode (mergeDeps oldDeps newDeps) summ
          _ newNode -> newNode

        mergeDeps oldDeps newDeps = Set.toList (Set.fromList oldDeps <> Set.fromList newDeps)

        oldMap = Map.fromList $ [(mkNodeKey n, n) | n <- mgModSummaries' old]

        newMap = Map.fromList $ [(mkNodeKey n, n) | n <- mgModSummaries' new]
#else
    pure cache {moduleGraph = Just (maybe id unionMG cache.moduleGraph new)}
#endif

prepareCache :: MVar Cache -> Target -> HscEnv -> Cache -> IO (Cache, (HscEnv, Bool))
prepareCache cacheVar target hsc_env0 cache0 = do
  result <-
    if cache0.features.enable
    then do
      hsc_env1 <- restoreOneshotCache cache0 =<< restoreHptCache cache0 =<< setTarget cacheVar cache0 target hsc_env0
      if cache0.features.loader
      then do
        withHscState hsc_env1 \ nsNames loaderStateVar symbolCacheVar -> do
          cache1 <- modifyMVar loaderStateVar \ initialLoaderState ->
            modifyMVar symbolCacheVar \ initialSymbolCache ->
              (first coerce) <$>
              modifyMVar nsNames \ names ->
                restoreCache target initialLoaderState (SymbolCache initialSymbolCache) names cache0
          pure (hsc_env1, cache1)
      else pure (Just (hsc_env1, cache0))
    else pure Nothing
  let (hsc_env1, cache1) = fromMaybe (hsc_env0, cache0 {features = cache0.features {loader = False}}) result
  pure (cache1, (hsc_env1, cache1.features.enable))

storeIface :: HscEnv -> ModIface -> IO ()
storeIface _ _ =
  pure ()

storeHug :: HscEnv -> Cache -> IO Cache
storeHug hsc_env cache = do
  pure cache {hug = Just merged}
  where
    merged = maybe id (unitEnv_union mergeHugs) cache.hug hsc_env.hsc_unit_env.ue_home_unit_graph

-- | Extract the unit env of the currently active unit and store it in the cache.
-- This is used by the make mode worker after the metadata step has initialized the new unit.
insertUnitEnv :: HscEnv -> Cache -> Cache
insertUnitEnv hsc_env cache =
  cache {hug = Just (maybe fresh update cache.hug)}
  where
    fresh = unitEnv_singleton current ue
    ue = unitEnv_lookup current hsc_env.hsc_unit_env.ue_home_unit_graph
    current = hsc_env.hsc_unit_env.ue_current_unit
    update = unitEnv_insert current ue

finalizeCache ::
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  HscEnv ->
  Target ->
  Maybe ModuleArtifacts ->
  Cache ->
  IO Cache
finalizeCache logVar workerId hsc_env target artifacts cache0 = do
  cache1 <-
    if cache0.features.enable
    then do
      cache1 <-
        if cache0.features.loader
        then do
          fromMaybe cache0 . join <$> withHscState hsc_env \ nsNames loaderStateVar symbolCacheVar ->
            readMVar loaderStateVar >>= traverse \ newLoaderState -> do
              newSymbols <- readMVar symbolCacheVar
              newNames <- readMVar nsNames
              maybe initCache (updateCache target) cache0.interp newLoaderState (SymbolCache newSymbols) newNames cache0
        else pure cache0
      cache2 <-
        if cache0.features.hpt
        then do
          storeHug hsc_env cache1
        else pure cache1
      for_ artifacts \ ModuleArtifacts {iface} ->
        storeIface hsc_env iface
      pure cache2
    else pure cache0
  report logVar workerId target cache1
  pure cache1

withSessionM :: (HscEnv -> IO (HscEnv, a)) -> Ghc a
withSessionM use =
  withSession \ hsc_env -> do
    (new_env, a) <- liftIO $ use hsc_env
    setSession new_env
    pure a

withCache ::
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  MVar Cache ->
  Target ->
  Ghc (Maybe (Maybe ModuleArtifacts, a)) ->
  Ghc (Maybe (Maybe ModuleArtifacts, a))
withCache logVar workerId cacheVar target prog = do
  _ <- withSessionM \ hsc_env -> modifyMVar cacheVar (prepareCache cacheVar target hsc_env)
  result <- prog
  finalize (fst =<< result)
  pure result
  where
    finalize art =
      withSession \ hsc_env ->
        liftIO (modifyMVar_ cacheVar (finalizeCache logVar workerId hsc_env target art))
