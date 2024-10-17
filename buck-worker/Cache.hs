module Cache where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (unless)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_, traverse_)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import GHC (Ghc, moduleName, moduleNameString)
import GHC.Data.FastString (FastString)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad (withSession)
import GHC.Linker.Types (LinkerEnv (..), Loader (..), LoaderState (..))
import GHC.Ptr (Ptr)
import GHC.Runtime.Interpreter (Interp (..))
import GHC.Types.Name.Cache (NameCache (..), OrigNameCache)
import GHC.Types.Unique.DFM (plusUDFM)
import GHC.Types.Unique.FM (UniqFM, minusUFM, nonDetEltsUFM, sizeUFM)
import GHC.Types.Unique.Supply (initUniqSupply)
import GHC.Unit.Module.Env (emptyModuleEnv, moduleEnvKeys, plusModuleEnv)
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Outputable (comma, fsep, hang, punctuate, text, ($$), (<+>))
import Log (Log, logOther, logd)
import System.Environment (lookupEnv)

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
    loader :: LoaderStats,
    symbols :: SymbolsStats,
    names :: NamesStats
  }
  deriving stock (Eq, Show)

emptyStatsUpdate :: StatsUpdate
emptyStatsUpdate =
  StatsUpdate {
    loader = emptyLoaderStats,
    symbols = SymbolsStats {new = 0},
    names = NamesStats {new = 0}
  }

data CacheStats =
  CacheStats {
    restore :: StatsUpdate,
    update :: StatsUpdate
  }
  deriving stock (Eq, Show)

emptyStats :: CacheStats
emptyStats =
  CacheStats {
    restore = emptyStatsUpdate,
    update = emptyStatsUpdate
  }

data InterpCache =
  InterpCache {
    loaderState :: LoaderState
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

-- TODO the name cache could in principle be shared directly – try it out
data Cache =
  Cache {
    enable :: Bool,
    initialized :: Bool,
    interp :: Maybe InterpCache,
    names :: OrigNameCache,
    stats :: Map Target CacheStats,
    path :: BinPath
  }

emptyCache :: Bool -> IO (MVar Cache)
emptyCache enable = do
  initialPath <- lookupEnv "PATH"
  newMVar Cache {
    enable,
    initialized = False,
    interp = Nothing,
    names = emptyModuleEnv,
    stats = mempty,
    path = BinPath {
      initial = initialPath,
      extra = mempty
    }
  }

initialize ::
  MVar Cache ->
  IO ()
initialize cache =
  modifyMVar_ cache \ c -> do
    unless c.initialized do
      initUniqSupply 0 1
    pure c {initialized = True}

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

pushStats :: Bool -> Target -> Maybe LoaderStats -> SymbolsStats -> NamesStats -> Cache -> Cache
pushStats restoring target (Just new) symbols names cache =
  cache {stats = Map.alter f target cache.stats}
  where
    f old = Just (add (fromMaybe emptyStats old))
    add old | restoring = old {restore = old.restore {loader = new, symbols, names}}
            | otherwise = old {update = old.update {loader = new, symbols, names}}
pushStats _ _ _ _ _ cache =
  cache

restoreCache ::
  Target ->
  Maybe LoaderState ->
  OrigNameCache ->
  Cache ->
  IO (Cache, (OrigNameCache, Maybe LoaderState))
restoreCache target initialLoaderState initialNames cache
  | Just InterpCache {..} <- cache.interp
  = do
    (restoredLs, loaderStats) <- case initialLoaderState of
      Just sessionLs ->
        restoreLoaderState loaderState sessionLs
      Nothing ->
        pure (loaderState, Nothing)
    let
      symbolsStats = basicSymbolsStats mempty mempty
      namesStats = basicNamesStats initialNames cache.names
      newCache = pushStats True target loaderStats symbolsStats namesStats cache
      -- this overwrites entire modules, since OrigNameCache is a three-level map.
      -- eventually we'll want to merge properly.
      names = plusModuleEnv initialNames cache.names
    pure (newCache, (names, Just restoredLs))

  | otherwise
  = pure (cache, (initialNames, initialLoaderState))

-- TODO filter all cached items to include only external Names if possible
initCache ::
  LoaderState ->
  OrigNameCache ->
  Cache ->
  IO Cache
initCache loaderState names Cache {names = _, ..} =
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
  OrigNameCache ->
  Cache ->
  IO Cache
updateCache target InterpCache {..} newLoaderState newNames cache = do
  (updatedLs, stats) <- updateLoaderState loaderState newLoaderState
  pure $ pushStats False target stats symbolsStats namesStats cache {
    interp = Just InterpCache {
      loaderState = updatedLs
    },
    -- for now: when a module is compiled, its names are definitely complete, so when a downstream module uses it as a
    -- dep, we don't want to overwrite the previous entry.
    -- but when we recompile parts of the tree this is different, so wel'll want to merge properly.
    names = plusModuleEnv newNames cache.names
  }
  where
    symbolsStats = SymbolsStats 0
    namesStats = basicNamesStats cache.names newNames

report ::
  MonadIO m =>
  MVar Log ->
  MVar Cache ->
  Target ->
  m ()
report logVar cacheVar target =
  liftIO (readMVar cacheVar) >>= \ Cache {stats} ->
    for_ (Map.lookup target stats) \ CacheStats {restore, update} -> do
      let
        restoreStats =
          text (show (length restore.loader.newBcos)) <+> text "BCOs" $$
          text (show restore.loader.linker.newClosures) <+> text "closures" $$
          text (show restore.symbols.new) <+> text "symbols" $$
          text (show restore.loader.sameBcos) <+> text "BCOs already in cache"

        newBcos = text <$> update.loader.newBcos

        updateStats =
          (if null newBcos then text "No new BCOs" else text "New BCOs:" <+> fsep (punctuate comma newBcos)) $$
          text (show update.loader.linker.newClosures) <+> text "new closures" $$
          text (show update.symbols.new) <+> text "new symbols" $$
          text (show update.loader.sameBcos) <+> text "BCOs already in cache"

      logd logVar $ hang (text target.get Outputable.<> text ":") 2 $
        hang (text "Restore:") 2 restoreStats $$
        hang (text "Update:") 2 updateStats

withHscState :: (MVar OrigNameCache -> MVar (Maybe LoaderState) -> IO ()) -> Ghc ()
withHscState use =
  withSession \ HscEnv {hsc_interp, hsc_NC = NameCache {nsNames}} ->
    for_ hsc_interp \ Interp {interpLoader = Loader {loader_state}} ->
      liftIO $ use nsNames loader_state

withCache :: MVar Log -> MVar Cache -> [String] -> Ghc a -> Ghc a
withCache logVar cacheVar [src] prog = do
  liftIO (initialize cacheVar)
  liftIO (readMVar cacheVar) >>= \case
    Cache {enable = True} -> do
      prepare
      finally (prog <* finalize) (report logVar cacheVar target)
    _ -> prog
  where
    prepare =
      withHscState \ nsNames loaderStateVar ->
        modifyMVar_ loaderStateVar \ initialLoaderState ->
          modifyMVar nsNames \ names ->
            modifyMVar cacheVar (restoreCache target initialLoaderState names)

    finalize =
      withHscState \ nsNames loaderStateVar ->
        readMVar loaderStateVar >>= traverse_ \ newLoaderState -> do
          newNames <- readMVar nsNames
          modifyMVar_ cacheVar \ c ->
            maybe initCache (updateCache target) c.interp newLoaderState newNames c

    target = Target src

withCache logVar _ _ prog = do
  liftIO $ logOther logVar "withCache called with target count /= 1"
  prog
