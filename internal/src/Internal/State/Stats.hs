module Internal.State.Stats where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!?))
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Set ((\\))
import GHC (ModuleName, moduleName, moduleNameString)
import GHC.Linker.Types (LinkerEnv (..), LoaderState (..))
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Types.Unique.FM (minusUFM, nonDetEltsUFM, sizeUFM)
import GHC.Unit.Module.Env (moduleEnvKeys)
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Outputable (SDoc, comma, doublePrec, fsep, hang, nest, punctuate, text, vcat, ($$), (<+>))
import Internal.Log (logDebugD, logd)
import Types.Args (TargetId (..))
import Types.Log (Log)
import Types.State.Oneshot (SymbolCache (..))
import Types.State.Stats (
  CacheStats (..),
  FinderStats (..),
  LinkerStats (..),
  LoaderStats (..),
  StatsUpdate (..),
  SymbolsStats (..),
  )
import Types.Target (Target (..))

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
    new = sizeUFM (minusUFM update.symbols base.symbols)
  }

----------------------------------------------------------------------------------------------------

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
reportMessages ::
  Target ->
  Maybe (Map Target CacheStats) ->
  Double ->
  SDoc
reportMessages target mb_stats memory =
  statsPart $$
  memoryPart
  where
    statsPart = case mb_stats of
      Just stats -> maybe (text "Cache unused for this module.") statsMessages (stats !? target)
      Nothing -> text "Cache disabled."

    memoryPart = text "Memory:" <+> doublePrec 2 memory <+> text "MB"

-- | Log a report for a completed compilation, using 'reportMessages' to assemble the content.
report ::
  MonadIO m =>
  MVar Log ->
  -- | A description of the current worker process.
  Maybe TargetId ->
  Target ->
  Maybe (Map Target CacheStats) ->
  m ()
report logVar workerId target stats = do
  s <- liftIO getRTSStats
  let memory = fromIntegral (s.gc.gcdetails_mem_in_use_bytes) / 1000000
  logd logVar (hang header 2 (reportMessages target stats memory))
  where
    header = text target.path Outputable.<> maybe (text "") workerDesc workerId Outputable.<> text ":"

    workerDesc wid = text (" (" ++ wid.string ++ ")")

logMemStats :: String -> MVar Log -> IO ()
logMemStats step logVar = do
  s <- liftIO getRTSStats
  let logMem desc value = logDebugD logVar (text (desc ++ ":") <+> doublePrec 2 (fromIntegral value / 1_000_000) <+> text "MB")
  logDebugD logVar (text ("-------------- " ++ step))
  logMem "Mem in use" s.gc.gcdetails_mem_in_use_bytes
  logMem "Max mem in use" s.max_mem_in_use_bytes
  logMem "Max live bytes" s.max_live_bytes
