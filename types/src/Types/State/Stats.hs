module Types.State.Stats where

import Data.Map.Strict (Map)
import GHC (ModuleName)

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

data StatsUpdate =
  StatsUpdate {
    loaderStats :: LoaderStats,
    symbols :: SymbolsStats
  }
  deriving stock (Eq, Show)

emptyStatsUpdate :: StatsUpdate
emptyStatsUpdate =
  StatsUpdate {
    loaderStats = emptyLoaderStats,
    symbols = SymbolsStats {new = 0}
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
