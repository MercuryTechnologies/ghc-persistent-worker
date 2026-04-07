-- | Abstraction over cache operations used by the build pipeline.
--
-- Groups the cache read operations that the dispatch and propagation handlers need,
-- separating them from the concrete filesystem implementation in 'GhcServer.Cache'.
module GhcServer.Data.BuildCache where

import Data.Set (Set)
import GHC (ModuleName)
import GhcServer.Data.Unit (UnitName)
import Types.CachedDeps (CachedUnit)

-- | Cache query interface used by the build handlers.
--
-- All operations are read-only — cache writes are handled by the metadata
-- and compilation steps directly.
data BuildCache =
  BuildCache {
    -- | Check whether a unit has cached artifacts from a prior build.
    unitCached :: UnitName -> IO Bool,
    -- | Load a unit's 'CachedUnit' from @cached_unit.json@, if it exists.
    loadUnit :: UnitName -> IO (Either String (Maybe CachedUnit)),
    -- | Check whether an interface for a module has been compiled previously.
    interfaceExists :: UnitName -> ModuleName -> IO Bool,
    -- | Compute the set of all units with cache from a prior build.
    cachedUnits :: IO (Set UnitName)
  }
