{-# LANGUAGE CPP #-}

-- | Description: An optimized implementation of 'UnitIndex', which abstracts a few operations performed on the unit
-- state by GHC.
-- This feature was created by us and is only available in MWB branches of GHC.
--
-- The custom implementation avoids extreme memory consumption caused by redundancies introduced by a high number of
-- home units:
--
-- - When each unit state is initialized, all package DBs for its dependencies are read from disk.
--   Since many of the units may depend on the same packages, caching them by file path reduces memory usage
--   significantly.
-- - Each unit state is supplied with an index mapping module names to the units that provide them, for all
--   dependencies, called module name providers.
--   The map also records whether the module is visible.
--   This is either:
--
--   - A module-specific property, in which case it is governed by its package's configuration and therefore identical
--     for any home unit
--   - A package-specific property, in which case it is governed by a home unit's package flags.
--
--   In the former case, it is unnecessary to store this information in every unit state, so we've moved it to our unit
--   index.
--   In the latter case, it is unnecessary to store module-granular visibility information, so we've stored the unit
--   visibility maps in the unit index and modified the lookup algorithm to query the shared module map to obtain the
--   providers, and filter those based on home unit specific visibility.
module Internal.State.UnitIndex where

#if defined(UNIT_INDEX)

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!?))
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Traversable (mapAccumM)
import GHC hiding (SuccessFlag (..))
import GHC.Driver.Env (HscEnv (..))
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set (UniqSet)
import GHC.Unit.Env
import GHC.Unit.Module
import GHC.Unit.State
import qualified GHC.Unit.State as GHC
import Internal.State.UnitIndex.Update (Provider (..), Providers, unitOverrides, updateProviders)
import Prelude hiding ((<>))
import System.OsPath (OsPath)
import Types.State.Make (MakeState (..))

enableSharedProviders :: Bool
enableSharedProviders = True

enableSharedDatabases :: Bool
enableSharedDatabases = True

-- | The unit index state shared by all home units.
data UnitIndexBackend =
  UnitIndexBackend {
    databases :: !(Map OsPath (UnitDatabase UnitId)),
    units :: !(UniqSet UnitId),
    providers :: !Providers,
    visibilities :: !(UniqMap UnitId VisibilityMap),
    pluginVisibilities :: !(UniqMap UnitId VisibilityMap)
  }

newUnitIndexBackend :: UnitIndexBackend
newUnitIndexBackend =
  UnitIndexBackend {
    databases = mempty,
    units = mempty,
    providers = mempty,
    visibilities = mempty,
    pluginVisibilities = mempty
  }

-- TODO are reexported modules visible when the unit is hidden by a renaming flag?
-- | Construct a 'ModuleOrigin' for the 'Provider' of the given module, using both the global information in 'Provider'
-- and the home unit specific visibility information.
--
-- This attempts to resemble the logic in GHC, but it's hard to directly compare the two.
globalOrigin ::
  UnitState ->
  VisibilityMap ->
  Module ->
  Provider ->
  (Module, ModuleOrigin)
globalOrigin UnitState {wireMap} visibility Module {moduleUnit, moduleName} Provider {hidden, reexports} =
  (mkModule unit moduleName, origin)
  where
    origin
      | hidden
      = ModHidden
      | Just reex <- reexports
      = mkOrigin (uv_expose_all <$> vis) (NonEmpty.partition isVisible reex)
      | otherwise
      = mkOrigin (Just (maybe False uv_expose_all vis)) ([], [])

    vis = lookupUniqMap visibility unit

    isVisible info = elemUniqMap (mkUnit info) visibility

    mkOrigin fromOrigUnit (fromExposedReexport, fromHiddenReexport) =
      ModOrigin {fromPackageFlag = False, ..}

    unit = maybe moduleUnit (RealUnit . Definite) (lookupUniqMap wireMap (toUnitId moduleUnit))

-- | Look up a module in the unit specifc and global providers to determine its origins.
--
-- If the unit state contains entries (in @overrides@) for modules renamed by @-package@ flags, those are returned.
-- Otherwise, this queries the global providers and applies the home unit's visibility map.
queryFindOrigin ::
  UnitId ->
  UnitIndexBackend ->
  UnitState ->
  ModuleName ->
  Bool ->
  Maybe (UniqMap Module ModuleOrigin)
queryFindOrigin home UnitIndexBackend {providers, visibilities, pluginVisibilities} state name plugins =
  lookupUniqMap overrides name <> (applyVisibility <$> lookupUniqMap providers name)
  where
    applyVisibility =
      listToUniqMap . fmap (uncurry (globalOrigin state visibility)) . nonDetUniqMapToList

    visibility = fromMaybe mempty (lookupUniqMap vis home)

    (overrides, vis) =
      if plugins
      then (state.pluginModuleNameProvidersMap, visibilities)
      else (state.moduleNameProvidersMap, pluginVisibilities)

queryFindOriginDefault ::
  UnitIndexBackend ->
  UnitState ->
  ModuleName ->
  Bool ->
  Maybe (UniqMap Module ModuleOrigin)
queryFindOriginDefault _ UnitState {moduleNameProvidersMap, pluginModuleNameProvidersMap} name plugins =
  lookupUniqMap source name
  where
    source = if plugins then pluginModuleNameProvidersMap else moduleNameProvidersMap

-- | This currently doesn't use the global probiders to list all modules, therefore breaking import suggestions.
newUnitIndexQuery ::
  MonadIO m =>
  MVar UnitIndexBackend ->
  UnitId ->
  m UnitIndexQuery
newUnitIndexQuery ref unit = do
  state <- liftIO $ readMVar ref
  pure UnitIndexQuery {
    findOrigin = (if enableSharedProviders then queryFindOrigin unit else queryFindOriginDefault) state,
    moduleProviders = \ us -> us.moduleNameProvidersMap
  }

-- | Resolve all database files in the given unit config to 'UnitDatabase' values.
-- If a DB's path is present in the cache in 'UnitIndexBackend', use that, otherwise read it from disk.
readDatabasesShared ::
  MVar UnitIndexBackend ->
  GHC.Logger ->
  UnitId ->
  UnitConfig ->
  IO [UnitDatabase UnitId]
readDatabasesShared ref logger _ cfg =
  modifyMVar ref \ state -> do
    conf_refs <- getUnitDbRefs cfg
    confs <- catMaybes <$> mapM (resolveUnitDatabase cfg) conf_refs
    let
      loadDb z path =
        if enableSharedDatabases
        then case z !? path of
          Just db -> pure (z, db)
          Nothing -> do
            db <- readUnitDatabase logger cfg path
            pure (Map.insert path db z, db)
        else do
          db <- readUnitDatabase logger cfg path
          pure (z, db)
    (newDatabases, dbs) <- mapAccumM loadDb state.databases confs
    pure (state {databases = newDatabases}, dbs)

-- | Handler for 'computeProviders' that splits module name providers into a shared map of 'Provider' and more granular
-- unit-specific state.
-- The former and the unit visibility map is stored in the custom state, while the renaming-based overrides are stored
-- in the default unit state field.
--
-- Plugin dependencies are treated separately, like the original implementation.
computeProvidersShared ::
  MVar UnitIndexBackend ->
  Logger ->
  UnitId ->
  UnitConfig ->
  VisibilityMap ->
  VisibilityMap ->
  UnitInfoMap ->
  UnitInfoMap ->
  ModuleNameProvidersMap ->
  IO (ModuleNameProvidersMap, ModuleNameProvidersMap)
computeProvidersShared ref logger unit cfg vis_map plugin_vis_map initial_dbs _ unusable = do
  modifyMVar ref \ UnitIndexBackend {..} -> do
    (newProviders, newUnits) <- updateProviders logger cfg initial_dbs providers units
    let overrides = unitOverrides logger vis_map unusable newProviders
        pluginOverrides = unitOverrides logger plugin_vis_map unusable newProviders
    pure $! (UnitIndexBackend {
      providers = newProviders,
      visibilities = addToUniqMap visibilities unit vis_map,
      pluginVisibilities = addToUniqMap pluginVisibilities unit plugin_vis_map,
      units = newUnits,
      ..
    }, (overrides, pluginOverrides))

newUnitIndex :: IO UnitIndex
newUnitIndex = do
  ref <- newMVar newUnitIndexBackend
  basic <- GHC.newUnitIndex
  pure UnitIndex {
    unitIndexQuery = newUnitIndexQuery ref,
    readDatabases = readDatabasesShared ref,
    computeProviders = if enableSharedProviders then computeProvidersShared ref else basic.computeProviders
  }

restoreUnitIndex :: MakeState -> HscEnv -> HscEnv
restoreUnitIndex state hsc_env =
  hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_index = state.unitIndex}}

#else

import GHC (HscEnv)
import Types.State.Make (MakeState, UnitIndex (..))

newUnitIndex :: IO UnitIndex
newUnitIndex = pure UnitIndex

restoreUnitIndex :: MakeState -> HscEnv -> HscEnv
restoreUnitIndex _ = id

#endif
