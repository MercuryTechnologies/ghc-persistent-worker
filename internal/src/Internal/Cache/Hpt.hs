{-# LANGUAGE CPP, OverloadedLists, PatternSynonyms #-}

module Internal.Cache.Hpt where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), groupBy)
import Data.Map.Strict qualified as M (insert, lookup)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Time (getCurrentTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import GHC (DynFlags, GhcException (..), ModIface, ModIface_ (..), ModLocation (..), ModuleName, mkModule)
import GHC.Data.Bag (emptyBag)
import GHC.Data.Maybe (MaybeErr (..))
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hscSetActiveUnitId, hsc_HPT)
import GHC.Driver.Main (initModDetails)
import GHC.Driver.Session (targetProfile)
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Iface.Errors.Ppr (readInterfaceErrorDiagnostic)
import GHC.Iface.Errors.Types (ReadInterfaceError (..))
import GHC.Linker.Types (Linkable (..), LinkablePart (..))
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (mkOccEnv)
import GHC.Types.Name.Reader (GlobalRdrEltX (..), Parent (NoParent))
import GHC.Unit (Definite (..), GenUnit (..), UnitId)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..), homeModInfoByteCode)
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Module.Location (pattern ModLocation)
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Unit.Module.ModIface (IfaceTopEnv (..), set_mi_top_env)
import GHC.Unit.Module.WholeCoreBindings (WholeCoreBindings (..))
import GHC.Utils.Misc (modificationTimeIfExists)
import GHC.Utils.Outputable (ppr, ($+$))
import GHC.Utils.Panic (throwGhcExceptionIO, tryMost)
import Internal.Cache.Metadata (loadCachedUnit, loadCachedUnits, readParseGHCArgs)
import Internal.Compat.GHC914 (setExtraDecls)
import Internal.Log (logTimed)
import Prelude hiding (log)
import System.OsPath.Extra (OsPath, fromOsPath)
import Types.BuckArgs (IsInterpreted (Compiled, Interpreted), decodeJsonArg)
import Types.CachedDeps (CachedDep (..), CachedDeps (..), CachedUnit (..), JsonFs (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (make))
import Types.State.Make (bcoLoadState)

#if defined(MWB)

import GHC.Unit.Module.ModIface (mi_foreign)

#elif MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Types.Avail (sortAvails)
import GHC.Types.Name.Reader (globalRdrEnvElts, gresToAvailInfo)
import GHC.Unit.Module.ModIface (mi_sc_extra_decls, mi_sc_foreign)

#endif

#if defined(MWB)

import GHC.Driver.Main(compileWholeCoreBindings)

-- This is basically initWholeCoreBindings, but strict version and
-- does not add empty HMI to HPT.
loadWholeCoreBindings ::
  HscEnv ->
  ModIface ->
  ModDetails ->
  Linkable ->
  IO Linkable
loadWholeCoreBindings hsc_env _iface details (Linkable utc_time this_mod uls) =
  Linkable utc_time this_mod <$> mapM go uls
  where
    go = \case
      CoreBindings wcb -> do
        -- we only add byte code objects.
        (bco, _fos) <- compileWholeCoreBindings hsc_env type_env wcb
        pure (BCOs bco)
      l -> pure l
    type_env = md_types details

#else

import GHC.Driver.Main(initWholeCoreBindings)

loadWholeCoreBindings ::
  HscEnv ->
  ModIface ->
  ModDetails ->
  Linkable ->
  IO Linkable
loadWholeCoreBindings = initWholeCoreBindings

#endif

-- | Load bytecode from an interface.
-- Used only for modules missing from the current target's HPT when restoring the Buck cache after restarting a build.
--
-- The missing fields in @ModLocation@ aren't vital for the bytecode's purpose, but it wouldn't hurt to add them
-- eventually.
-- For example, the source file is used to add debug info and find foreign export stubs.
loadCachedByteCode :: HscEnv -> FilePath -> ModIface -> ModDetails -> IO (Maybe Linkable)
loadCachedByteCode hsc_env ifaceFile iface details =
  for core_bindings \ wcb -> do
    linkable <- bcoLinkable [CoreBindings wcb]
    loadWholeCoreBindings hsc_env iface details linkable
   where
    wcb_mod_location =
      ModLocation {
        ml_hs_file = Nothing,
        ml_hi_file = ifaceFile,
        ml_dyn_hi_file = ifaceFile,
        ml_obj_file = error "loadCachedByteCode",
        ml_dyn_obj_file = error "loadCachedByteCode",
        ml_hie_file = error "loadCachedByteCode"
      }

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

    core_bindings =
      mi_simplified_core iface <&> \ sc ->
        WholeCoreBindings {wcb_mod_location, wcb_bindings = mi_sc_extra_decls sc, wcb_foreign = mi_sc_foreign sc, wcb_module = mi_module iface, ..}

#elif defined(MWB)

    core_bindings =
      mi_extra_decls iface <&> \ wcb_bindings ->
        WholeCoreBindings {wcb_mod_location, wcb_foreign = mi_foreign iface, wcb_module = mi_module iface, ..}

#endif

    bcoLinkable parts = do
      if_time <- modificationTimeIfExists (ml_hi_file wcb_mod_location)
      time <- maybe getCurrentTime pure if_time
      return $! Linkable time (mi_module iface) parts

-- | If the given module name is missing from the HPT, load the given interface from disk and store it in the module's
-- 'HomeModInfo'.
--
-- This only happens when the module is depended upon downstream for the first time after restarting the worker with a
-- partial build.
--
-- Maybe this could reuse some stuff in @hscRecompStatus@?
loadCachedDep ::
  Logger ->
  IsInterpreted ->
  ModuleName ->
  (WorkerState, HscEnv) ->
  OsPath ->
  IO (WorkerState, HscEnv)
loadCachedDep log interp name (state0, hsc_env0) ifaceFile = do
  existing <- lookupHpt hpt name
  case existing of
    Just hmi ->
      case homeModInfoByteCode hmi of
        Just _ -> pure (state0, hsc_env0)
        Nothing -> loadHmiFromCached
    Nothing -> loadHmiFromCached

  where
    updateBcoState = do
      new_lock <- newEmptyMVar
      let make = state0.make
          m = make.bcoLoadState
          mlock = M.lookup name m
      case mlock of
        Nothing -> do
          let m' = M.insert name new_lock m
              make' = make {bcoLoadState = m'}
          pure (state0 {make = make'}, (new_lock, False))
        Just lock -> pure (state0, (lock, True))

    loadHmiFromCached = do
      (state1, (lock, load_already_requested)) <- updateBcoState
      if load_already_requested
        then readMVar lock >> pure (state1, hsc_env0)
        else loadHmi >> putMVar lock () >> pure (state1, hsc_env0)

    loadHmi = do
      logTimed log ("Loading HPT module from cache: " ++ fromOsPath ifaceFile) do
        hm_iface0 <- loadIface
        !hm_details <- initModDetails hsc_env0 hm_iface0
        homeMod_bytecode <- loadCachedByteCode hsc_env0 (fromOsPath ifaceFile) hm_iface0 hm_details
        let hm_iface = setExtraDecls Nothing hm_iface0
        let new = HomeModInfo {
          hm_iface,
          hm_linkable = HomeModLinkable {homeMod_object = Nothing, homeMod_bytecode},
          hm_details
        }
        addHomeModInfoToHpt new hpt
        pure hsc_env0

    -- @readIface@ needs the dflags only for platform/ways, so we don't need the unit dflags
    loadIface =
      ifaceResult =<< readIface' (hsc_dflags hsc_env0) (hsc_NC hsc_env0) (toModule name) (fromOsPath ifaceFile)

    -- NOTE: We use this custom version of readIface to ignore the hi way (i.e. CheckHiWay -> IgnoreHiWay)
    readIface' dflags name_cache wanted_mod file_path = do
      let profile = targetProfile dflags
      res <- tryMost $ readBinIface profile name_cache IgnoreHiWay QuietBinIFace file_path
      case res of
        Right iface
          -- NB: This check is NOT just a sanity check, it is
          -- critical for correctness of recompilation checking
          -- (it lets us tell when -this-unit-id has changed.)

          -- NOTE: mi_top_env is synthesized in order to make the symbols
          -- from the loaded interfaces be avaiable for the evaluated
          -- expression in the interpreter session.
          | wanted_mod == actual_mod && interp == Interpreted ->
              let es = mi_exports iface
                  convert (Avail n) = Just (nameOccName n, [GRE {gre_name = n, gre_par = NoParent, gre_lcl = True, gre_imp = emptyBag, gre_info = ()}])
                  convert (AvailTC _ _) = Nothing

                  exports = mkOccEnv (mapMaybe convert es)
                  imports = []
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
                  -- Unclear if this is equivalent.
                  rdrs = IfaceTopEnv (sortAvails (gresToAvailInfo (globalRdrEnvElts exports))) imports
#else
                  rdrs = Just (IfaceTopEnv exports imports)
#endif
               in return (Succeeded (set_mi_top_env rdrs iface))
          | wanted_mod == actual_mod && interp == Compiled -> return (Succeeded iface)
          | otherwise     -> return (Failed err)
          where
            actual_mod = mi_module iface
            err = HiModuleNameMismatchWarn file_path wanted_mod actual_mod
        Left exn -> return (Failed (ExceptionOccurred file_path exn))

    ifaceResult = \case
      Succeeded i ->
        pure i
      Failed err ->
        let msg = ppr name $+$ readInterfaceErrorDiagnostic err
        in throwGhcExceptionIO (PprProgramError "Loading cached interface failed" msg)

    toModule = mkModule (RealUnit (Definite uid))

    uid = hscActiveUnitId hsc_env0

    hpt = hsc_HPT hsc_env0

hasUnit :: UnitId -> HscEnv -> Bool
hasUnit uid hsc_env =
  isJust $ unitEnv_lookup_maybe uid hsc_env.hsc_unit_env.ue_home_unit_graph

-- | Load all dependencies of the current module from the Buck cache into the HPT if they don't exist.
--
-- When the make worker is killed by Buck at the end of a build, and the user subsequently changes some code and starts
-- a new build, the state (the current HPT) is initially empty, since Buck immediately tries to compile the changed
-- module, assuming its deps to be available to the compiler.
-- A JSON file provides 'CachedDeps' to the worker, containing all interface paths for the current home unit, which we
-- restore into the HPT here.
loadCachedDeps ::
  Logger ->
  IsInterpreted ->
  (WorkerState, HscEnv) ->
  CachedDeps ->
  IO (WorkerState, HscEnv)
loadCachedDeps log interp (state0, hsc_env0) (CachedDeps deps) =
  logTimed log "Loading cached deps" do
    (state1, hsc_env1) <- foldM loadDepUnit (state0, hsc_env0) byUnit
    pure (state1, hscSetActiveUnitId (hscActiveUnitId hsc_env0) hsc_env1)
  where
    -- If the unit isn't present in the unit env, it wasn't built by a worker, since it would have been loaded in the
    -- metadata restoration step.
    loadDepUnit (state, hsc_env) mods@(CachedDep {package = JsonFs uid} :| _) =
      if hasUnit uid hsc_env
      then loadActiveUnit (state, hscSetActiveUnitId uid hsc_env) (toList mods)
      else pure (state, hsc_env)

    loadActiveUnit = foldM loadDep

    loadDep (state, hsc_env) CachedDep {name = JsonFs name, interfaces = iface :| _} =
      liftIO (loadCachedDep log interp name (state, hsc_env) iface)

    byUnit = groupBy (on (==) (.package)) deps

loadHomeUnit ::
  Logger ->
  DynFlags ->
  FeatureFlags ->
  UnitId ->
  (WorkerState, HscEnv) ->
  OsPath ->
  IO (WorkerState, HscEnv)
loadHomeUnit log dflags0 features unit (state0, hsc_env0) path
  | hasUnit unit hsc_env0
  = pure (state0, hsc_env0)
  | otherwise
  = do
    cachedUnit@CachedUnit {unit_args} <- decodeJsonArg "--home-unit" path
    (state1, hsc_env1) <- fmap (fromMaybe (state0, hsc_env0)) $ for cachedUnit.dep_units \ file -> do
      deps <- decodeJsonArg "--home-unit" file
      loadCachedUnits log dflags0 deps features (state0, hsc_env0)
    dflags <- maybe (pure dflags0) (readParseGHCArgs features.flagParser hsc_env1 dflags0) unit_args
    logTimed log "Loading cached home unit" $ fmap swap do
      runStateT (loadCachedUnit log features.fixedNodesCache hsc_env1 unit (cachedUnit, dflags)) state1
