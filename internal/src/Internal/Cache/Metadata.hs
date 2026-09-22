{-# LANGUAGE CPP #-}

module Internal.Cache.Metadata where

import Control.Applicative ((<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, throwIO, try)
import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT (..), modify, modifyM)
import Data.Aeson (eitherDecodeFileStrict')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Foldable (fold, traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Traversable (for)
import Data.Tuple (swap)
import GHC (DynFlags (..), IsBootInterface (..), ModuleName, mkModule, mkModuleName)
import qualified GHC as GHC
import GHC.Driver.DynFlags (PackageDBFlag (..), PackageFlag (..), PkgDbRef (..))
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (..))
import GHC.Driver.Make (ModNodeKeyWithUid (..), summariseFile)
import GHC.Driver.Session (topDir, updatePlatformConstants)
import GHC.Fingerprint (Fingerprint, fingerprintFingerprints, getFileHash)
import GHC.Iface.Recomp.Binary (computeFingerprint, putNameLiterally)
import GHC.Iface.Recomp.Flags (fingerprintDynFlags, fingerprintHpcFlags, fingerprintOptFlags)
import GHC.Types.SourceError (throwErrors)
import GHC.Unit (Definite (..), GenUnit (..), GenWithIsBoot (..), HomeUnit, UnitDatabase, UnitId (..), UnitState)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), updateHug)
import GHC.Unit.Home (GenHomeUnit (DefiniteHomeUnit))
import GHC.Unit.Home.PackageTable (emptyHomePackageTable)
import GHC.Unit.Module.Graph (ModuleGraphNode (..), NodeKey (..))
import GHC.Utils.CliOption (Option (..))
import GHC.Utils.Outputable (comma, hcat, ppr, punctuate, quotes, showPprUnsafe, text, (<+>))
import Internal.Compat.GHC914 (moduleNodeEdge)
import Internal.Compat.UnitIndex (initUnits)
import Internal.DynFlags (buckLocation, parseFlags, setupPath)
import Internal.DynFlags.Parse (parseDynFlags)
import Internal.Error (eitherMessages, unknownErrors)
import Internal.Log (logDebugD, logTimed, logTimedD)
import Internal.State (updateMakeState)
import qualified Internal.State.Make as Make
import Internal.State.Make (insertUnitEnv, rebuildModuleGraph, storeModuleGraphNodes)
import System.OsPath.Extra (OsPath, fromOsPath)
import Types.BuckArgs (CachedBuckArgs (..), parseCachedBuckArgs)
import Types.CachedDeps (
  CachedBuildPlan (..),
  CachedBuildPlans (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  )
import Types.FeatureFlags (FeatureFlags (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.State.Make (LibLoadState (..), MakeState (..), PlanFiles (..), UnitFingerprint (..))

#if defined(FIXED_NODES) || defined(MWB)

import GHC.Unit.Home.Graph (unitEnv_insert, unitEnv_keys)

#else

import GHC.Unit.Env (unitEnv_insert, unitEnv_keys, unitEnv_lookup_maybe)

#endif

#if defined(FIXED_NODES)

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Types.SourceFile (HscSource (HsSrcFile))
import GHC.Unit.Finder (addHomeModuleToFinder, mkHomeModLocation)
import GHC.Unit.Module.Graph (ModuleNodeInfo (..))
import System.OsPath.Extra (splitExtension)

#endif

-- | Add a fresh 'HomeUnitEnv' to the home unit graph using the supplied unit state and dependencies.
insertHomeUnit ::
  UnitId ->
  DynFlags ->
  [UnitDatabase UnitId] ->
  UnitState ->
  HomeUnit ->
  UnitEnv ->
  IO UnitEnv
insertHomeUnit unit dflags dbs unit_state home_unit unit_env = do
  hpt <- emptyHomePackageTable
  pure (updateHug (unitEnv_insert unit (hue hpt)) unit_env) {
    ue_platform = targetPlatform dflags,
    ue_namever = ghcNameVersion dflags
  }
  where
    hue homeUnitEnv_hpt = HomeUnitEnv {
      homeUnitEnv_units = unit_state,
      homeUnitEnv_unit_dbs = Just dbs,
      homeUnitEnv_dflags = dflags,
      homeUnitEnv_hpt,
      homeUnitEnv_home_unit = Just home_unit
    }

-- | Create a new home unit using the supplied 'DynFlags'.
-- This allows specifying a set of home units that differ from the currently present units, to allow concurrent cache
-- restoration.
initUnitsAndPlatform ::
  HscEnv ->
  DynFlags ->
  Set UnitId ->
  IO (DynFlags, [UnitDatabase UnitId], UnitState, HomeUnit)
initUnitsAndPlatform hsc_env dflags0 allUnitIds = do
  (dbs, unit_state, home_unit, mconstants) <- initUnits hsc_env dflags0 allUnitIds
  dflags1 <- updatePlatformConstants dflags0 mconstants
  pure (dflags1, dbs, unit_state, home_unit)

-- | Create a new home unit using the supplied 'DynFlags'.
initHomeUnit :: HscEnv -> DynFlags -> UnitId -> UnitEnv -> IO UnitEnv
initHomeUnit hsc_env dflags0 unit unit_env = do
  (dflags1, dbs, unit_state, home_unit) <- initUnitsAndPlatform hsc_env dflags0 allUnitIds
  insertHomeUnit unit dflags1 dbs unit_state home_unit unit_env
  where
    allUnitIds = unitEnv_keys (ue_home_unit_graph unit_env)

-- | Add a new home unit to the given session using the provided 'DynFlags'.
-- The flags have been constructed from Buck CLI args passed to the metadata step, which, crucially, contain the package
-- DB arguments for dependencies.
addHomeUnitTo :: HscEnv -> DynFlags -> IO (HscEnv, UnitId)
addHomeUnitTo hsc_env dflags = do
  unit_env <- initHomeUnit hsc_env dflags unit hsc_env.hsc_unit_env
  pure (hsc_env {hsc_unit_env = unit_env}, unit)
  where
    unit = dflags.homeUnitId_

decodeJsonBuildPlan :: OsPath -> IO CachedUnit
decodeJsonBuildPlan =
  eitherDecodeFileStrict' . fromOsPath >=> \case
    Right a -> pure a
    Left err -> throwIO (userError err)

-- | GHC's own recompilation fingerprint of a unit's flags, the check @--make@ runs before it trusts an interface,
-- extended with the inputs GHC leaves out of the flag hash but the worker builds the unit state from: the package
-- flags, the package databases and the GHC libdir. The active unit must already be in the session, since
-- 'fingerprintDynFlags' reads the home unit; the flags being fingerprinted are set on 'hsc_dflags' here rather than
-- taken from the stored unit. GHC's flag hash excludes @-odir@ and @-hidir@, so two execution roots do not differ;
-- @initUnits@ does not touch the fields fingerprinted here, so the parsed args suffice whether or not the unit state
-- has been resolved.
flagsFingerprint :: HscEnv -> UnitId -> DynFlags -> IO Fingerprint
flagsFingerprint hsc_env unit dflags = do
  dyn <- fingerprintDynFlags env this_mod putNameLiterally
  opt <- fingerprintOptFlags dflags putNameLiterally
  hpc <- fingerprintHpcFlags dflags putNameLiterally
  extra <- computeFingerprint putNameLiterally (dbs, pkgs, topDir dflags)
  pure (fingerprintFingerprints [dyn, opt, hpc, extra])
  where
    env = (hscSetActiveUnitId unit hsc_env) {hsc_dflags = dflags}
    this_mod = mkModule (RealUnit (Definite unit)) (mkModuleName "Main")
    dbs = renderDb <$> packageDBFlags dflags
    pkgs = renderPkg <$> packageFlags dflags

    renderDb = \case
      PackageDB ref -> "db:" ++ renderRef ref
      NoUserPackageDB -> "no-user-db"
      NoGlobalPackageDB -> "no-global-db"
      ClearPackageDBs -> "clear-dbs"

    renderRef = \case
      GlobalPkgDb -> "global"
      UserPkgDb -> "user"
      PkgDbPath p -> fromOsPath p

    renderPkg = \case
      ExposePackage raw _ _ -> "expose:" ++ raw
      HidePackage p -> "hide:" ++ p

planModules :: CachedUnit -> Set ModuleName
planModules CachedUnit {build_plan, cache} =
  Set.fromList (coerce <$> Map.keys (fold (cache <|> build_plan)))

planFilesHash :: OsPath -> Maybe OsPath -> IO PlanFiles
planFilesHash planPath argsPath = do
  plan <- getFileHash (fromOsPath planPath)
  args <- for argsPath \ p -> (p,) <$> getFileHash (fromOsPath p)
  pure PlanFiles {plan, args}

unitFingerprint :: HscEnv -> UnitId -> DynFlags -> Set ModuleName -> Maybe PlanFiles -> IO UnitFingerprint
unitFingerprint hsc_env unit dflags modules planFiles = do
  flags <- flagsFingerprint hsc_env unit dflags
  pure UnitFingerprint {flags, modules, planFiles}

-- | Whether a known unit's kept state may serve a request that names this plan.
-- 'Valid' carries a refreshed record when the byte fast path missed but the semantic check passed, so the next request
-- takes the fast path again.
data Verdict =
  Valid (Maybe UnitFingerprint)
  |
  Stale String

-- | Decide a 'Verdict' for a unit already present in the session. Fail closed: a unit with no stored record, or a plan
-- or args file that cannot be read, is 'Stale'. The fast path compares the plan and args file hashes with the stored
-- ones; on a miss the plan is decoded and its flags and module set are compared with the stored fingerprint.
validateStoredUnit :: Logger -> FeatureFlags -> HscEnv -> MakeState -> UnitId -> OsPath -> IO Verdict
validateStoredUnit logger features hsc_env make unit planPath =
  case Map.lookup unit make.unitFingerprints of
    Nothing -> stale "no fingerprint recorded"
    Just stored ->
      try @SomeException (verdict stored) >>= \case
        Left e -> stale ("plan or args file unreadable: " ++ show e)
        Right v -> pure v
  where
    verdict stored = do
      planHash <- getFileHash (fromOsPath planPath)
      case stored.planFiles of
        Just pf | pf.plan == planHash -> do
          argsMatch <- case pf.args of
            Nothing -> pure True
            Just (ap, ah) -> (== ah) <$> getFileHash (fromOsPath ap)
          if argsMatch then pure (Valid Nothing) else semantic stored
        _ -> semantic stored

    semantic stored = do
      cachedUnit <- decodeJsonBuildPlan planPath
      dflags <- maybe (pure hsc_env.hsc_dflags) (readParseGHCArgs features.flagParser hsc_env hsc_env.hsc_dflags) cachedUnit.unit_args
      newFlags <- flagsFingerprint hsc_env unit dflags
      let newMods = planModules cachedUnit
      if newFlags /= stored.flags
        then stale "flags changed"
        else if newMods /= stored.modules
          then stale ("module set changed: " ++ moduleSetDiff stored.modules newMods)
          else do
            pf <- planFilesHash planPath cachedUnit.unit_args
            pure (Valid (Just UnitFingerprint {flags = newFlags, modules = newMods, planFiles = Just pf}))

    stale reason = do
      logger.info ("ghc-worker: evict unit " ++ showPprUnsafe unit ++ ": " ++ reason)
      pure (Stale reason)

    moduleSetDiff old new =
      unwords (["+" ++ showPprUnsafe m | m <- Set.toAscList (Set.difference new old)]
        ++ ["-" ++ showPprUnsafe m | m <- Set.toAscList (Set.difference old new)])

-- | Construct a 'ModuleGraphNode' from data obtained from the Buck cache and add its location to the @Finder@.
--
-- If GHC has fixed module graph nodes, those are constructed; otherwise we have to call 'summariseFile' to create a
-- full node, which parses the module.
loadCachedModule :: Bool -> HscEnv -> UnitId -> JsonFs ModuleName -> CachedModule -> IO ModuleGraphNode
loadCachedModule useFixedNodes hsc_env unit (JsonFs modName) CachedModule {source, modules, packages, flags} = do
  node <- createNode source modName
  pure (ModuleNode (moduleNodeEdge <$> (homeDeps ++ packageDeps)) node)
  where
    homeDeps =
      [
        NodeKey_Module (ModNodeKeyWithUid (GWIB depName NotBoot) unit)
        |
        JsonFs depName <- modules
      ]

    packageDeps =
      [
        NodeKey_Module (ModNodeKeyWithUid (GWIB depName NotBoot) depUnit)
        |
        CachedPackageDep {id = JsonFs depUnit, modules = depModules} <- packages,
        JsonFs depName <- depModules
      ]

    createNode src name
      | useFixedNodes
      = createNodeFixed src name
      | otherwise
      = createNodeCompile src

#if defined(FIXED_NODES)

    createNodeFixed src name = do
      _ <- addHomeModuleToFinder hsc_env.hsc_FC (DefiniteHomeUnit unit Nothing) name location HsSrcFile
      pure $ ModuleNodeFixed (ModNodeKeyWithUid (GWIB name NotBoot) unit) location
      where
        fopts = initFinderOpts (hsc_dflags hsc_env)
        (basename, extension) = splitExtension src
        location = mkHomeModLocation fopts name basename extension HsSrcFile

    createNodeCompile src = ModuleNodeCompile <$> createNodeLegacy src

#else

    createNodeFixed src _ = createNodeLegacy src

    createNodeCompile = createNodeLegacy

#endif

    createNodeLegacy src = do
      summResult <- summariseFile hsc_env (DefiniteHomeUnit unit Nothing) mempty (fromOsPath src) Nothing Nothing
      summary <- eitherMessages GhcDriverMessage summResult

      -- Apply per-module GHC flags.
      (dflags', _, _) <- GHC.parseDynamicFlags
          hsc_env.hsc_logger
          (GHC.ms_hspp_opts summary)
          (map GHC.noLoc flags)
      pure summary {GHC.ms_hspp_opts = dflags'}


-- | Restore non-GHC state from the Buck cache.
-- Command line arguments interpreted directly by the worker aren't part of the unit args, but they can yet influence
-- the behavior of unit state restoration from cache.
--
-- At the moment, this only includes the @$PATH@ variable, which is relevant even when restoring the module graph from
-- cache because we're parsing the source code, which might include executing preprocessors.
loadCachedArgs ::
  OsPath ->
  StateT WorkerState IO ()
loadCachedArgs path = do
  cachedArgs <- liftIO $ readFile (fromOsPath path)
  case parseCachedBuckArgs (lines cachedArgs) of
    Right args -> modifyM (setupPath args.cachedBinPath)
    Left err -> liftIO $ throwIO (userError err)

-- | Ensure there are no positional args that are remaining and not parsed yet up to this point.
--   If exists, error with Left case.
ensureNoPositionalArgs :: (DynFlags, [ByteString]) -> Either DriverMessages DynFlags
ensureNoPositionalArgs = \case
  (dflags, []) -> Right dflags
  (dflags, args) -> Left (unknownErrors (Just "worker") dflags (argsError args))
  where
    argsError args =
      text "Found positional args when restoring unit state from cache:"
      <+>
      hcat (punctuate comma (text . Text.unpack . decodeUtf8 <$> args))

-- | Cached CLI args for a unit.
--
-- This function is separated so that we can parallelize CLI arg parsing part.
readParseGHCArgs ::
  Bool ->
  HscEnv ->
  DynFlags ->
  OsPath ->
  IO DynFlags
readParseGHCArgs parseFast hsc_env0 dflags0 args_file
  | parseFast = do
    args <- BS.readFile (fromOsPath args_file)
    -- TODO: for correctness, we may need to ensure there are no positional args left
    -- by ensureNoPositionalArgs, but this needs to be adjusted with client implementation.
    either (throwErrors . fmap GhcDriverMessage) pure (fst <$> parseDynFlags dflags0 args)
  | otherwise = do
    args <- readFile (fromOsPath args_file)
    (dflags1, _, _, _) <- parseFlags dflags0 hsc_env0.hsc_logger (buckLocation <$> lines args)
    pure dflags1

loadCachedModules ::
  Bool ->
  HscEnv ->
  UnitId ->
  CachedUnit ->
  IO [ModuleGraphNode]
loadCachedModules useFixedNodes hsc_env unit CachedUnit {build_plan, cache} =
  traverse (uncurry (loadCachedModule useFixedNodes hsc_env unit)) modules
  where
    modules = Map.toList (fold (cache <|> build_plan))

-- | Restore the unit state and module graph from the external cache.
--
-- The cached data consists of a simple list of GHC command line arguments that can recreate the unit state, as well as
-- the module graph produced by a previous metadata request.
loadCachedHomeUnit ::
  Logger ->
  Bool ->
  Bool ->
  HscEnv ->
  UnitId ->
  OsPath ->
  (CachedUnit, DynFlags) ->
  StateT WorkerState IO HscEnv
loadCachedHomeUnit logger useFixedNodes useIncrModGraph hsc_env0 unit planPath (cachedUnit, dflags) =
  logTimedD logger (text "Loading cached home unit" <+> quotes (ppr unit)) do
    traverse_ loadCachedArgs cachedUnit.unit_buck_args
    hsc_env2 <- liftIO do
      (hsc_env1, _) <- addHomeUnitTo hsc_env0 dflags
      pure (hscSetActiveUnitId unit hsc_env1)
    modify (updateMakeState (insertUnitEnv hsc_env2))
    nodes <- liftIO $ loadCachedModules useFixedNodes hsc_env2 unit cachedUnit
    modify (updateMakeState (rebuildModuleGraph useIncrModGraph . storeModuleGraphNodes nodes))
    fp <- liftIO do
      planFiles <- planFilesHash planPath cachedUnit.unit_args
      unitFingerprint hsc_env2 unit dflags (planModules cachedUnit) (Just planFiles)
    modify (updateMakeState (Make.storeUnitFingerprint unit fp))
    pure hsc_env2

-- | Intermediate result of the concurrent loading phase.
data PreparedUnit =
  PreparedUnit {
    unitId :: UnitId,
    dflags :: DynFlags,
    dbs :: [UnitDatabase UnitId],
    unitState :: UnitState,
    homeUnit :: HomeUnit,
    moduleEntries :: [(JsonFs ModuleName, CachedModule)],
    buckArgs :: Maybe OsPath,
    planFiles :: PlanFiles
  }

insertPreparedUnit :: Logger -> FeatureFlags -> HscEnv -> PreparedUnit -> StateT WorkerState IO HscEnv
insertPreparedUnit logger features hsc_env pu = do
  logDebugD logger (text "Loading cached unit" <+> quotes (ppr pu.unitId))
  traverse_ loadCachedArgs pu.buckArgs
  hsc_env2 <- liftIO do
    unit_env <- insertHomeUnit pu.unitId pu.dflags pu.dbs pu.unitState pu.homeUnit hsc_env.hsc_unit_env
    let hsc_env1 = hsc_env {hsc_unit_env = unit_env}
    pure (hscSetActiveUnitId pu.unitId hsc_env1)
  -- TODO: this ad hoc extraction of extra library dependency should be replaced by proper specification
  -- from the build system rules and recorded in a file, preferrably to buildplan file.
  let lib_paths = libraryPaths pu.dflags
      libs = [ lib | Option ('-':'l':lib) <- ldInputs pu.dflags ]
  let updateExtraLibs = \make ->
        let requested' = Map.insert pu.unitId (lib_paths, libs) make.extraLib.requested
         in make {extraLib = LibLoadState requested' make.extraLib.loaded}

  modify (updateMakeState (updateExtraLibs . insertUnitEnv hsc_env2))
  nodes <- liftIO $ traverse (uncurry (loadCachedModule features.fixedNodesCache hsc_env2 pu.unitId)) pu.moduleEntries
  modify (updateMakeState (storeModuleGraphNodes nodes))
  fp <- liftIO $ unitFingerprint hsc_env2 pu.unitId pu.dflags (Set.fromList (coerce . fst <$> pu.moduleEntries)) (Just pu.planFiles)
  modify (updateMakeState (Make.storeUnitFingerprint pu.unitId fp))
  pure hsc_env2

loadCachedBuildPlan ::
  HscEnv ->
  DynFlags ->
  FeatureFlags ->
  Set UnitId ->
  CachedBuildPlan ->
  IO (Maybe PreparedUnit)
loadCachedBuildPlan hsc_env1 dflags0 features allUnitIds CachedBuildPlan {name = JsonFs unitId, build_plan} = do
  cachedUnit@CachedUnit {unit_args} <- decodeJsonBuildPlan build_plan
  for unit_args \ argsFile -> do
    dflags1 <- readParseGHCArgs features.flagParser hsc_env1 dflags0 argsFile
    (dflags2, dbs, unitState, homeUnit) <- initUnitsAndPlatform hsc_env1 dflags1 allUnitIds
    planFiles <- planFilesHash build_plan cachedUnit.unit_args
    let moduleEntries = Map.toList (fold (cachedUnit.cache <|> cachedUnit.build_plan))
    pure PreparedUnit {
      unitId,
      dflags = dflags2,
      dbs,
      unitState,
      homeUnit,
      moduleEntries,
      buckArgs = cachedUnit.unit_buck_args,
      planFiles
    }

-- | Determine the set of build plans that need to be restored from cache because they aren't present in the state's
-- unit env; as well as the set of unit IDs that will be present after restoration.
--
-- Preserves the dependency order of cached units in @missingPlans@.
compareUnits :: HscEnv -> [CachedBuildPlan] -> (Set UnitId, [CachedBuildPlan])
compareUnits hsc_env buildPlans =
  (total, missingPlans)
  where
    total = Set.union known (Set.fromList missing)

    missing = coerce [name | CachedBuildPlan {name} <- missingPlans]

    missingPlans = filter planMissing buildPlans

    planMissing CachedBuildPlan {name = JsonFs unit} = not (Set.member unit known)

    known = unitEnv_keys (ue_home_unit_graph hsc_env.hsc_unit_env)

-- | Process build plans concurrently, limiting the number of threads to the number of CPUs.
processConcurrent :: (CachedBuildPlan -> IO (Maybe PreparedUnit)) -> [CachedBuildPlan] -> IO [Maybe PreparedUnit]
processConcurrent f plans = do
  threads <- getNumCapabilities
  sem <- newQSem threads
  forConcurrently plans \ plan ->
    bracket_ (waitQSem sem) (signalQSem sem) (f plan)

-- | Restore the unit state and module graph for each unit in cache that isn't present in the unit env.
--
-- Phase 1 (concurrent): For each absent unit, decode JSON, parse GHC args, and run 'initUnits'.
-- This is safe because 'initUnits' only reads DynFlags and package DBs; it doesn't modify the 'UnitEnv'.
-- We pre-compute the full set of home unit IDs to avoid the sequential dependency.
--
-- Phase 2 (sequential): Insert prepared units into the 'UnitEnv', build graph nodes, store module graphs.
--
-- Phase 3: Derive GHC's 'ModuleGraph' from the accumulated node index, once for the whole batch.
loadCachedDepUnits ::
  Logger ->
  DynFlags ->
  CachedBuildPlans ->
  FeatureFlags ->
  (WorkerState, HscEnv) ->
  IO (WorkerState, HscEnv)
loadCachedDepUnits logger dflags0 (CachedBuildPlans buildPlans) features (state0, hsc_env0) = do
  let hsc_env1 = Make.loadState hsc_env0 state0.make
      known = unitEnv_keys (ue_home_unit_graph hsc_env1.hsc_unit_env)
  state0' <- foldM (revalidateUnit logger features hsc_env1) state0
    [(unit, build_plan) | CachedBuildPlan {name = JsonFs unit, build_plan} <- buildPlans, Set.member unit known]
  let hsc_env1' = Make.loadState hsc_env0 state0'.make
  logTimed logger "Loading cached dep units" $ fmap swap do
    let (total, missing) = compareUnits hsc_env1' buildPlans
    prepared <- catMaybes <$> traverser (loadCachedBuildPlan hsc_env1' dflags0 features total) missing
    (hsc_env2, state1) <- runStateT (foldM (insertPreparedUnit logger features) hsc_env1' prepared) state0'
    pure (hsc_env2, updateMakeState (Make.rebuildModuleGraph features.useIncrModGraph) state1)
  where
    traverser = if features.concurrentInitUnits then processConcurrent else traverse

-- | A known dependency unit keeps its kept state only while its plan still validates; otherwise it is evicted here so
-- 'compareUnits' then treats it as missing and restores it in dependency order.
revalidateUnit :: Logger -> FeatureFlags -> HscEnv -> WorkerState -> (UnitId, OsPath) -> IO WorkerState
revalidateUnit logger features hsc_env state (unit, planPath) =
  validateStoredUnit logger features hsc_env state.make unit planPath >>= \case
    Valid Nothing -> pure state
    Valid (Just fp) -> pure (updateMakeState (Make.storeUnitFingerprint unit fp) state)
    Stale _ -> pure (updateMakeState (Make.evictUnit features.useIncrModGraph unit) state)
