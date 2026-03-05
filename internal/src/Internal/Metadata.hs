{-# LANGUAGE CPP #-}

module Internal.Metadata where

import Control.Concurrent (readMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import GHC (
  DynFlags (..),
  Ghc,
  GhcException (..),
  GhcMode (..),
  ModuleGraph,
  getSession,
  getSessionDynFlags,
  setSession,
  )
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateFlags, hscUpdateLoggerFlags)
import GHC.Driver.Monad (modifySession, withSession, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId)
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.BuildPlan (buildPlanForSources)
import Internal.BuildPlan.Json (writeBuildPlan)
import Internal.Cache.Metadata (addHomeUnitTo, loadCachedUnits)
import Internal.Log (logTimed)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (runSession, withDynFlags, withGhcInSession)
import Internal.State (updateMakeStateVar)
import Internal.State.Make (insertUnitEnv, loadState, storeModuleGraph)
import Internal.State.Stats (logMemStats)
import System.Directory (createDirectoryIfMissing)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, unsafeEncodeUtf)
import Types.Args (Args (..), BuildPlanField, buildPlanAll)
import Types.BuildPlan (BuildPlan (..))
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.State (WorkerState (..))
import Types.Target (TargetSpec (..), UnitTarget (..))

#if !defined(MWB) && !defined(MWB_2025_10)

import GHC (ModSummary)

depJSON :: DynFlags -> Maybe FilePath
depJSON _ = Nothing

ms_opts :: ModSummary -> [String]
ms_opts _ = []

#endif

legacyMkDepend :: Bool
legacyMkDepend =
  False

-- | 'doMkDependHS' needs this to be enabled.
metadataTempSession :: HscEnv -> HscEnv
metadataTempSession =
  hscUpdateFlags \ d -> d {ghcMode = MkDepend}

-- | Add a new home unit to the current session using the provided 'DynFlags'.
-- The flags have been constructed from Buck CLI args passed to the metadata step, which, crucially, contain the package
-- DB arguments for dependencies.
addHomeUnit :: DynFlags -> Ghc UnitId
addHomeUnit dflags = do
  hsc_env <- getSession
  (hsc_env1, unit) <- liftIO $ addHomeUnitTo hsc_env dflags
  setSession hsc_env1
  pure unit

-- | Initialize the home unit env for this target and restore the module graphs computed previously for other units.
--
-- This part is the most significant difference that the make worker has from GHC make mode, since it never happens
-- natively that units are added incrementally.
-- Therefore, this is a relatively delicate procedure that hasn't been fully optimized yet.
--
-- We especially want to take care that the command line flags aren't applied to the base session before we initialize
-- the home unit in order to replicate what GHC does in @initMulti@.
prepareMetadataSession :: Env -> DynFlags -> Ghc UnitId
prepareMetadataSession env dflags = do
  state <- liftIO $ readMVar env.state
  modifySession \ hsc_env -> loadState hsc_env state.make
  unit <- addHomeUnit dflags
  setActiveUnit unit
  storeNewUnit
  pure unit
  where
    setActiveUnit unit = modifySession (hscUpdateLoggerFlags . hscSetActiveUnitId unit)

    storeNewUnit = withSession \ hsc_env -> liftIO $ updateMakeStateVar env.state (insertUnitEnv hsc_env)

resolveDepJson :: HscEnv -> Maybe OsPath -> Ghc OsPath
resolveDepJson hsc_env path =
  case (path, unsafeEncodeUtf <$> depJSON hsc_env.hsc_dflags) of
    (Just new, Just old)
      | new == old -> pure new
      | otherwise -> do
        liftIO $ OsPath.writeFile old mempty
        pure new
    (Just new, Nothing) -> pure new
    (Nothing, Just old) -> pure old
    (Nothing, Nothing) -> missingDepJson
  where
    missingDepJson =
      liftIO $ throwGhcExceptionIO (ProgramError "Metadata called without --build-plan or -dep-json")

-- | Dispatch build plan computation and writing the metadata JSON based on the flag 'legacyMkDepend'.
--
-- If set, run 'doMkDependHS', otherwise use the new customized version that calls @downsweep@ and constructs the JSON
-- without handling the Makefile argument.
--
-- If both the new and old CLI argument for the JSON path was specified, write an empty file to the old path if it
-- differs to satisfy Buck.
--
-- We need to use a temporary session because 'doMkDependHS' uses some custom settings that we don't want to leak,
-- though it's not been thoroughly tested what precisely the impact is.
writeMetadata ::
  Maybe OsPath ->
  Maybe (NonEmpty BuildPlanField) ->
  [String] ->
  Ghc ModuleGraph
writeMetadata path fieldSelection srcs = do
  initializeSessionPlugins
  withTempSession metadataTempSession do
    if legacyMkDepend
    then doMkDependHS srcs
    else do
      hsc_env <- getSession
      writeLegacyMakefile hsc_env
      depJson <- resolveDepJson hsc_env path
      plan <- buildPlanForSources fields srcs
      liftIO $ writeBuildPlan depJson plan
      pure plan.graph
  where
    fields = Set.fromList (toList (fromMaybe buildPlanAll fieldSelection))

    writeLegacyMakefile hsc_env =
      when (not (null hsc_env.hsc_dflags.depMakefile)) do
        liftIO $ writeFile hsc_env.hsc_dflags.depMakefile ""

-- | Run downsweep and merge the resulting module graph into the cached graph.
-- This is executed for the metadata step, which natively only calls 'doMkDependHS'.
-- Since that function doesn't give us access to the module graph in its original shape, we inline it into this project
-- to exfiltrate the graph.
--
-- Before downsweep, we also create a fresh @Finder@ to prevent 'doMkDependHS' from polluting the cache with entries
-- with different compilation ways and restore the previous unit env so dependencies are visible.
computeMetadata :: Env -> IO (Bool, Maybe TargetSpec)
computeMetadata env = do
  res <- runMaybeT do
    () <- MaybeT $ runSession env \ _ -> do
      dflags <- getSessionDynFlags
      for_ env.args.cachedBuildPlans \ bp ->
        withSession (liftIO . loadCachedUnits env.log env.state dflags bp)
      pure (Just ())
    logTimed env.log "Computing module graph" do
      MaybeT $ runSession env $ withDynFlags env \ dflags srcs -> do
        unit <- prepareMetadataSession env dflags
        let target = TargetUnit (UnitTarget unit)
        liftIO $ env.log.setTarget target
        module_graph <- writeMetadata env.args.buildPlan env.args.fields (fst <$> srcs)
        liftIO do
          updateMakeStateVar env.state (storeModuleGraph module_graph)
          for_ dflags.stubDir \ stubdir -> do
            env.log.debug ("Creating stubdir: " ++ stubdir)
            createDirectoryIfMissing False stubdir
        pure (Just target)
  logMemStats "after metadata" env.log
  pure (isJust res, res)

-- | Simplified metadata computation for the proxy executable.
-- Skips cache restoration and persistent worker state, directly computing and writing the build plan.
proxyMetadata :: Env -> IO Bool
proxyMetadata env =
  fmap isJust $ runSession env $ withGhcInSession env \ srcs ->
    Just () <$ writeMetadata env.args.buildPlan env.args.fields (fst <$> srcs)
