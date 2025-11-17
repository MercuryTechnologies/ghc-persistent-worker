{-# LANGUAGE ViewPatterns, CPP, OverloadedStrings, PatternSynonyms #-}
#define RECENT (MIN_VERSION_GLASGOW_HASKELL(9,13,0,0) || defined(MWB_2025_10))

module Internal.Compile.Make where

#if RECENT

import GHC.Unit.Module.Graph (ModuleNodeInfo (..))
import GHC.Unit.Module.Location (pattern ModLocation)

#endif

import qualified GHC
import GHC (
  DynFlags (..),
  GeneralFlag (..),
  Ghc,
  GhcException (..),
  GhcMonad (..),
  ModLocation (..),
  ModSummary (..),
  Module,
  gopt,
  mgLookupModule,
  )
import GHC.Driver.DynFlags (gopt_set)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Monad (modifySessionM)
import GHC.Driver.Pipeline (compileOne)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit.Env (ue_unsafeHomeUnit)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..))
import GHC.Utils.Outputable (ppr, showPprUnsafe, text, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import GHC.Utils.TmpFs (TmpFs, cleanCurrentModuleTempFiles, keepCurrentModuleTempFiles)
import Internal.Debug (pprModuleFull)
import Internal.Error (eitherMessages, noteGhc)
import Internal.Log (logTimedD)
import Internal.State (ModuleArtifacts (..))
import Internal.UnitEnv (addDepsToHscEnv)
import Types.Log (Logger (..))
import Types.Target (ModuleTarget (..), Target (..), TargetSpec (..))

-- | Update the location of the result of @summariseFile@ to point to the locations specified on the command line, since
-- these are placed in the source file's directory by that function.
setHiLocation :: HscEnv -> ModSummary -> ModSummary
setHiLocation HscEnv {hsc_dflags = DynFlags {outputHi = Just ml_hi_file, outputFile_ = Just ml_obj_file}} summ =
  summ {ms_location = summ.ms_location {ml_hi_file, ml_obj_file}}
setHiLocation _ summ = summ

cleanCurrentModuleTempFilesMaybe :: MonadIO m => GHC.Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs

computeSummary ::
  Logger ->
  HscEnv ->
  FilePath ->
  IO ModSummary
computeSummary logger hsc_env src = do
    logTimedD logger ("Computing fresh ModSummary for" <+> text src) do
      summResult <- summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
      setHiLocation hsc_env <$> eitherMessages GhcDriverMessage summResult

-- | Find a module in the module graph and return its `ModSummary`.
lookupSummary ::
  Logger ->
  HscEnv ->
  Module ->
  IO ModSummary
lookupSummary _logger hsc_env target =
  check =<< noteGhc notFound (mgLookupModule hsc_env.hsc_mod_graph target)
  where
    notFound =
      "Could not find ModSummary in the module graph for "
      ++
      showPprUnsafe (pprModuleFull target)

#if RECENT
    check = \case
      ModuleNodeCompile ms -> pure ms
      ModuleNodeFixed _ ModLocation {ml_hs_file} ->
        case ml_hs_file of
          Just src ->
            computeSummary _logger hsc_env src
          Nothing ->
            throwGhcExceptionIO (PprProgramError "Fixed node without source path" (ppr target))
#else
    check = pure
#endif

-- | Obtain a `ModSummary` for the current target.
-- If the target was specified by module name, we assume that the new workflow is used, in which the module graph is
-- fully initialized in the metadata request, and look it up there.
--
-- Otherwise, the source file path is used to generate a fresh summary.
ensureSummary ::
  Logger ->
  HscEnv ->
  TargetSpec ->
  IO ModSummary
ensureSummary logger hsc_env = \case
  TargetModule (ModuleTarget m) -> do
    logTimedD logger ("Fetching ModSummary for" <+> ppr m <+> "from module graph") do
      lookupSummary logger hsc_env m
  TargetSource (Target src) -> do
    computeSummary logger hsc_env src
  TargetUnit unit ->
    throwGhcExceptionIO (PprProgramError "Specified target unit for compile request" (ppr unit))
  TargetUnknown spec ->
    throwGhcExceptionIO (PprProgramError "Invalid target spec using TargetUnknown" (text spec))

-- | Compile a module with multiple home units in the session state, using the home package table to look up
-- dependencies.
--
-- First, update the current unit's configuration to include this module's dependencies.
-- Buck only provides @-package@ flags for deps that are used by a given module, while the unit state is designed to be
-- initialized up front with the deps of all modules.
-- Note: This should soon be obsolete, since we now have full control over the metadata step.
--
-- Next, perform the steps that usually happen in make mode's upsweep:
-- - Create a @ModSummary@ using @summariseFile@
-- - Call the module compilation function @compileOne@
-- - Store the resulting @HomeModInfo@ in the current unit's home package table.
compileModuleWithDepsInHpt ::
  Logger ->
  TargetSpec ->
  Ghc (Maybe ModuleArtifacts)
compileModuleWithDepsInHpt logger target =
  logTimedD logger "Compiling" do
    initializeSessionPlugins
    hsc_env <- getSession
    hmi@HomeModInfo {hm_iface = iface, hm_linkable} <- liftIO do
      summary <- ensureSummary logger hsc_env target
      result <- compileOne hsc_env (forceRecomp summary) 1 100000 Nothing (HomeModLinkable Nothing Nothing)
      cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) summary.ms_hspp_opts
      pure result
    modifySessionM (liftIO . addDepsToHscEnv [hmi])
    pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hm_linkable})
  where
    -- This bypasses another recompilation check in 'compileOne'
    forceRecomp summary =
      summary {ms_hspp_opts = gopt_set summary.ms_hspp_opts Opt_ForceRecomp}
