{-# language ViewPatterns, CPP, OverloadedStrings #-}

module Internal.CompileHpt where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import GHC (DynFlags (..), Ghc, GhcMonad (..), ModLocation (..), ModSummary (..), mkGeneralLocated, setUnitDynFlags)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env (HscEnv (..), hscUpdateHUG, hscSetActiveUnitId)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Pipeline (compileOne)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Runtime.Loader (initializePlugins)
import GHC.Unit.Env (UnitEnv (..), addHomeModInfoToHug, ue_unsafeHomeUnit, HomeUnitEnv (..), unitEnv_lookup_maybe, HomeUnitGraph, UnitEnvGraph (..))
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..))
import Internal.Cache (ModuleArtifacts (..), Target (..))
import Internal.CompileMake (step1)
import Internal.Debug (showHugShort)
import Internal.Error (eitherMessages)
import Internal.Log (dbg, dbgp, dbgs)
import System.FilePath ((</>))
import GHC.Unit (UnitState(..), unitIdString, stringToUnitId, UnitId)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

addDepsToHscEnv :: [HomeModInfo] -> HscEnv -> HscEnv
addDepsToHscEnv deps = hscUpdateHUG (\hug -> foldr addHomeModInfoToHug hug deps)

setHiLocation :: HscEnv -> ModSummary -> ModSummary
setHiLocation HscEnv {hsc_dflags = DynFlags {outputHi = Just ml_hi_file, outputFile_ = Just ml_obj_file}} summ =
  summ {ms_location = summ.ms_location {ml_hi_file, ml_obj_file}}
setHiLocation _ summ = summ

unitFlags :: [String] -> HscEnv -> Ghc DynFlags
unitFlags args HscEnv {hsc_logger, hsc_dflags = dflags0} = do
  (dflags, fileish, warns) <- parseDynamicFlagsCmdLine dflags0 (map (mkGeneralLocated "no loc") args)
  dbgs fileish
  liftIO $ printOrThrowDiagnostics hsc_logger (initPrintConfig dflags) (initDiagOpts dflags) (GhcDriverMessage <$> warns)
  pure dflags

adaptHp :: HomeUnitGraph -> [String] -> (Set UnitId, [String])
adaptHp (UnitEnvGraph ueg) =
  spin mempty
  where
    spin seen = \case
      "-package" : p : rest
        | Map.member (stringToUnitId p) ueg
        -> (["-package-id", p] ++) <$> spin (Set.insert (stringToUnitId p) seen) rest
      arg : rest -> (arg :) <$> spin seen rest
      [] -> (seen, [])

compileHpt ::
  String ->
  NonEmpty (String, String, [String]) ->
  [String] ->
  Target ->
  Ghc (Maybe ModuleArtifacts)
compileHpt tmp units specific (Target src) = do
  dbg "------- start"
  dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
  hsc_env0 <- liftIO . initializePlugins =<< getSession
  let current = hsc_env0.hsc_unit_env.ue_current_unit
      prev_deps = do
        HomeUnitEnv {homeUnitEnv_units = UnitState {homeUnitDepends}} <- unitEnv_lookup_maybe current hsc_env0.hsc_unit_env.ue_home_unit_graph
        pure homeUnitDepends
      (explicit, withPackageId) = adaptHp hsc_env0.hsc_unit_env.ue_home_unit_graph specific
  if manual
  then do
    dflags <- unitFlags (withPackageId ++ concat [["-package-id", unitIdString u] | u <- fold prev_deps, not (Set.member u explicit)]) hsc_env0
    setUnitDynFlags hsc_env0.hsc_unit_env.ue_current_unit dflags
  else step1 unitArgs
  modifySession (hscSetActiveUnitId current)
  dbg "------- updated"
  dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
  hsc_env <- getSession
  hmi@HomeModInfo {hm_iface = iface, hm_linkable} <- liftIO do
    summary <-
      fmap (setHiLocation hsc_env) .
      eitherMessages GhcDriverMessage =<<
      summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
    dbg "------ summarised"
    compileOne hsc_env summary 1 100000 Nothing (HomeModLinkable Nothing Nothing)
  dbg "------ compiled"
  modifySession (addDepsToHscEnv [hmi])
  dbg "------ added"
  pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hm_linkable})
  where
    unitArgs = units <&> \ (name, srcDir, deps) ->
      ["-i", "-i" ++ srcDir, "-hidir" ++ tmp </> "out", "-i" ++ tmp </> "out", "-this-unit-id", name] ++ concat [["-package-id", dep] | dep <- deps]

    manual = True
