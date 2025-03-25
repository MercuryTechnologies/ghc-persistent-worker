{-# language ViewPatterns, CPP, OverloadedStrings #-}

module Internal.CompileHpt where

import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (DynFlags (..), Ghc, GhcMonad (..), ModLocation (..), ModSummary (..), mkGeneralLocated, setUnitDynFlags)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateHUG)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Pipeline (compileOne)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId, UnitState (..), stringToUnitId, unitIdString)
import GHC.Unit.Env (
  HomeUnitEnv (..),
  HomeUnitGraph,
  UnitEnv (..),
  UnitEnvGraph (..),
  addHomeModInfoToHug,
  ue_unsafeHomeUnit,
  unitEnv_lookup_maybe,
  )
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..))
import Internal.Cache (ModuleArtifacts (..), Target (..))
import Internal.Error (eitherMessages)
import Internal.Log (dbgs)

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

homeUnitDeps :: HscEnv -> UnitId -> Maybe [UnitId]
homeUnitDeps hsc_env target = do
  HomeUnitEnv {homeUnitEnv_units = UnitState {homeUnitDepends}} <- unitEnv_lookup_maybe target hug
  pure homeUnitDepends
  where
    hug = hsc_env.hsc_unit_env.ue_home_unit_graph

homeUnitDepFlags :: HscEnv -> Set UnitId -> UnitId -> [String]
homeUnitDepFlags hsc_env explicit target =
  concat [["-package-id", unitIdString u] | u <- fold prev_deps, not (Set.member u explicit)]
  where
    prev_deps = homeUnitDeps hsc_env target

initUnit :: [String] -> Ghc ()
initUnit specific = do
  hsc_env0 <- getSession
  let current = hsc_env0.hsc_unit_env.ue_current_unit
      (explicit, withPackageId) = adaptHp hsc_env0.hsc_unit_env.ue_home_unit_graph specific
      unitOptions = withPackageId ++ homeUnitDepFlags hsc_env0 explicit current
  dflags <- unitFlags unitOptions hsc_env0
  setUnitDynFlags current dflags
  modifySession (hscSetActiveUnitId current)

compileHpt ::
  [String] ->
  Target ->
  Ghc (Maybe ModuleArtifacts)
compileHpt specific (Target src) = do
  -- dbg ("------- start " ++ takeFileName src)
  -- dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
  initializeSessionPlugins
  initUnit specific
  -- dbg "------- updated"
  -- dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
  hsc_env <- getSession
  hmi@HomeModInfo {hm_iface = iface, hm_linkable} <- liftIO do
    summary <-
      fmap (setHiLocation hsc_env) .
      eitherMessages GhcDriverMessage =<<
      summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
    -- dbg "------ summarised"
    compileOne hsc_env summary 1 100000 Nothing (HomeModLinkable Nothing Nothing)
  -- dbg "------ compiled"
  modifySession (addDepsToHscEnv [hmi])
  -- dbg "------ added"
  pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hm_linkable})
