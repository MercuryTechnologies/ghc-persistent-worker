{-# language OverloadedStrings, CPP #-}

module Internal.Debug where

import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import GHC (DynFlags (..), IsBootInterface (..), Module, mi_module, moduleName)
import GHC.Fingerprint (fingerprintString)
import GHC.Types.Unique.Map (nonDetEltsUniqMap)
import GHC.Unit (UnitDatabase (..), UnitId, UnitState (..), homeUnitId, moduleEnvToList, moduleUnitId, unitPackageId)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..))
import GHC.Unit.External (ExternalPackageState (..), eucEPS)
import GHC.Unit.Module.Graph (ModuleGraph)
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Outputable (Outputable, SDoc, hang, hcat, ppr, text, vcat, (<+>))
import System.FilePath ((</>))
import Types.Target (TargetSpec, renderTargetSpec)

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) || defined(MWB)

import GHC (ModSummary (..))
import GHC.Unit.Home.Graph (UnitEnvGraph (..))
import GHC.Unit.Home.PackageTable (HomePackageTable (..), pprHPT)
import GHC.Unit.Module.ModSummary (isBootSummary)

#else

import GHC.Types.Unique.DFM (udfmToList)
import GHC.Unit.Env (UnitEnvGraph (..))
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomePackageTable, hm_iface)
import GHC.Utils.Outputable (comma, punctuate)

#endif

#if defined(FIXED_NODES)

import GHC.Unit.Module.Graph (ModuleNodeInfo (..))

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) && !defined(MWB)

import Data.Foldable (toList)
import GHC.Unit.Module.Graph (mgTransDeps)

#else

import GHC.Unit.Module.Graph (ModuleGraphNode (..), mgModSummaries')

#endif

#if defined(UNIT_INDEX)

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.List (intercalate)
import GHC (
  Ghc,
  GhcMode (..),
  ModuleName,
  PkgQual (..),
  getSession,
  mgModSummaries,
  moduleNameString,
  moduleUnit,
  ms_mod_name,
  )
import GHC.Driver.Env (HscEnv (..), hscUnitIndexQuery, hsc_units)
import GHC.Types.Unique.Map (nonDetUniqMapToList)
import GHC.Unit (ModuleOrigin (..))
import GHC.Unit.Finder (FindResult (..), findImportedModule)
import GHC.Unit.State (UnitIndexQuery (..))
import GHC.Utils.Outputable (showPprUnsafe)
import Internal.Log (dbg)

#endif

entryD :: (SDoc, SDoc) -> SDoc
entryD (k, v) = hang (hcat [k, ":"]) 2 v

entry :: (String, SDoc) -> SDoc
entry (k, v) = entryD (text k, v)

entries :: [(String, SDoc)] -> SDoc
entries = vcat . fmap entry

showMap ::
  Outputable a =>
  (b -> SDoc) ->
  [(a, b)] ->
  SDoc
showMap pprB m =
  vcat [ppr from <+> text "->" <+> (pprB to) | (from, to) <- m]

pprModuleFull :: Module -> IsBootInterface -> SDoc
pprModuleFull m boot =
  ppr (moduleUnitId m) Outputable.<> ":" Outputable.<> ppr (moduleName m) Outputable.<>
  (if boot == IsBoot then " {-# SOURCE #-}" else "")

#if defined(FIXED_NODES)

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  vcat (concatMap showOne (mgModSummaries' g))
  where
    showOne = \case
      ModuleNode deps (ModuleNodeCompile ms) -> [hang (pprModuleFull (ms_mod ms) (isBootSummary ms) <+> "->") 2 (vcat (ppr <$> deps))]
      ModuleNode deps (ModuleNodeFixed key _) -> [hang (ppr key <+> "->") 2 (vcat (ppr <$> deps))]
      LinkNode deps unit -> [hang (ppr unit <+> "->") 2 (vcat (ppr <$> deps))]
      -- UnitNode deps unit -> [hang (ppr unit <+> "->") 2 (vcat (ppr <$> deps))]
      _ -> []

#else

#if defined(MWB)

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  vcat (showOne <$> mgModSummaries' g)
  where
    showOne = \case
      ModuleNode deps ms -> hang (pprModuleFull (ms_mod ms) (isBootSummary ms) <+> "->") 2 (vcat (ppr <$> deps))
      _ -> ""

#else

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  showMap (ppr . toList) (Map.toList (mgTransDeps g))

#endif

#endif

showEps :: ExternalPackageState -> IO SDoc
showEps EPS {..} = do
  pure $ entries $ [
    ] ++ if False then [pit] else []
  where
    pit = ("pit", vcat [ppr m <+> ppr (mi_module iface) | (m, iface) <- moduleEnvToList eps_PIT])

showUnitState :: UnitState -> SDoc
showUnitState UnitState {..} =
  entries $ [
    ("homeUnitDepends", ppr homeUnitDepends)
  ] ++
  if False
  then [("unitInfoMap", ppr (ppr . unitPackageId <$> nonDetEltsUniqMap unitInfoMap))]
  else []

showHomeUnitDflags :: DynFlags -> SDoc
showHomeUnitDflags DynFlags {homeUnitId_} =
  entries [
    ("homeUnitId", ppr homeUnitId_)
  ]

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) || defined(MWB)

showHpt :: HomePackageTable -> IO SDoc
showHpt = pprHPT

#else

showHpt :: HomePackageTable -> IO SDoc
showHpt hpt =
  pure $ hcat (punctuate comma [ppr (mi_module hm_iface) | (_, HomeModInfo {..}) <- udfmToList hpt])
   -- <+> ppr hm_linkable

#endif

showDbPath :: UnitDatabase UnitId -> SDoc
showDbPath UnitDatabase {unitDatabasePath} =
  text (show unitDatabasePath)

showHomeUnitEnvShort :: HomeUnitEnv -> IO SDoc
showHomeUnitEnvShort HomeUnitEnv {..} = do
  hpt <- showHpt homeUnitEnv_hpt
  pure $ entries [
    ("deps", ppr homeUnitEnv_units.homeUnitDepends),
    ("dbs", maybe (text "not loaded") (ppr . fmap showDbPath) homeUnitEnv_unit_dbs),
    ("hpt", hpt)
    ]

showHomeUnitEnv :: HomeUnitEnv -> IO SDoc
showHomeUnitEnv HomeUnitEnv {..} = do
  hpt <- showHpt homeUnitEnv_hpt
  pure $ entries [
    ("units", showUnitState homeUnitEnv_units),
    ("homeUnitEnv_unit_dbs", ppr homeUnitEnv_unit_dbs),
    ("dflags", showHomeUnitDflags homeUnitEnv_dflags),
    ("hpt", hpt),
    ("home_unit", ppr (homeUnitId <$> homeUnitEnv_home_unit))
    ]

showHugShort :: HomeUnitGraph -> IO SDoc
showHugShort (UnitEnvGraph hug) = do
  units <- for (Map.toList hug) \ (k, e) -> do
    env <- showHomeUnitEnvShort e
    pure (entryD ((ppr k), env))
  pure (vcat units)

showHug :: HomeUnitGraph -> IO SDoc
showHug (UnitEnvGraph hug) = do
  units <- for (Map.toList hug) \ (k, e) -> do
    env <- showHomeUnitEnv e
    pure (entryD ((ppr k), env))
  pure (vcat units)

showUnitEnv :: UnitEnv -> IO SDoc
showUnitEnv UnitEnv {..} = do
  eps <- showEps =<< eucEPS ue_eps
  hug <- showHug ue_home_unit_graph
  pure $ entries [
    ("eps", eps),
    ("hug", hug),
    ("current_unit", ppr ue_current_unit)
    ]

debugSocketPath :: TargetSpec -> FilePath
debugSocketPath target =
  "/tmp/ghc-persistent-worker/debug-sockets" </> show (fingerprintString (renderTargetSpec target))

#if defined(UNIT_INDEX)

-- | Examine common reasons why an import may not be found by GHC during compilation.
debugLookupModuleHsc :: HscEnv -> ModuleName -> IO ()
debugLookupModuleHsc hsc_env name = do
  dbg ""
  dbg ("# Debugging module lookup for '" ++ moduleNameString name ++ "' in " ++ modeName ++ " mode")
  dbg ""
  case filter msMatch (mgModSummaries hsc_env.hsc_mod_graph) of
    [] -> dbg "* Not in the module graph"
    [summary] -> dbg ("* In module graph for unit " ++ summaryUnit summary)
    mods -> dbg ("* Multiple module graph nodes in units: " ++ intercalate ", " (summaryUnit <$> mods))
  query <- hscUnitIndexQuery hsc_env
  case query.findOrigin (hsc_units hsc_env) name False of
    Nothing -> do
      dbg "* Not in unit index"
      findImportedModule hsc_env name NoPkgQual >>= \case
        Found _ _ ->
          dbg "* Only present in the Finder, likely cached from a call to 'addHomeModuleToFinder' in downsweep"
        NotFound {..}
          | Just unit <- fr_pkg
          -> do
            dbg ("* Finder has it but rejects its unit " ++ showPprUnsafe unit ++ " due to missing interface")
          | not (null fr_pkgs_hidden)
          -> dbg ("* Finder has it in hidden units " ++ showPprUnsafe fr_pkgs_hidden)
          | not (null fr_mods_hidden)
          -> dbg ("* Finder has hidden modules " ++ showPprUnsafe fr_mods_hidden)
        FoundMultiple {} ->
          dbg "* Finder has multiple matching modules"
        NoPackage unit ->
          dbg ("* Finder doesn't know unit " ++ showPprUnsafe unit)
        _ -> do
          dbg "* Not in Finder cache"
          dbg ("! If this happens in a metadata test with a home unit dependency,")
          dbg ("  the Finder cache is likely not being preserved from the first run")
    Just origins -> do
      dbg "* Found"
      for_ (nonDetUniqMapToList origins) \case
        (unit, origin) -> do
          dbg ("  * In unit " ++ showPprUnsafe unit)
          case origin of
            ModOrigin {..} ->
              dbg ("  * " ++ (if fromPackageFlag then "F" else "Not f") ++ "rom package flag")
            _ -> dbg "  * uhhh"
  dbg "---"
  where
    msMatch summary = name == ms_mod_name summary

    summaryUnit ModSummary {ms_mod} = showPprUnsafe (moduleUnit ms_mod)

    modeName = case hsc_env.hsc_dflags.ghcMode of
      CompManager -> "make"
      OneShot -> "oneshot"
      MkDepend -> "metadata"

debugLookupModule :: ModuleName -> Ghc ()
debugLookupModule name = do
  hsc_env <- getSession
  liftIO $ debugLookupModuleHsc hsc_env name

#endif
