{-# language OverloadedStrings, CPP #-}

module Internal.Debug where

import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import GHC (DynFlags (..), Module, mi_module, moduleName)
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

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

import GHC.Unit.Home.Graph (UnitEnvGraph (..))
import GHC.Unit.Home.PackageTable (HomePackageTable (..), pprHPT)
import GHC.Unit.Module.Graph (ModuleNodeInfo (..))

#else

import GHC.Types.Unique.DFM (udfmToList)
import GHC.Unit.Env (UnitEnvGraph (..))
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomePackageTable, hm_iface)
import GHC.Utils.Outputable (comma, punctuate)

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9,11,0,0) && !defined(MWB)

import Data.Foldable (toList)
import GHC.Unit.Module.Graph (mgTransDeps)

#else

import GHC (ms_mod)
import GHC.Unit.Module.Graph (ModuleGraphNode (..), mgModSummaries')

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

pprModuleFull :: Module -> SDoc
pprModuleFull m =
  ppr (moduleUnitId m) Outputable.<> ":" Outputable.<> ppr (moduleName m)

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  vcat (concatMap showOne (mgModSummaries' g))
  where
    showOne = \case
      ModuleNode deps (ModuleNodeCompile ms) -> [hang (pprModuleFull (ms_mod ms) <+> "->") 2 (vcat (ppr <$> deps))]
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
      ModuleNode deps ms -> hang (pprModuleFull (ms_mod ms) <+> "->") 2 (vcat (ppr <$> deps))
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
showHomeUnitDflags DynFlags {..} =
  entries [
    ("homeUnitId", ppr homeUnitId_)
  ]

#if MIN_VERSION_GLASGOW_HASKELL(9,11,0,0)

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
  text unitDatabasePath

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
