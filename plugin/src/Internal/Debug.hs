{-# language OverloadedStrings #-}

module Internal.Debug where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import GHC (DynFlags (..), mi_module)
import GHC.Types.Unique.DFM (udfmToList)
import GHC.Unit (UnitState (..), homeUnitId, moduleEnvToList, unitPackageId)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..), UnitEnvGraph (..))
import GHC.Unit.External (ExternalPackageState (..), eucEPS)
import GHC.Unit.Home.ModInfo (HomePackageTable, hm_iface)
import GHC.Unit.Module.Graph (ModuleGraph, mgTransDeps)
import GHC.Utils.Outputable (SDoc, hang, hcat, ppr, text, vcat, (<+>), Outputable)
import GHC.Types.Unique.Map (nonDetEltsUniqMap)

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

showModGraph :: ModuleGraph -> SDoc
showModGraph g =
  showMap (ppr . toList) (Map.toList (mgTransDeps g))

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

showHpt :: HomePackageTable -> SDoc
showHpt hpt =
  vcat [ppr (mi_module (hm_iface hmi)) | (_, hmi) <- udfmToList hpt]

showHomeUnitEnvShort :: HomeUnitEnv -> SDoc
showHomeUnitEnvShort HomeUnitEnv {..} =
  entries [
    ("deps", ppr homeUnitEnv_units.homeUnitDepends),
    ("hpt", showHpt homeUnitEnv_hpt)
  ]

showHomeUnitEnv :: HomeUnitEnv -> SDoc
showHomeUnitEnv HomeUnitEnv {..} =
  entries [
    ("units", showUnitState homeUnitEnv_units),
    ("homeUnitEnv_unit_dbs", ppr homeUnitEnv_unit_dbs),
    ("dflags", showHomeUnitDflags homeUnitEnv_dflags),
    ("hpt", showHpt homeUnitEnv_hpt),
    ("home_unit", ppr (homeUnitId <$> homeUnitEnv_home_unit))
  ]

showHugShort :: HomeUnitGraph -> SDoc
showHugShort (UnitEnvGraph hug) =
  vcat [entryD ((ppr k), (showHomeUnitEnvShort e)) | (k, e) <- Map.toList hug]

showHug :: HomeUnitGraph -> SDoc
showHug (UnitEnvGraph hug) =
  vcat [entryD ((ppr k), (showHomeUnitEnv e)) | (k, e) <- Map.toList hug]

showUnitEnv :: UnitEnv -> IO SDoc
showUnitEnv UnitEnv {..} = do
  eps <- showEps =<< eucEPS ue_eps
  pure $ entries [
    ("eps", eps),
    ("hug", showHug ue_home_unit_graph),
    ("current_unit", ppr ue_current_unit)
    ]
