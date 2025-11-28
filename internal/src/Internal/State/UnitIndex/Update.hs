{-# LANGUAGE CPP #-}

module Internal.State.UnitIndex.Update where

#if defined(UNIT_INDEX)

import Data.List.NonEmpty (NonEmpty)
import GHC hiding (SuccessFlag (..))
import GHC.Generics (Generic)
import GHC.Types.Unique (Uniquable)
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set (UniqSet, elementOfUniqSet, mkUniqSet, unionUniqSets)
import GHC.Unit.Database
import GHC.Unit.Module
import GHC.Unit.State
import GHC.Utils.Logger (LogFlags (..), logFlags)
import GHC.Utils.Outputable (Outputable, ppr, renderWithContext, text, ($$), (<+>))
import GHC.Utils.Panic (pprPanic, throwGhcException)

type UnusableUnits = UniqMap UnitId (UnitInfo, UnusableUnitReason)

-- | Stripped down version of 'ModuleOrigin' that only concerns home unit independent properties:
--
-- - Whether the module is hidden by being listed in @other-modules@
-- - Whether the module is reexported by other units
data Provider =
  Provider {
    hidden :: Bool,
    reexports :: Maybe (NonEmpty UnitInfo)
  }
  deriving stock (Generic)

instance Outputable Provider where
  ppr Provider {..} = if hidden then text "hidden" else text "available"

instance Semigroup Provider where
  l <> r =
    if l.hidden == r.hidden
    then Provider {hidden = l.hidden, reexports = l.reexports <> r.reexports}
    else pprPanic "Provider: package both exposed/hidden" $ text "l:" <+> ppr l.hidden $$ text "r:" <+> ppr r.hidden

instance Monoid Provider where
  mempty = Provider {hidden = False, reexports = Nothing}

type Providers = UniqMap ModuleName (UniqMap Module Provider)

mkProviderMap :: Unit -> ModuleName -> Provider -> UniqMap Module Provider
mkProviderMap pkg pro = unitUniqMap (mkModule pkg pro)

-- | Add a list of key/value pairs to a nested map.
--
-- The outer map is processed with 'Data.Map.Strict' to prevent memory leaks
-- when reloading modules in GHCi (see #4029). This ensures that each
-- value is forced before installing into the map.
addListTo :: (Monoid a, Uniquable k1)
          => UniqMap k1 (UniqMap k2 a)
          -> [(k1, UniqMap k2 a)]
          -> UniqMap k1 (UniqMap k2 a)
addListTo = foldl' merge
  where merge m (k, v) = addToUniqMap_C (plusUniqMap_C mappend) m k v

addProvider :: UnitInfo -> Providers -> Providers
addProvider info@GenericUnitInfo {unitExposedModules, unitHiddenModules} z =
  addListTo z ((export <$> unitExposedModules) ++ hiddens)
  where
    export (name, exposedReexport) =
     let (originUnit, originName, reexports) = checkReexport name exposedReexport
     in (name, mkProviderMap originUnit originName Provider {hidden = False, reexports})

    checkReexport name = \case
      Nothing -> (unit, name, Nothing)
      Just (Module originUnit originName) -> (originUnit, originName, Just (pure info))

    hiddens = [(name, mkProviderMap unit name Provider {hidden = True, reexports = Nothing}) | name <- unitHiddenModules]

    unit = mkUnit info

mkUnitInfoMap :: [UnitInfo] -> UnitInfoMap
mkUnitInfoMap infos = foldl' add emptyUniqMap infos
  where
   mkVirt      p = virtualUnitId (mkInstantiatedUnit (unitInstanceOf p) (unitInstantiations p))
   add pkg_map p
      | not (null (unitInstantiations p))
      = addToUniqMap (addToUniqMap pkg_map (mkVirt p) p)
                     (unitId p) p
      | otherwise
      = addToUniqMap pkg_map (unitId p) p

-- | Add new provider entries for each unit that hasn't been processed before.
updateProviders ::
  Logger ->
  UnitConfig ->
  UnitInfoMap ->
  Providers ->
  UniqSet UnitId ->
  IO (Providers, UniqSet UnitId)
updateProviders _ _ infos old oldUnits =
  pure (new, newUnits)
  where
    new = nonDetFoldUniqMap (addProvider . snd) old newGlobalUnits

    newUnits = unionUniqSets (mkUniqSet (nonDetKeysUniqMap newGlobalUnits)) oldUnits

    newGlobalUnits = mkUnitInfoMap (nonDetEltsUniqMap (filterWithKeyUniqMap (const . isNewUnit) infos))

    isNewUnit uid = not (elementOfUniqSet uid oldUnits)

invalidRenamedModule ::
  Logger ->
  ModuleName ->
  Unit ->
  GhcException
invalidRenamedModule logger orig pk =
  (CmdLineError (renderWithContext (log_default_user_context (logFlags logger)) message))
  where
    message = text "package flag: could not find module name" <+> ppr orig <+> text "in package" <+> ppr pk

renamingOrigin :: ModuleOrigin
renamingOrigin =
  ModOrigin {
    fromOrigUnit = Nothing,
    fromExposedReexport = [],
    fromHiddenReexport = [],
    fromPackageFlag = True
  }

-- | Add module name providers for each unit that has renamings configured by @-package@ flags.
unitOverrides ::
  Logger ->
  VisibilityMap ->
  ModuleNameProvidersMap ->
  Providers ->
  ModuleNameProvidersMap
unitOverrides logger vis_map unusable providers =
  renamed <> unusable
  where
    renamed = nonDetFoldUniqMap unitRenamings emptyUniqMap vis_map

    unitRenamings (unit, UnitVisibility {uv_renamings}) z =
      addListTo z (moduleRenaming unit <$> uv_renamings)

    moduleRenaming unit (orig, new) =
      case lookupUniqMap providers orig of
        Just r -> (new, renamingOrigin <$ r)
        Nothing -> throwGhcException (invalidRenamedModule logger orig unit)

#endif
