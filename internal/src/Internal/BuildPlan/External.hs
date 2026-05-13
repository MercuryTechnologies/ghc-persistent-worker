module Internal.BuildPlan.External where

import Control.Monad (join)
import Data.Functor ((<&>))
import Data.Map (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable (mapAccumM)
import GHC (GenLocated (..), Located, PkgQual)
import GHC.Data.Maybe (rightToMaybe)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Errors.Types (DriverMessage (..))
import GHC.Iface.Errors (cannotFindModule)
import GHC.Iface.Errors.Types (IfaceMessage (Can'tFindInterface), InterfaceLookingFor (LookingForModule))
import GHC.Types.Error (Messages, singleMessage)
import GHC.Types.SrcLoc (SrcSpan (..))
import GHC.Types.Unique.Map (UniqMap, lookupUniqMap)
import GHC.Unit (Definite (..), GenModule (..), GenUnit (..))
import GHC.Unit.Finder (FindResult (..), findImportedModule)
import GHC.Unit.Module (ModuleName (..), UnitId (..), unitIdString)
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Utils.Error (mkPlainErrorMsgEnvelope)
import Language.Haskell.Syntax.ImpExp (IsBootInterface (NotBoot))
import Types.BuildPlan (BuildPlanEnv (..), ModuleKey (..), PackageKey (..), summaryModuleKey)

-- | A lookup table for external modules shared across units.
type ExternalCache = Map (PkgQual, ModuleName) (Maybe (UnitId, ModuleName))

packageName :: UniqMap UnitId PackageKey -> UnitId -> PackageKey
packageName unitNames unit =
  fromMaybe (PackageKey (unitIdString unit)) (lookupUniqMap unitNames unit)

-- | Call the Finder to resolve an import.
-- Return @'Just' 'Nothing'@ if the module is in the current home unit.
lookupImport ::
  HscEnv ->
  Set UnitId ->
  PkgQual ->
  ModuleName ->
  SrcSpan ->
  IO (Either (Messages DriverMessage) (Maybe (UnitId, ModuleName)))
lookupImport hsc_env homeUnitIds qual name loc =
  findImportedModule hsc_env name qual <&> \case
    Found _ Module {moduleName, moduleUnit = RealUnit (Definite unit)}
      | Set.member unit homeUnitIds
      -> Right Nothing
      | otherwise
      -> Right (Just (unit, moduleName))
    fail_detail ->
      Left $
        singleMessage $
          mkPlainErrorMsgEnvelope loc $
            DriverInterfaceError $
              Can'tFindInterface (cannotFindModule hsc_env name fail_detail) $
                LookingForModule name NotBoot -- boot module is not supported here yet. TODO: support boot

moduleImport ::
  BuildPlanEnv ->
  ExternalCache ->
  (PkgQual, Located ModuleName) ->
  IO (ExternalCache, Maybe (UnitId, ModuleName))
moduleImport BuildPlanEnv {hsc_env, homeUnitIds} cache (qual, L loc name) =
  maybe cacheMiss cacheHit (cache !? (qual, name))
  where
    -- If the import is cached, use it for the deps directly.
    cacheHit result = pure (cache, result)

    -- If the import isn't cached, look it up and add it to the cache and deps.
    -- If it can't be found, return 'Nothing'.
    -- We don't want to validate imports, since that already happened during downsweep.
    -- If the imported module is part of the home unit, store and return 'Nothing' to avoid further lookups.
    cacheMiss = fmap (join . rightToMaybe) . traverse found <$> lookupImport hsc_env homeUnitIds qual name loc

    found result = (Map.insert (qual, name) result cache, result)

-- | Resolve all imports in the given module and return only those that don't refer to modules in the current home unit.
-- Accumulate a lookup cache to improve performance.
moduleImports ::
  BuildPlanEnv ->
  ExternalCache ->
  ModSummary ->
  IO (ExternalCache, (ModuleKey, Map UnitId (PackageKey, [ModuleName])))
moduleImports env cache0 summary = do
  (cache, ext) <- mapAccumM (moduleImport env) cache0 summary.ms_textual_imps
  pure (cache, (summaryModuleKey summary, byUnit ext))
  where
    byUnit ext =
      Map.mapWithKey withUnitName $
      Map.fromListWith (++) (fmap pure <$> catMaybes ext)

    withUnitName unit modules = (packageName env.unitNames unit, modules)

-- | Resolve all imports in all modules of the given unit.
-- Accumulate a lookup cache to improve performance.
unitImports ::
  BuildPlanEnv ->
  [ModSummary] ->
  IO (Map ModuleKey (Map UnitId (PackageKey, [ModuleName])))
unitImports env summaries = do
  (_, ext) <- mapAccumM (moduleImports env) mempty summaries
  pure (Map.fromList ext)
