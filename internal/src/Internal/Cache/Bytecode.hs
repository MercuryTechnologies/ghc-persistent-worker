-- | LRU tracking and unloading of lazily-loaded bytecode (the @lazyByteCode@ feature, see 'Types.FeatureFlags').
module Internal.Cache.Bytecode where

import Control.Monad (unless)
import Data.Bifunctor (bimap)
import Data.Foldable (foldr', for_, traverse_)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.ByteCode.Types (bc_bcos)
import GHC.Data.FlatBag (sizeFlatBag)
import GHC.Driver.Env (HscEnv, hscInterp)
import GHC.Linker.Loader (unload)
import GHC.Linker.Types (Linkable (..), linkableBCOs, linkableModule)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), UnitEnvGraph (..), unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Types (Module, moduleName, moduleUnitId)
import Types.State.Make (BcoCacheEntry (..), MakeState (..))

-- | Count the number of BCOs in a 'Linkable' as a coarse proxy measure for memory usage.
-- Note: this ignores @LazyBCOs@.
linkableBcoCount :: Linkable -> Int
linkableBcoCount lnk =
  sum (fromIntegral . sizeFlatBag . bc_bcos <$> linkableBCOs lnk)

-- | Increase the access counter and update the LRU cache entries for each 'Linkable'.
touchBcoCache :: [Linkable] -> MakeState -> MakeState
touchBcoCache linkables make =
  make {
    bcoCache = foldr' touch make.bcoCache withBcos,
    bcoAccessCounter
  }
  where
    withBcos = filter (not . null . linkableBCOs) linkables

    touch linkable = Map.insert linkable.linkableModule (newEntry linkable)

    newEntry linkable = BcoCacheEntry {linkable, lastAccess = bcoAccessCounter, size = linkableBcoCount linkable}

    bcoAccessCounter = make.bcoAccessCounter + 1

-- | Clear the bytecode field of a module's HPT entry.
dropFromHpt :: UnitEnvGraph HomeUnitEnv -> Module -> IO ()
dropFromHpt hug target =
  for_ unitEnv \ hue -> do
    lookupHpt (homeUnitEnv_hpt hue) (moduleName target) >>= traverse_ \ hmi ->
      addHomeModInfoToHpt (clear hmi) (homeUnitEnv_hpt hue)
  where
    clear hmi = hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode = Nothing}}

    unitEnv = unitEnv_lookup_maybe (moduleUnitId target) hug

-- | Break the list of cache entries where the total size of BCOs exceeds the given number.
splitEvictions :: Int -> [(Module, BcoCacheEntry)] -> ([(Module, BcoCacheEntry)], [(Module, BcoCacheEntry)])
splitEvictions limit entries =
  bimap (fmap fst) (fmap fst) $
    break (\ (_, size) -> size >= limit) $
    zip entries $
    scanl (+) 0 [entry.size | (_, entry) <- entries]

-- | If the tracked total size of 'bcoCache' exceeds the given limit, unload the least recently used excess entries:
--
-- - Clear the bytecode fields in the HPT for the affected modules
-- - Call 'unload' with the remaining modules (that function expects the kept modules, not the unloaded ones)
evictBcoCache :: HscEnv -> Int -> MakeState -> IO MakeState
evictBcoCache hsc_env limit make = do
  unless (null evicted) do
    traverse_ (dropFromHpt make.hug . fst) evicted
    unload (hscInterp hsc_env) hsc_env (linkable . snd <$> kept)
  pure make {bcoCache = Map.fromList kept}
  where
    (kept, evicted) = splitEvictions limit sorted

    sorted = reverse (List.sortOn (lastAccess . snd) (Map.toList make.bcoCache))
