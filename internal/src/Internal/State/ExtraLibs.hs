-- | Extra native libraries of home units, the @-l@ options of their flags.
--
-- The worker records them per unit when it adds the unit
-- ('recordExtraLibs'), and loads those of a set of units into the
-- interpreter ('loadUnitLibraries'), each library at most once for the
-- whole worker.
module Internal.State.ExtraLibs (
  recordExtraLibs,
  loadUnitLibraries,
) where

import Control.Concurrent (MVar)
import Control.Monad (foldM)
import Data.Foldable (for_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC.Driver.DynFlags (DynFlags (..), targetPlatform)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Platform (platformSOName)
import GHC.Runtime.Interpreter (Interp, loadDLL)
import GHC.Unit (UnitId)
import GHC.Utils.CliOption (Option (..))
import GHC.Utils.Outputable (text, (<+>))
import Internal.Error (workerErrorIO)
import Internal.State (modifyMakeState)
import System.Directory (findFile)
import Types.State (WorkerState)
import Types.State.Make (LibLoadState (..), MakeState (..))

-- | Record in the 'MakeState' that @unit@ requires the libraries specified in
-- @ldInputs dflags@.
--
-- The records are then used in 'loadUnitLibraries'.
--
-- TODO: this ad hoc extraction of extra library dependencies should be
-- replaced by a proper specification from the build system rules, recorded
-- in a file, preferably the build plan file.
recordExtraLibs :: UnitId -> DynFlags -> MakeState -> MakeState
recordExtraLibs unit dflags state =
  state {extraLib = state.extraLib {requested}}
  where
    requested = M.insert unit (dflags.libraryPaths, libs) state.extraLib.requested
    libs = [lib | Option ('-' : 'l' : lib) <- dflags.ldInputs]

-- | Load the recorded libraries of the given units, in order, each library
-- at most once for the whole worker.
loadUnitLibraries :: MVar WorkerState -> HscEnv -> Interp -> [UnitId] -> IO ()
loadUnitLibraries stateVar hsc_env interp units =
  for_ units \ unit ->
    modifyMakeState stateVar \ make ->
      case M.lookup unit make.extraLib.requested of
        Nothing -> pure (make, ())
        Just (lib_paths, libs) -> do
          loaded <- foldM (load lib_paths) make.extraLib.loaded libs
          pure (make {extraLib = make.extraLib {loaded}}, ())
  where
    load lib_paths loaded lib
      | lib `S.member` loaded = pure loaded
      | otherwise = S.insert lib loaded <$ loadLibrary hsc_env interp lib_paths lib

-- | Loads the first shared library with name @lib@ that is found
-- in any of the directories in @lib_paths@. Fails if no such library
-- is found.
loadLibrary :: HscEnv -> Interp -> [FilePath] -> String -> IO ()
loadLibrary hsc_env interp lib_paths lib = do
  findFile lib_paths so_name >>= \case
    Nothing -> emitError ("Library not found: " <+> text so_name)
    Just so_file ->
      loadDLL interp so_file >>= \case
        Left err -> emitError (text err)
        Right _ -> pure ()
  where
    so_name = platformSOName (targetPlatform hsc_env.hsc_dflags) lib

    emitError msg = workerErrorIO hsc_env ("Loading DLL error:" <+> msg)
