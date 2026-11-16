-- | Extra native libraries of home units, the @-l@ options of their flags.
module Internal.State.ExtraLibs (
  updateExtraLibs,
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

-- TODO: this ad hoc extraction of extra library dependency should be replaced
-- by proper specification from the build system rules and recorded in a file,
-- preferrably to buildplan file.
updateExtraLibs :: UnitId -> DynFlags -> MakeState -> MakeState
updateExtraLibs unit dflags state =
  let lib_paths = libraryPaths dflags
      libs = [ lib | Option ('-':'l':lib) <- ldInputs dflags ]
      requested' = M.insert unit (lib_paths, libs) state.extraLib.requested
   in state {extraLib = LibLoadState requested' state.extraLib.loaded}

loadUnitLibraries :: MVar WorkerState -> HscEnv -> Interp -> [UnitId] -> IO ()
loadUnitLibraries stateVar hsc_env interp unitIds =
  for_ unitIds \unit_id ->
    modifyMakeState stateVar \make -> do
      let m = make.extraLib.requested
      case M.lookup unit_id m of
        Nothing -> pure (make, ())
        Just (lib_paths, libs) -> do
          let load loaded lib
                | lib `S.member` loaded = pure loaded
                | otherwise = loadDLL_ hsc_env interp lib_paths lib >> pure (S.insert lib loaded)
          loaded' <- foldM load make.extraLib.loaded libs
          pure (make {extraLib = LibLoadState m loaded'}, ())

loadDLL_ :: HscEnv -> Interp -> [FilePath] -> String -> IO ()
loadDLL_ hsc_env interp lib_paths lib = do
  let dflags = hsc_dflags hsc_env
      platform = targetPlatform dflags
      so_name = platformSOName platform lib
  mb_so_file  <- findFile lib_paths so_name
  case mb_so_file of
    Nothing -> emitError ("Library not found: " <+> text so_name)
    Just so_file -> do
      e <- loadDLL interp so_file
      case e of
        Left err -> emitError (text err)
        Right _ -> pure ()
  where
    emitError msg =
      workerErrorIO hsc_env ("Loading DLL error:" <+> msg)
