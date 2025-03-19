module Internal.Metadata where

import Control.Concurrent (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import GHC (DynFlags (..), GhcMode (..), GhcMonad (..))
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Monad (modifySession, modifySessionM, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit.Env (UnitEnv (..), unitEnv_union)
import Internal.Cache (Cache (..), mergeHugs, updateModuleGraph)
import Internal.Debug (showHugShort, showModGraph)
import Internal.Log (dbg, dbgp)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (Env (..), runSession, withGhcInSession)
import Control.Monad (when)

restoreHug :: Env -> HscEnv -> IO HscEnv
restoreHug env hsc_env = do
  cache <- readMVar env.cache
  pure $ maybe hsc_env restore cache.hug
  where
    restore hug =
      hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_home_unit_graph = unitEnv_union mergeHugs hug current}}

    current = hsc_env.hsc_unit_env.ue_home_unit_graph

computeMetadata :: Env -> IO (Maybe Int32)
computeMetadata env =
  runSession False env $ withGhcInSession env \ srcs -> do
    initializeSessionPlugins
    module_graph <- withTempSession id do
      when False do
        modifySessionM (liftIO . restoreHug env)
      dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
      modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
      module_graph <- doMkDependHS (fst <$> srcs)
      dbgp (showModGraph module_graph)
      dbg "-----"
      dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
      pure module_graph
    liftIO $ updateModuleGraph env.cache module_graph
    pure (Just 0)
