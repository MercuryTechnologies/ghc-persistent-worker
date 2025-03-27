module Internal.Metadata where

import Control.Concurrent (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import GHC (DynFlags (..), GhcMode (..), GhcMonad (..))
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Monad (modifySession, withTempSession, modifySessionM)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit.Env (UnitEnv (..), unitEnv_union)
import Internal.Cache (Cache (..), mergeHugs, updateModuleGraph, newFinderCache, Target (..))
import Internal.Debug (showHugShort, showModGraph)
import Internal.Log (dbg, dbgp)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (Env (..), runSession, withGhcInSession)
import GHC.Unit.Module.Graph (unionMG)
import GHC.Platform.Ways (addWay, Way (WayDyn))

restoreEnv :: Env -> HscEnv -> IO HscEnv
restoreEnv env hsc_env = do
  cache <- readMVar env.cache
  pure $ maybe id restoreMg cache.moduleGraph $ maybe hsc_env restore cache.hug
  where
    restoreMg new e = e {hsc_mod_graph = unionMG e.hsc_mod_graph new}

    restore hug =
      hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_home_unit_graph = unitEnv_union mergeHugs hug current}}

    current = hsc_env.hsc_unit_env.ue_home_unit_graph

computeMetadata :: Env -> IO Bool
computeMetadata env =
  fmap (fromMaybe False) $ runSession False env $ withGhcInSession env \ srcs -> do
    initializeSessionPlugins
    cache <- liftIO $ readMVar env.cache
    module_graph <- withTempSession (hscUpdateFlags (\ d -> d { targetWays_ = addWay WayDyn (targetWays_ d) })) do
      modifySessionM (liftIO . restoreEnv env)
      modifySessionM \ hsc_env -> do
        hsc_FC <- liftIO $ newFinderCache env.cache cache (Target "metadata")
        pure hsc_env {hsc_FC}
      dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
      modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
      module_graph <- doMkDependHS (fst <$> srcs)
      dbgp (showModGraph module_graph)
      dbg "-----"
      dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
      pure module_graph
    liftIO $ updateModuleGraph env.cache module_graph
    pure (Just True)
