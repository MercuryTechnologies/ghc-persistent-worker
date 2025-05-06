module Internal.Metadata where

import Control.Concurrent (modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import GHC (DynFlags (..), Ghc, GhcMode (..))
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Monad (modifySession, modifySessionM, withTempSession, withSession)
import GHC.Platform.Ways (Way (WayDyn), addWay)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit.Env (UnitEnv (..), unitEnv_union)
import Internal.Cache (Cache (..), Target (..), insertUnitEnv, mergeHugs, newFinderCache, updateModuleGraph)
import Internal.MakeFile (doMkDependHS)
import Internal.Session (Env (..), runSession, withGhcInSession)

-- | Copy the cached unit env and module graph to the given session.
restoreEnv :: Cache -> HscEnv -> HscEnv
restoreEnv cache hsc_env = do
  maybe id restoreMg cache.moduleGraph $ maybe hsc_env restore cache.hug
  where
    restoreMg new e = e {hsc_mod_graph = new}

    restore hug =
      hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_home_unit_graph = unitEnv_union mergeHugs hug current}}

    current = hsc_env.hsc_unit_env.ue_home_unit_graph

-- | 'doMkDependHS' needs this to be enabled.
addDynWay :: HscEnv -> HscEnv
addDynWay =
  hscUpdateFlags \ d -> d { targetWays_ = addWay WayDyn (targetWays_ d) }

-- | Run downsweep and merge the resulting module graph into the cached graph.
-- This is executed for the metadata step, which natively only calls 'doMkDependHS'.
-- Since that function doesn't give us access to the module graph in its original shape, we inline it into this project
-- to exfiltrate the graph.
-- This has @WayDyn@ hardcoded for now, but it should be adapted to Buck's build configuration.
-- This is usually not necessary (in fact, 'doMkDependHS' clears the target ways) but since we're keeping the module
-- graph the target way will be reflected in the stored @ModSummary@ nodes.
--
-- Before downsweep, we also create a fresh @Finder@ for some reason and restore the previous unit env so dependencies
-- are visible.
computeMetadataInSession :: Ghc () -> Env -> [String] -> Ghc (Maybe Bool)
computeMetadataInSession setup env srcs = do
  initializeSessionPlugins
  cache <- liftIO $ readMVar env.cache
  modifySession (restoreEnv cache)
  setup
  withSession \ hsc_env ->
    liftIO $ modifyMVar_ env.cache $ (pure . insertUnitEnv hsc_env)
  module_graph <- withTempSession addDynWay do
    modifySessionM \ hsc_env -> do
      hsc_FC <- liftIO $ newFinderCache env.cache cache (Target "metadata")
      pure hsc_env {hsc_FC}
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
    doMkDependHS srcs
  liftIO $ updateModuleGraph env.cache module_graph
  pure (Just True)

-- | Run 'computeMetadataInSession' without extra session initialization code, which is just used in tests.
computeMetadata :: Env -> IO Bool
computeMetadata env =
  fmap (fromMaybe False) $ runSession False env $ withGhcInSession env \ srcs ->
    computeMetadataInSession (pure ()) env (fst <$> srcs)
