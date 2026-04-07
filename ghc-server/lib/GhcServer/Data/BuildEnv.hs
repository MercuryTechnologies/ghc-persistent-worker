-- | Bundled arguments that flow through the build pipeline.
module GhcServer.Data.BuildEnv where

import Control.Concurrent.MVar (MVar)
import GhcServer.Data.BuildEvent (BuildEvents)
import GhcServer.Data.Unit (Project)
import System.OsPath (OsPath)
import Types.Args (Args)
import Types.Log (Logger)
import Types.State (WorkerState)

-- | Common arguments threaded from 'runBuild' through dispatch to worker adapters.
data BuildEnv =
  BuildEnv {
    baseArgs :: Args,
    projectRoot :: OsPath,
    outputDir :: OsPath,
    tmpDir :: OsPath,
    stateVar :: MVar WorkerState,
    project :: Project,
    log :: Logger,
    events :: BuildEvents
  }
