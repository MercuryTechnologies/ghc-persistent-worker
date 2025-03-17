module Internal.Metadata where

import Data.Int (Int32)
import GHC (DynFlags (..), GhcMode (..), getSessionDynFlags)
import GHC.Driver.Env (hscUpdateFlags)
import GHC.Driver.MakeFile (doMkDependHS)
import GHC.Driver.Monad (modifySession, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import Internal.Log (dbgs)
import Internal.Session (Env (..), runSession, withGhcInSession)

computeMetadata :: Env -> IO (Maybe Int32)
computeMetadata env =
  runSession False env $ withGhcInSession env \ srcs -> do
    initializeSessionPlugins
    dbgs . targetWays_ =<< getSessionDynFlags
    r <- withTempSession id do
      modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
      doMkDependHS (fst <$> srcs)
      pure (Just 0)
    dbgs . targetWays_ =<< getSessionDynFlags
    pure r
