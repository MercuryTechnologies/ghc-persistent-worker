{-# LANGUAGE CPP, NoFieldSelectors #-}

module Internal.State where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import GHC (Ghc, emptyMG, HscEnv)
import GHC.Driver.Monad (modifySessionM, withSession)
import GHC.Unit.Home.Graph (unitEnv_new)
import Internal.Debug (showHugShort, showModGraph)
import qualified Internal.State.Make as Make
import Internal.State.UnitIndex (newUnitIndex)
import System.Environment (lookupEnv)
import System.OsPath.Extra (toOsPath)
import Types.Log (Logger (..))
import Types.State (BinPath (..), WorkerState (..), defaultOptions)
import Types.State.Make (MakeState (..))

newState :: IO (MVar WorkerState)
newState = do
  initialPath <- lookupEnv "PATH"
  unitIndex <- newUnitIndex
  let bcoLoadState = M.empty
  newMVar WorkerState {
    path = BinPath {
      initial = toOsPath <$> initialPath,
      extra = mempty
    },
    baseSession = Nothing,
    options = defaultOptions,
    make = MakeState {
      moduleGraph = emptyMG,
      hug = unitEnv_new mempty,
      interp = Nothing,
      unitIndex,
      bcoLoadState
    },
    targetArgs = mempty
  }

modifyMakeState :: MVar WorkerState -> (MakeState -> IO (MakeState, a)) -> IO a
modifyMakeState var f =
  modifyMVar var \ state -> do
    (make, a) <- f state.make
    pure (state {make}, a)

-- | Update the 'MakeState' field in the 'WorkerState'.
updateMakeState :: (MakeState -> MakeState) -> WorkerState -> WorkerState
updateMakeState f state = state {make = f state.make}

updateMakeStateVar :: MVar WorkerState -> (MakeState -> MakeState) -> IO ()
updateMakeStateVar var f = modifyMakeState var (\ s -> pure (f s, ()))

-- | Restore the HUG, module graph and interpreter state from the worker state, since those are the only two components
-- modified by the worker that aren't already shared by the base session.
withState ::
  Logger ->
  MVar WorkerState ->
  ((WorkerState, HscEnv) -> IO (WorkerState, HscEnv)) ->
  Ghc a ->
  Ghc a
withState logger stateVar setup prog = do
  modifySessionM restore
  prog <* withSession store
  where
    restore hsc_env =
      liftIO $ modifyMVar stateVar \ state -> do
        let (make, hsc_env1) = Make.loadStateCompile hsc_env state.make
        setup (state {make}, hsc_env1)

    store hsc_env =
      liftIO $ modifyMVar_ stateVar \ state -> do
        make <- Make.storeState logger hsc_env state.make
        pure state {make}

dumpState ::
  Logger ->
  MVar WorkerState ->
  Maybe String ->
  IO ()
dumpState logger state exception =
  withMVar state \ WorkerState {make = MakeState {moduleGraph, hug}} -> do
    write "-----------------"
    write "Request failed!"
    traverse_ write exception
    write "-----------------"
    write "Module graph:"
    writeD (showModGraph moduleGraph)
    write "-----------------"
    write "Home unit graph:"
    writeD =<< showHugShort hug
  where
    write = logger.debug
    writeD = logger.debugD
