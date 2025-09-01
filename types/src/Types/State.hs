module Types.State where

import Control.Concurrent (MVar, newMVar)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv, emptyMG)
import GHC.Unit.Env (unitEnv_new)
import System.Environment (lookupEnv)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.State.Make (MakeState (..))
import Types.State.Oneshot (OneshotCacheFeatures (..), OneshotState, newOneshotCacheFeatures, newOneshotStateWith)
import Types.Target (TargetSpec)

data BinPath =
  BinPath {
    initial :: Maybe String,
    extra :: Set String
  }
  deriving stock (Eq, Show)

data Options =
  Options {
    extraGhcOptions :: String
  }

defaultOptions :: Options
defaultOptions =
  Options {
    extraGhcOptions = ""
  }

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    options :: Options,
    make :: MakeState,
    oneshot :: OneshotState,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs)
  }

newStateWith :: OneshotCacheFeatures -> IO (MVar WorkerState)
newStateWith features = do
  initialPath <- lookupEnv "PATH"
  oneshot <- newOneshotStateWith features
  newMVar WorkerState {
    path = BinPath {
      initial = initialPath,
      extra = mempty
    },
    baseSession = Nothing,
    options = defaultOptions,
    make = MakeState {
      moduleGraph = emptyMG,
      hug = unitEnv_new mempty,
      interp = Nothing
    },
    oneshot,
    targetArgs = mempty
  }

newState :: Bool -> IO (MVar WorkerState)
newState enable = newStateWith newOneshotCacheFeatures {enable}
