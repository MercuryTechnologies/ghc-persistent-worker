module Types.State where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.State.Make (MakeState (..))
import Types.State.Oneshot (OneshotState)
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
