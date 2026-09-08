module Types.State where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import System.OsPath (OsPath)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.State.Make (MakeState (..))
import Types.Target (TargetSpec)

data BinPath =
  BinPath {
    initial :: Maybe OsPath,
    extra :: Set OsPath
  }
  deriving stock (Eq, Show)

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    make :: MakeState,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs)
  }
