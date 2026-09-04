module Types.State where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import GHC.Generics (Generic)
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

data Options =
  Options {
    extraGhcOptions :: String
  }
  deriving stock (Generic)

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
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs)
  }
