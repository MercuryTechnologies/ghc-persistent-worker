module Ghc.Ui.Data.Session where

import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Ghc.Ui.Data.Tasks as Tasks
import Ghc.Ui.Data.Tasks (TasksState)
import Ghc.Ui.Data.WorkerId (WorkerId)
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Network.GRPC.Client (Connection)
import Types.Instrument qualified as Shared

newtype Id =
  Id { text :: Text }
  deriving stock (Eq, Ord, Show)

data Worker =
  Worker {
    workerId :: WorkerId,
    connection :: Connection,
    stats :: Stats
  }
  deriving stock (Generic)

data Stats =
  Stats {
    memory :: Map.Map String Int, -- in bytes
    gc_cpu_ns :: Int,
    cpu_ns :: Int
  }

instance Semigroup Stats where
  l <> r =
    Stats {
      memory = Map.unionWith (+) l.memory r.memory,
      gc_cpu_ns = l.gc_cpu_ns + r.gc_cpu_ns,
      cpu_ns = l.cpu_ns + r.cpu_ns
    }

instance Monoid Stats where
  mempty =
    Stats {
      memory = [],
      gc_cpu_ns = 0,
      cpu_ns = 0
    }

data SessionState =
  SessionState {
    title :: String,
    workers :: Map WorkerId Worker,
    activeTasks :: TasksState,
    modules :: ModuleSelector.State,
    sesStartTime :: UTCTime,
    sesEndTime :: Maybe UTCTime,
    finishedWorkerStats :: Stats
  }
  deriving stock (Generic)

data SessionEvent = InstrEvent WorkerId Shared.Event

initialState :: String -> UTCTime -> SessionState
initialState title startTime =
  SessionState {
    title,
    workers = [],
    activeTasks = Tasks.initialState,
    modules = ModuleSelector.initialState,
    sesStartTime = startTime,
    sesEndTime = Nothing,
    finishedWorkerStats = mempty
  }
