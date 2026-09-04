module Ghc.Ui.Data.Session where

import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Ghc.Ui.Data.Tasks as Tasks
import Ghc.Ui.Data.Tasks (TasksState)
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Types (WorkerId)
import Network.GRPC.Client (Connection)
import Types.Instrument qualified as Shared

newtype Id =
  Id { unId :: Text }
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
  Stats m1 gc1 cpu1 <> Stats m2 gc2 cpu2 =
    Stats (Map.unionWith (+) m1 m2) (gc1 + gc2) (cpu1 + cpu2)

instance Monoid Stats where
  mempty = Stats mempty 0 0

data SessionState =
  SessionState {
    title :: String,
    workers :: [Worker],
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
