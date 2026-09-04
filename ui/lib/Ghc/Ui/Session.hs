module Ghc.Ui.Session where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (str, vBox, vLimitPercent)
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import GHC.Generics (Generic)
import Ghc.Ui.ActiveTasks qualified as ActiveTasks
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Types (Name, WorkerId)
import Ghc.Ui.Utils (formatBytes, formatPs, stripEscSeqs)
import Lens.Micro.Platform (each, filtered, modifying, use, zoom)
import Network.GRPC.Client (Connection)
import Types.Instrument qualified as Instr
import Types.Target (TargetSpec (..))

newtype Id = Id {unId :: Text.Text}
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

data State =
  Session {
    title :: String,
    workers :: [Worker],
    activeTasks :: ActiveTasks.State,
    modules :: ModuleSelector.State,
    sesStartTime :: UTCTime,
    sesEndTime :: Maybe UTCTime,
    finishedWorkerStats :: Stats
  }
  deriving stock (Generic)

data Event = InstrEvent WorkerId Instr.Event

mkSession :: String -> UTCTime -> State
mkSession title startTime =
  Session {
    title,
    workers = [],
    activeTasks = ActiveTasks.initialState,
    modules = ModuleSelector.initialState,
    sesStartTime = startTime,
    sesEndTime = Nothing,
    finishedWorkerStats = mempty
  }

draw :: Name -> UTCTime -> State -> Widget Name
draw current now Session {..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ title ++ " ") $
    vBox
      [ vLimitPercent 30 $ ActiveTasks.draw current now activeTasks
      , hBorder
      , ModuleSelector.draw current modules
      , hBorder
      , drawStats (length workers) (foldMap (.stats) workers <> finishedWorkerStats)
      ]

drawStats :: Int -> Stats -> Widget Name
drawStats workerCount Stats{..} =
  vBox
    [ str $
        " Worker count: "
          ++ show workerCount
          ++ " | Memory:"
          ++ concatMap
            (\(k, v) -> " " ++ k ++ "=" ++ formatBytes v)
            (Map.toList memory)
    , str $
        " CPU Time: "
          ++ formatPs (1000 * cpu_ns)
          ++ " | GC Time: "
          ++ formatPs (1000 * gc_cpu_ns)
    ]

handleEvent :: Event -> EventM Name State ()
handleEvent (InstrEvent wid evt) =
  case evt of
    Instr.CompileStart {..} -> do
      zoom #activeTasks $ ActiveTasks.addTask (TargetUnknown target) wid canDebug
    Instr.CompileEnd {..} -> do
      let content = stripEscSeqs stderr
          target' = TargetUnknown $ if target == "" then takeWhile (/= ':') content else target
      if exitCode == 0
      then do
        start <- zoom #activeTasks $ ActiveTasks.removeTask target'
        end <- liftIO getCurrentTime
        let time = nominalDiffTimeToSeconds . diffUTCTime end <$> start
        zoom #modules $ ModuleSelector.addModule target' content time wid
      else do
        zoom #activeTasks $ ActiveTasks.taskFailure target' content
    Instr.Stats {..} -> do
      modifying (#workers . each . filtered (\w -> w.workerId == wid) . #stats) \ st ->
        st {
          memory,
          gc_cpu_ns = gcCpuNs,
          cpu_ns = cpuNs
        }
    Instr.Halt -> pure ()

removeWorker :: WorkerId -> EventM Name State ()
removeWorker wid = do
  st <- use (#workers . each . filtered (\w -> w.workerId == wid) . #stats)
  modifying #finishedWorkerStats (<> st{memory = mempty})
  modifying #workers (filter (\w -> w.workerId /= wid))
  zoom #modules $ ModuleSelector.removeWorker wid
