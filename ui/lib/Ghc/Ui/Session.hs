module Ghc.Ui.Session where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (str, vBox, vLimitPercent)
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Ghc.Ui.ActiveTasks qualified as ActiveTasks
import Ghc.Ui.Data.Session (SessionEvent (..), SessionState (..), Stats (..), Worker (..))
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Utils (formatBytes, formatPs, stripEscSeqs)
import Lens.Micro.Platform (each, filtered, modifying, use, zoom)
import Types.Instrument qualified as Instr
import Types.Target (TargetSpec (..))
import Ghc.Ui.Data.WorkerId (WorkerId)

draw :: Name -> UTCTime -> SessionState -> Widget Name
draw current now SessionState {..} =
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

handleEvent :: SessionEvent -> EventM Name SessionState ()
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

removeWorker :: WorkerId -> EventM Name SessionState ()
removeWorker wid = do
  st <- use (#workers . each . filtered (\w -> w.workerId == wid) . #stats)
  modifying #finishedWorkerStats (<> st{memory = mempty})
  modifying #workers (filter (\w -> w.workerId /= wid))
  zoom #modules $ ModuleSelector.removeWorker wid
