module Ghc.Ui.Event.Session where

import Brick.Types (EventM)
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Session (SessionEvent (..), SessionState (..), Stats (..), Worker (..))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Ghc.Ui.Event.Tasks qualified as Tasks
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Utils (stripEscSeqs)
import Lens.Micro.Platform (each, filtered, modifying, use, zoom)
import Types.Instrument qualified as Instr
import Types.Target (TargetSpec (..))

handleEvent :: SessionEvent -> EventM Name SessionState ()
handleEvent (InstrEvent wid evt) =
  case evt of
    Instr.CompileStart {..} -> do
      zoom #activeTasks $ Tasks.addTask (TargetUnknown target) wid canDebug
    Instr.CompileEnd {..} -> do
      let content = stripEscSeqs stderr
          target' = TargetUnknown $ if target == "" then takeWhile (/= ':') content else target
      if exitCode == 0
      then do
        start <- zoom #activeTasks $ Tasks.removeTask target'
        end <- liftIO getCurrentTime
        let time = nominalDiffTimeToSeconds . diffUTCTime end <$> start
        zoom #modules $ ModuleSelector.addModule target' content time wid
      else do
        zoom #activeTasks $ Tasks.taskFailure target' content
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
