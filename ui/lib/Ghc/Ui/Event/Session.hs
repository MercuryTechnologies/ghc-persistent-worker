module Ghc.Ui.Event.Session where

import Brick.Types (EventM)
import Control.Lens ((%%=), (<>=))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Session (SessionEvent (..), SessionState (..), Stats (..), Worker (..))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Ghc.Ui.Event.Tasks qualified as Tasks
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Lens.Micro.Platform (each, filtered, modifying, zoom)
import Types.Api qualified as Api

stripEscSeqs :: String -> String
stripEscSeqs [] = []
stripEscSeqs ('\ESC' : '[' : xs) = stripEscSeqs (drop 1 (dropWhile (/= 'm') xs))
stripEscSeqs (x : xs) = x : stripEscSeqs xs

handleEvent :: SessionEvent -> EventM Name SessionState ()
handleEvent (InstrEvent wid evt) =
  case evt of
    Api.CompileStart {..} -> do
      zoom #activeTasks $ Tasks.addTask target requestId wid debuggable
    Api.CompileEnd {..} -> do
      let content = stripEscSeqs stderr
      if exitCode == 0
      then do
        start <- zoom #activeTasks $ Tasks.removeTask target requestId
        end <- liftIO getCurrentTime
        let time = nominalDiffTimeToSeconds . diffUTCTime end <$> start
        zoom #modules $ ModuleSelector.addModule target content time wid
      else do
        zoom #activeTasks $ Tasks.taskFailure target requestId content
    Api.Stats {..} -> do
      modifying (#workers . each . filtered (\w -> w.workerId == wid) . #stats) \ st ->
        st {
          memory = Map.mapKeys Text.unpack memory,
          gc_cpu_ns = gcCpuNs,
          cpu_ns = cpuNs
        }
handleEvent (RebuildFailed wid target message) = do
  zoom #modules $ ModuleSelector.enableModule wid target
  zoom #activeTasks $ Tasks.addFailedRebuild target wid ("Rebuild failed: " ++ Text.unpack message)

removeWorker :: WorkerId -> EventM Name SessionState ()
removeWorker target = do
  removed <- #workers %%= Map.updateLookupWithKey (\ _ _ -> Nothing) target
  for_ removed \ worker ->
    #finishedWorkerStats <>= worker.stats {memory = mempty}
  zoom #modules $ ModuleSelector.removeWorker target
