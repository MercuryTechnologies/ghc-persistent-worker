module Ghc.Ui.Event.Tasks where

import Brick.Types (EventM)
import Brick.Widgets.List (listElementsL, listSelectedElementL, listSelectedL)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, getCurrentTime)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Tasks (Task (..), TasksState)
import Ghc.Ui.Data.WorkerId (WorkerId)
import Lens.Micro.Platform (modifying, preuse, use, (.=), (<&>))
import Types.Target (TargetSpec (..))

addTask :: TargetSpec -> WorkerId -> Bool -> EventM Name TasksState ()
addTask target worker debuggable = do
  startTime <- liftIO $ getCurrentTime
  tasks <- use listElementsL
  let i = if debuggable then 0 else fromMaybe 0 (Seq.findIndexL (not . (.debuggable)) tasks)
  listElementsL .= Seq.insertAt i (Task {target, startTime, failure = Nothing, worker, debuggable}) tasks
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

removeTask :: TargetSpec -> EventM Name TasksState (Maybe UTCTime)
removeTask target = do
  tasks <- use listElementsL
  case Seq.breakl ((== target) . (.target)) tasks of
    (before, (Task {startTime = start}) Seq.:<| after) -> do
      listElementsL .= before <> after
      modifying listSelectedL \ i ->
        if length before + length after == 0
        then Nothing
        else i
      pure (Just start)
    _ -> pure Nothing

taskFailure :: TargetSpec -> String -> EventM Name TasksState ()
taskFailure target content = do
  tasks <- use listElementsL
  case Seq.breakl ((== target) . (.target)) tasks of
    (before, task Seq.:<| after) ->
      listElementsL .= before <> (task {failure = Just content} Seq.<| after)
    _ -> pure ()

getSelectedTarget :: EventM Name TasksState (Maybe (WorkerId, TargetSpec))
getSelectedTarget = do
  preuse listSelectedElementL <&> fmap \ Task {worker, target} -> (worker, target)
