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
import Types.Api (RequestId, Target)

-- | Insert a task, the debuggable ones first, and keep the selection on the same task.
insertTask :: Maybe RequestId -> Maybe String -> Target -> WorkerId -> Bool -> EventM Name TasksState ()
insertTask requestId failure target worker debuggable = do
  startTime <- liftIO $ getCurrentTime
  tasks <- use listElementsL
  let i = if debuggable then 0 else fromMaybe 0 (Seq.findIndexL (not . (.debuggable)) tasks)
  listElementsL .= Seq.insertAt i (Task {target, requestId, startTime, failure, worker, debuggable}) tasks
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

addTask :: Target -> RequestId -> WorkerId -> Bool -> EventM Name TasksState ()
addTask target requestId =
  insertTask (Just requestId) Nothing target

-- | Show a rebuild that the worker did not accept as a failed task.
addFailedRebuild :: Target -> WorkerId -> String -> EventM Name TasksState ()
addFailedRebuild target worker message =
  insertTask Nothing (Just message) target worker False

-- | Whether the task is the compilation of the target in the request.
isTask :: Target -> RequestId -> Task -> Bool
isTask target requestId task =
  task.target == target && task.requestId == Just requestId

removeTask :: Target -> RequestId -> EventM Name TasksState (Maybe UTCTime)
removeTask target requestId = do
  tasks <- use listElementsL
  case Seq.breakl (isTask target requestId) tasks of
    (before, (Task {startTime = start}) Seq.:<| after) -> do
      listElementsL .= before <> after
      modifying listSelectedL \ i ->
        if length before + length after == 0
        then Nothing
        else i
      pure (Just start)
    _ -> pure Nothing

taskFailure :: Target -> RequestId -> String -> EventM Name TasksState ()
taskFailure target requestId content = do
  tasks <- use listElementsL
  case Seq.breakl (isTask target requestId) tasks of
    (before, task Seq.:<| after) ->
      listElementsL .= before <> (task {failure = Just content} Seq.<| after)
    _ -> pure ()

getSelectedTarget :: EventM Name TasksState (Maybe (WorkerId, Target))
getSelectedTarget = do
  preuse listSelectedElementL <&> fmap \ Task {worker, target} -> (worker, target)
