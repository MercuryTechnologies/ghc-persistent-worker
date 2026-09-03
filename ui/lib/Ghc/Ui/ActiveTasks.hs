{-# LANGUAGE FieldSelectors #-}

module Ghc.Ui.ActiveTasks where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedElementL, listSelectedL, renderList)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Ghc.Ui.Types (Name (ActiveTasks), WorkerId, canDebugAttr)
import Ghc.Ui.Utils (formatPico, popup)
import Lens.Micro.Platform (modifying, preuse, use, (.=))
import Types.Target (TargetSpec (..), renderTargetSpec)

type State = GenericList Name Seq.Seq Task

initialState :: State
initialState = list ActiveTasks Seq.empty 1

data Task = Task
  { _taskTarget :: TargetSpec
  , _taskStartTime :: UTCTime
  , _failure :: Maybe String
  , _fromWorker :: WorkerId
  , _canDebug :: Bool
  }

draw :: Name -> UTCTime -> State -> Widget Name
draw current now = renderList drawTask (current == ActiveTasks)
 where
  drawTask _ Task{_taskTarget = name, ..} =
    (if _canDebug then withAttr canDebugAttr else id) $
      padRight Max (str (renderTargetSpec name)) <+> str (maybe (formatPico $ nominalDiffTimeToSeconds (max 0 (diffUTCTime now _taskStartTime))) (const "Failure") _failure)

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{_taskTarget = name,..} =
  popup 70 (renderTargetSpec name) $ strWrap $ maybe "" id _failure

addTask :: TargetSpec -> WorkerId -> Bool -> EventM Name State ()
addTask name wid canDebug = do
  time <- liftIO $ getCurrentTime
  tasks <- use listElementsL
  let i = if canDebug then 0 else fromMaybe 0 (Seq.findIndexL (not . _canDebug) tasks)
  listElementsL .= Seq.insertAt i (Task name time Nothing wid canDebug) tasks
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

removeTask :: TargetSpec -> EventM Name State (Maybe UTCTime)
removeTask target = do
  tasks <- use listElementsL
  case Seq.breakl ((== target) . _taskTarget) tasks of
    (before, (Task { _taskStartTime = start }) Seq.:<| after) -> do
      listElementsL .= before <> after
      modifying listSelectedL (\i -> if length before + length after == 0 then Nothing else i)
      pure $ Just start
    _ -> pure Nothing

taskFailure :: TargetSpec -> String -> EventM Name State ()
taskFailure target content = do
  tasks <- use listElementsL
  case Seq.breakl ((== target) . _taskTarget) tasks of
    (before, task Seq.:<| after) ->
      listElementsL .= before <> (task{_failure = Just content} Seq.<| after)
    _ -> pure ()

getSelectedTarget :: EventM Name State (Maybe (WorkerId, TargetSpec))
getSelectedTarget = do
  mtask <- preuse listSelectedElementL
  pure $ (\Task{_fromWorker = wid, _taskTarget = target} -> (wid, target)) <$> mtask
