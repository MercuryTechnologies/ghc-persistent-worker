module UI.ActiveTasks where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedL, renderList, listSelectedElementL)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Types.State (Target (..))
import Lens.Micro.Platform (modifying, use, (.=), preuse)
import UI.Types (Name (ActiveTasks), WorkerId)
import UI.Utils (formatPico, popup)

type State = GenericList Name Seq.Seq Task

initialState :: State
initialState = list ActiveTasks Seq.empty 1

data Task = Task
  { _taskTarget :: Target
  , _taskStartTime :: UTCTime
  , _failure :: Maybe String
  , _fromWorker :: WorkerId
  }

draw :: Name -> UTCTime -> State -> Widget Name
draw current now = renderList drawTask (current == ActiveTasks)
 where
  drawTask _ Task{_taskTarget = Target name, ..} =
    padRight Max (str name) <+> str (maybe (formatPico $ nominalDiffTimeToSeconds (max 0 (diffUTCTime now _taskStartTime))) (const "Failure") _failure)

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{_taskTarget = Target name,..} =
  popup 70 name $ strWrap $ maybe "" id _failure

addTask :: Target -> WorkerId -> EventM Name State ()
addTask name wid = do
  time <- liftIO $ getCurrentTime
  modifying
    listElementsL
    (Seq.insertAt 0 (Task name time Nothing wid))
  modifying listSelectedL (Just . maybe 0 succ)

removeTask :: Target -> EventM Name State (Maybe UTCTime)
removeTask target = do
  tasks <- use listElementsL
  case Seq.breakl ((== target) . _taskTarget) tasks of
    (before, (Task _ start _ _) Seq.:<| after) -> do
      listElementsL .= before <> after
      modifying listSelectedL (fmap $ \i -> if i > length before then i - 1 else i)
      pure $ Just start
    _ -> pure Nothing

taskFailure :: Target -> String -> EventM Name State ()
taskFailure target content = do
  tasks <- use listElementsL
  case Seq.breakl ((== target) . _taskTarget) tasks of
    (before, task Seq.:<| after) ->
      listElementsL .= before <> (task{_failure = Just content} Seq.<| after)
    _ -> pure ()

getRebuildTarget :: EventM Name State (Maybe (WorkerId, Target))
getRebuildTarget = do
  mtask <- preuse listSelectedElementL
  pure $ (\Task{_fromWorker = wid, _taskTarget = target} -> (wid, target)) <$> mtask