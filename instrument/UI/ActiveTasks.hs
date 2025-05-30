module UI.ActiveTasks where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedL, renderList)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Lens.Micro.Platform (modifying, use, (.=))
import UI.Types (Name (ActiveTasks))
import UI.Utils (formatPico, popup)

type State = GenericList Name Seq.Seq Task

initialState :: State
initialState = list ActiveTasks Seq.empty 1

data Task = Task
  { _taskName :: String
  , _taskStartTime :: UTCTime
  , _failure :: Maybe String
  }

draw :: Name -> UTCTime -> State -> Widget Name
draw current now = renderList drawTask (current == ActiveTasks)
 where
  drawTask _ Task{..} =
    padRight Max (str _taskName) <+> str (maybe (formatPico $ nominalDiffTimeToSeconds (max 0 (diffUTCTime now _taskStartTime))) (const "Failure") _failure)

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{..} =
  popup 70 _taskName $ strWrap $ maybe "" id _failure

addTask :: String -> EventM Name State ()
addTask name = do
  time <- liftIO $ getCurrentTime
  modifying
    listElementsL
    (Seq.insertAt 0 (Task name time Nothing))
  modifying listSelectedL (Just . maybe 0 succ)

removeTask :: String -> EventM Name State (Maybe UTCTime)
removeTask name = do
  tasks <- use listElementsL
  case Seq.breakl ((== name) . _taskName) tasks of
    (before, (Task _ start _) Seq.:<| after) -> do
      listElementsL .= before <> after
      modifying listSelectedL (fmap $ \i -> if i > length before then i - 1 else i)
      pure $ Just start
    _ -> pure Nothing

taskFailure :: String -> String -> EventM Name State ()
taskFailure name content = do
  tasks <- use listElementsL
  case Seq.breakl ((== name) . _taskName) tasks of
    (before, task Seq.:<| after) ->
      listElementsL .= before <> (task{_failure = Just content} Seq.<| after)
    _ -> pure ()
