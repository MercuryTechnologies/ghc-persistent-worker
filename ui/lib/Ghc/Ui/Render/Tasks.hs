module Ghc.Ui.Render.Tasks where

import Brick.Types (Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, withAttr, (<+>))
import Brick.Widgets.List (renderList)
import Data.Time (UTCTime, diffUTCTime, nominalDiffTimeToSeconds)
import Ghc.Ui.Attr (canDebugAttr)
import Ghc.Ui.Data.Name (Name (Tasks))
import Ghc.Ui.Data.Tasks (Task (..), TasksState)
import Ghc.Ui.Utils (formatPico, popup)
import Types.Target (renderTargetSpec)

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task {..} =
  popup 70 (renderTargetSpec target) $ strWrap $ maybe "" id failure

draw :: Name -> UTCTime -> TasksState -> Widget Name
draw current now = renderList drawTask (current == Tasks)
 where
  drawTask _ Task {..} =
    (if debuggable then withAttr canDebugAttr else id) $
      padRight Max (str (renderTargetSpec target)) <+> str (maybe (formatPico $ nominalDiffTimeToSeconds (max 0 (diffUTCTime now startTime))) (const "Failure") failure)
