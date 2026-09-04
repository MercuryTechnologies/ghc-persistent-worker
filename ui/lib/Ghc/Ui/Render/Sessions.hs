module Ghc.Ui.Render.Sessions where

import Brick.Types (Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.List (renderList)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Session (SessionState (..))
import Ghc.Ui.Data.Sessions (SessionsState)
import Ghc.Ui.Utils (popup)

draw :: SessionsState -> Widget Name
draw ss =
  popup 50 "Select session" $ renderList drawOption True ss
 where
  drawOption isSel (_, SessionState {title, workers}) =
    str $
      concat @[]
        [ if isSel then "> " else "  "
        , title
        , " - "
        , show (length workers)
        , " workers"
        ]
