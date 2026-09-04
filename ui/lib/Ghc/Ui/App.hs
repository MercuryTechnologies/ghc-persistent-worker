module Ghc.Ui.App where

import Brick (AttrMap)
import Brick.AttrMap (attrMap)
import Brick.Main (App (..), showFirstCursor)
import Brick.Util (on)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedFocusedAttr)
import Ghc.Ui.Data.Main (MainEvent, MainState (..))
import Ghc.Ui.Event.Main (handleEvent)
import Ghc.Ui.Grpc (serverApi)
import Ghc.Ui.Render.Main (renderMain)
import Ghc.Ui.Types (Name (..), canDebugAttr, disabledAttr)
import Graphics.Vty (bold, defAttr, dim, withStyle)
import Graphics.Vty.Attributes.Color (blue, brightBlack, brightWhite)

attrMapMain :: a -> AttrMap
attrMapMain _ =
  attrMap defAttr [
    (editFocusedAttr, brightWhite `on` blue),
    (listSelectedAttr, brightWhite `on` brightBlack),
    (listSelectedFocusedAttr, brightWhite `on` blue),
    (disabledAttr, withStyle defAttr dim),
    (canDebugAttr, withStyle defAttr bold)
  ]

app :: App MainState MainEvent Name
app =
  App {
    appDraw = renderMain,
    appStartEvent = pure (),
    appHandleEvent = handleEvent serverApi,
    appAttrMap = attrMapMain,
    appChooseCursor = showFirstCursor
  }
