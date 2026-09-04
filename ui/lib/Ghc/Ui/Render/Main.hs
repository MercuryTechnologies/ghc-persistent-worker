module Ghc.Ui.Render.Main where

import Brick.Forms (Form, renderForm)
import Brick.Types (Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (joinBorders, modifyDefAttr, str, vBox, withBorderStyle)
import Brick.Widgets.List (listSelectedElement)
import Ghc.Ui.ActiveTasks qualified as ActiveTasks
import Ghc.Ui.Event.Main (MainState (..))
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Session qualified as Session
import Ghc.Ui.SessionSelector qualified as SessionSelector
import Ghc.Ui.Types (Name (..))
import Ghc.Ui.Utils (popup)
import Graphics.Vty (italic, withStyle)
import Types.State (Options (..))

drawOptionsEditor :: Form Options e Name -> Widget Name
drawOptionsEditor form = popup 50 "Session Options" $ renderForm form

renderMain :: MainState -> [Widget Name]
renderMain MainState {..} =
  ( case currentFocus of
      SessionSelector -> [SessionSelector.draw sessions]
      OptionsEditor -> [drawOptionsEditor options]
      TaskDetails -> let task = session >>= listSelectedElement . (.activeTasks) in maybe [] (pure . ActiveTasks.drawTaskDetails . snd) task
      ModuleDetails -> let mdl = session >>= listSelectedElement . (.modules) in maybe [] (pure . ModuleSelector.drawModuleDetails . snd) mdl
      _ -> []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                  (Session.draw currentFocus currentTime)
                  session
          , modifyDefAttr (`withStyle` italic) $ str " q:quit   Enter:show details   r:trigger rebuild   d:debug   o:toggle options editor   s:toggle session selector"
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement sessions
