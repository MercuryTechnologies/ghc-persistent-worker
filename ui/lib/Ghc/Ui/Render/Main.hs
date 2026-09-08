module Ghc.Ui.Render.Main where

import Brick.Types (Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (joinBorders, modifyDefAttr, str, vBox, withBorderStyle)
import Brick.Widgets.List (listSelectedElement)
import Ghc.Ui.Data.Main (MainState (..))
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.Session qualified as Session
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import qualified Ghc.Ui.Render.Session as Session
import qualified Ghc.Ui.Render.Sessions as Sessions
import Ghc.Ui.Render.Tasks qualified as Tasks
import Graphics.Vty (italic, withStyle)

renderMain :: MainState -> [Widget Name]
renderMain MainState {..} =
  ( case currentFocus of
      Sessions -> [Sessions.draw sessions]
      TaskDetails -> let task = session >>= listSelectedElement . (.activeTasks) in maybe [] (pure . Tasks.drawTaskDetails . snd) task
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
