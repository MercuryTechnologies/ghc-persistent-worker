{-# LANGUAGE TemplateHaskell #-}

module UI (app, customMainWithDefaultVty, initialState, CustomEvent(..)) where

import Brick (App (..), BrickEvent (..), EventM, ViewportType (..), Widget, attrMap, customMainWithDefaultVty, halt, joinBorders, neverShowCursor, str, withBorderStyle, vScrollBy, viewportScroll)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Core (vBox, viewport)
import Graphics.Vty qualified as V
import Lens.Micro.Mtl (modifying)
import Lens.Micro.TH (makeLenses)

data Name = Main
  deriving stock (Eq, Ord, Show)

data CustomEvent = AddContent String

data State = State
  { _content :: String
  }
makeLenses ''State

initialState :: State
initialState =
  State
    { _content = ""
    }

drawUI :: State -> [Widget Name]
drawUI State{..} =
  [ joinBorders $
      withBorderStyle unicodeRounded $
        borderWithLabel (str "GHC Persistent Worker") $
          vBox
            [ viewport Main Vertical (str _content)
            , hBorder
            , str "Static info"
            ]
  ]

handleEvent :: BrickEvent Name CustomEvent -> EventM Name State ()
handleEvent (AppEvent (AddContent newContent)) = modifying content (++ newContent)
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KDown [])) = vScrollBy (viewportScroll Main) 1
handleEvent (VtyEvent (V.EvKey V.KUp [])) = vScrollBy (viewportScroll Main) (-1)
handleEvent _ = pure ()

app :: App State CustomEvent Name
app =
  App
    { appDraw = drawUI
    , appStartEvent = pure ()
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrMap V.defAttr []
    , appChooseCursor = neverShowCursor
    }