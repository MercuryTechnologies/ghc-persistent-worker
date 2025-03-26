{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UI (app, customMainWithDefaultVty, initialState, CustomEvent (..)) where

import Brick (
  App (..),
  BrickEvent (..),
  Direction (..),
  EventM,
  ViewportScroll (..),
  ViewportType (..),
  Widget,
  attrMap,
  customMainWithDefaultVty,
  halt,
  joinBorders,
  neverShowCursor,
  str,
  vLimitPercent,
  viewportScroll,
  withBorderStyle,
  zoom,
 )
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.Core (hLimitPercent, strWrap, vBox, viewport)
import Brick.Widgets.List (GenericList, Splittable (..), handleListEvent, list, listElementsL, listSelectedElement, listSelectedL, renderList)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Graphics.Vty qualified as V
import Lens.Micro.GHC (ix)
import Lens.Micro.Mtl (modifying, use, (.=))
import Lens.Micro.TH (makeLenses)
import Network.GRPC.Common.Protobuf (Proto, (^.))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Instr

data Name = Main | SessionSelector
  deriving stock (Eq, Ord, Show)

type SessionId = FilePath
data CustomEvent
  = StartSession SessionId UTCTime
  | EndSession SessionId UTCTime
  | AddContent SessionId String
  | InstrEvent SessionId (Proto Instr.Event)

data State = State
  { _sessions :: GenericList Name (Map.Map SessionId) Session
  , _showSessionsSelector :: Bool
  }

data Session = Session
  { _title :: String
  , _content :: String
  , _startTime :: UTCTime
  , _endTime :: Maybe UTCTime
  }

makeLenses ''State
makeLenses ''Session

initialState :: State
initialState =
  State
    { _sessions = list SessionSelector Map.empty 1
    , _showSessionsSelector = False
    }

drawUI :: State -> [Widget Name]
drawUI State{..} =
  ( if _showSessionsSelector
      then [drawSessionSelector _sessions]
      else []
  )
    ++ [ joinBorders $
          withBorderStyle unicodeRounded $
            let session = listSelectedElement _sessions
             in maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                  (drawSession . snd)
                  session
       ]

drawSessionSelector :: GenericList Name (Map.Map SessionId) Session -> Widget Name
drawSessionSelector ss =
  centerLayer $
    hLimitPercent 50 $
      vLimitPercent 50 $
        borderWithLabel (str " Select session ") $
          renderList (\isSel Session{..} -> str ((if isSel then "> " else "  ") ++ _title)) True ss

drawSession :: Session -> Widget Name
drawSession Session{..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ _title ++ " ") $
    vBox
      [ viewport Main Vertical (strWrap _content)
      , hBorder
      , str "Static info"
      ]

handleEvent :: BrickEvent Name CustomEvent -> EventM Name State ()
handleEvent (AppEvent (StartSession sid start)) = do
  modifying (sessions . listElementsL) (\m -> Map.insert sid (Session ("Session " ++ show (Map.size m + 1) ++ "  " ++ take 19 (iso8601Show start)) "" start Nothing) m)
  sessions . listSelectedL .= Just 0
handleEvent (AppEvent (EndSession sid end)) = do
  modifying (sessions . listElementsL . ix sid . endTime) (const $ Just end)
  handleEvent (AppEvent (AddContent sid "Session ended"))
handleEvent (AppEvent (AddContent sid newContent)) = do
  modifying (sessions . listElementsL . ix sid . content) (++ (newContent ++ "\n"))
  vScrollToEnd (viewportScroll Main)
handleEvent (AppEvent (InstrEvent sid evt)) = do
  modifying (sessions . listElementsL . ix sid . content) (++ (Text.unpack $ evt ^. Instr.compileEnd ^. Instr.stderr))
handleEvent (VtyEvent evt) = do
  ss <- use showSessionsSelector
  let hide = modifying showSessionsSelector not
  if ss
    then case evt of
      V.EvKey V.KEsc [] -> hide
      V.EvKey V.KEnter [] -> hide
      V.EvKey (V.KChar 's') [] -> hide
      _ -> zoom sessions (handleListEvent evt)
    else case evt of
      V.EvKey V.KEsc [] -> halt
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey V.KDown [] -> vScrollBy (viewportScroll Main) 1
      V.EvKey V.KUp [] -> vScrollBy (viewportScroll Main) (-1)
      V.EvKey V.KPageDown [] -> vScrollPage (viewportScroll Main) Down
      V.EvKey V.KPageUp [] -> vScrollPage (viewportScroll Main) Up
      V.EvKey (V.KChar 's') [] -> do
        modifying showSessionsSelector not
        handleEvent (AppEvent (AddContent "test-a" "Session ended"))
      _ -> pure ()
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

instance Splittable (Map.Map k) where
  splitAt i m = (Map.take i m, Map.drop i m)