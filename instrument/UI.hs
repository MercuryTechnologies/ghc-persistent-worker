{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UI (app, customMainWithDefaultVty, initialState, SessionId, CustomEvent (..)) where

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
  modifyDefAttr,
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
import Brick.Widgets.List (GenericList, handleListEvent, list, listElementsL, listSelectedElement, listSelectedL, renderList)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Graphics.Vty qualified as V
import Lens.Micro.GHC (_2, filtered, each)
import Lens.Micro.Mtl (modifying, use, (.=))
import Lens.Micro.TH (makeLenses)
import Network.GRPC.Common.Protobuf (Proto, (^.))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Instr
import Numeric (showFFloat)

data Name = Main | SessionSelector
  deriving stock (Eq, Ord, Show)

type SessionId = FilePath
data CustomEvent
  = StartSession UTCTime
  | EndSession UTCTime
  | AddContent String
  | InstrEvent (Proto Instr.Event)

data State = State
  { _sessions :: GenericList Name Seq.Seq (SessionId, Session)
  , _showSessionsSelector :: Bool
  }

data Session = Session
  { _title :: String
  , _content :: String
  , _startTime :: UTCTime
  , _endTime :: Maybe UTCTime
  , _stats :: Stats
  }

mkSession :: String -> UTCTime -> Session
mkSession title start =
  Session
    { _title = title
    , _content = ""
    , _startTime = start
    , _endTime = Nothing
    , _stats = Stats 0 0 0 0
    }

data Stats = Stats
  { _memory :: Int -- in bytes
  , gc_cpu_ns :: Int
  , cpu_ns :: Int
  , _activeJobs :: Int
  }

makeLenses ''State
makeLenses ''Session
makeLenses ''Stats

initialState :: State
initialState =
  State
    { _sessions = list SessionSelector Seq.empty 1
    , _showSessionsSelector = False
    }

drawUI :: State -> [Widget Name]
drawUI State{..} =
  ( if _showSessionsSelector
      then [drawSessionSelector _sessions]
      else []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                let session = listSelectedElement _sessions
                 in maybe
                      (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                      (drawSession . snd . snd)
                      session
          , modifyDefAttr (`V.withStyle` V.italic) $ str " esc,q: quit   s: toggle session selector"
          ]
       ]

drawSessionSelector :: GenericList Name Seq.Seq (SessionId, Session) -> Widget Name
drawSessionSelector ss =
  centerLayer $
    hLimitPercent 50 $
      vLimitPercent 50 $
        borderWithLabel (str " Select session ") $
          renderList drawOption True ss
 where
  drawOption isSel (_, Session{..}) =
    str $
      concat @[]
        [ if isSel then "> " else "  "
        , _title
        , " - "
        , maybe "Running..." (take 19 . iso8601Show) _endTime
        ]

drawSession :: Session -> Widget Name
drawSession Session{..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ _title ++ " ") $
    vBox
      [ viewport Main Vertical (strWrap _content)
      , hBorder
      , drawStats _stats
      ]

drawStats :: Stats -> Widget Name
drawStats Stats{..} =
  str $
    "Memory: " ++ show (_memory `div` 1_000_000) ++ "Mb"
    ++ " | CPU Time: " ++ formatNs cpu_ns
    ++ " | GC Time: " ++ formatNs gc_cpu_ns
    ++ (if _activeJobs > 0  then " | Active jobs: " ++ show _activeJobs else "")

formatNs :: Int -> String
formatNs ns =
  let s :: Double = fromIntegral ns / 1_000_000_000.0
  in showFFloat (Just 3) s "s"

handleCustomEvent :: CustomEvent -> EventM Name Session ()
handleCustomEvent StartSession{} = pure () -- handled in handleEvent
handleCustomEvent (EndSession end) = do
  modifying endTime (const $ Just end)
  handleCustomEvent (AddContent "Session ended")
handleCustomEvent (AddContent newContent) = do
  modifying content (++ (newContent ++ "\n"))
  vScrollToEnd (viewportScroll Main)
handleCustomEvent (InstrEvent evt) =
  case evt ^. Instr.maybe'compileStart of
    Just cs -> do
      handleCustomEvent (AddContent ("Starting " ++ Text.unpack (cs ^. Instr.target)))
      modifying (stats . activeJobs) (+ 1)

    _ -> case evt ^. Instr.maybe'compileEnd of
      Just ce -> do
        handleCustomEvent (AddContent (Text.unpack $ ce ^. Instr.stderr))
        modifying (stats . activeJobs) (subtract 1)
      _ -> case evt ^. Instr.maybe'stats of
        Just msg -> do
          modifying stats \st ->
            st
              { _memory = fromIntegral $ msg ^. Instr.memory
              , gc_cpu_ns = fromIntegral $ msg ^. Instr.gcCpuNs
              , cpu_ns = fromIntegral $ msg ^. Instr.cpuNs
              }
        _ -> pure ()

handleEvent :: BrickEvent Name (SessionId, CustomEvent) -> EventM Name State ()
handleEvent (AppEvent (sid, StartSession start)) = do
  modifying
    (sessions . listElementsL)
    ( \m ->
        let i = Seq.length m + 1
         in Seq.insertAt 0 (sid, mkSession ("Session " ++ show i ++ "  " ++ take 19 (iso8601Show start)) start) m
    )
  sessions . listSelectedL .= Just 0
handleEvent (AppEvent (sid, evt)) = zoom (sessions . listElementsL . each . filtered ((== sid) . fst) . _2) (handleCustomEvent evt)
handleEvent (VtyEvent evt) = do
  ss <- use showSessionsSelector
  let hide = modifying showSessionsSelector not
  if ss
    then do
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        V.EvKey (V.KChar 's') [] -> hide
        _ -> zoom sessions (handleListEvent evt)
      vScrollToEnd (viewportScroll Main)
    else case evt of
      V.EvKey V.KEsc [] -> halt
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey V.KDown [] -> vScrollBy (viewportScroll Main) 1
      V.EvKey V.KUp [] -> vScrollBy (viewportScroll Main) (-1)
      V.EvKey V.KPageDown [] -> vScrollPage (viewportScroll Main) Down
      V.EvKey V.KPageUp [] -> vScrollPage (viewportScroll Main) Up
      V.EvKey (V.KChar 's') [] -> do
        modifying showSessionsSelector not
      _ -> pure ()
handleEvent _ = pure ()

app :: App State (SessionId, CustomEvent) Name
app =
  App
    { appDraw = drawUI
    , appStartEvent = pure ()
    , appHandleEvent = handleEvent
    , appAttrMap = const $ attrMap V.defAttr []
    , appChooseCursor = neverShowCursor
    }