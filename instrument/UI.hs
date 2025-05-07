{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UI (module UI, customMainWithDefaultVty) where

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
  on,
  showFirstCursor,
  str,
  vLimitPercent,
  viewportScroll,
  withBorderStyle,
  zoom,
 )
import Brick.Forms (Form, FormFieldState, editTextField, formState, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.Core (hLimitPercent, strWrap, vBox, viewport, (<+>))
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (GenericList, handleListEvent, list, listElementsL, listSelectedElement, listSelectedElementL, listSelectedL, renderList)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Internal.Cache (Options (..), defaultOptions)
import Lens.Micro.Platform (Lens', each, filtered, lens, makeLenses, modifying, packed, preuse, use, (.=), _2)
import Network.GRPC.Common.Protobuf (Proto, (^.))
import Numeric (showFFloat)
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Instr

data Name = Main | SessionSelector | OEExtraGhcOptions
  deriving stock (Eq, Ord, Show)

type SessionId = FilePath
data Event
  = SendOptions
  | SessionEvent SessionId SessionEvent
data SessionEvent
  = StartSession UTCTime (Options -> IO ())
  | EndSession UTCTime
  | AddContent String
  | InstrEvent (Proto Instr.Event)

data State = State
  { _sessions :: GenericList Name Seq.Seq (SessionId, Session)
  , _options :: Form Options Event Name
  , _showSessionsSelector :: Bool
  , _showOptionsEditor :: Bool
  }

data Session = Session
  { _title :: String
  , _content :: String
  , _startTime :: UTCTime
  , _endTime :: Maybe UTCTime
  , _stats :: Stats
  , _sendOptions :: Options -> IO ()
  }

mkSession :: String -> UTCTime -> (Options -> IO ()) -> Session
mkSession _title _startTime _sendOptions =
  Session
    { _title
    , _content = ""
    , _startTime
    , _endTime = Nothing
    , _stats = Stats mempty 0 0 0
    , _sendOptions
    }

data Stats = Stats
  { _memory :: Map.Map Text.Text Int -- in bytes
  , _gc_cpu_ns :: Int
  , _cpu_ns :: Int
  , _activeJobs :: Int
  }

makeLenses ''State
makeLenses ''Session
makeLenses ''Stats

ghcOptionsLens :: Lens' Options Text.Text
ghcOptionsLens =
  lens
    ((.extraGhcOptions))
    (\opts s -> opts{extraGhcOptions = s})
    . packed

initialState :: State
initialState =
  State
    { _sessions = list SessionSelector Seq.empty 1
    , _options = newForm optionFields defaultOptions
    , _showSessionsSelector = False
    , _showOptionsEditor = False
    }

optionFields :: [Options -> FormFieldState Options Event Name]
optionFields =
  [ (str "Extra GHC Options: " <+>) @@= editTextField ghcOptionsLens OEExtraGhcOptions (Just 1)
  ]

drawUI :: State -> [Widget Name]
drawUI State{..} =
  ( if _showSessionsSelector
      then [drawSessionSelector _sessions]
      else
        if _showOptionsEditor
          then [drawOptionsEditor _options]
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
          , modifyDefAttr (`V.withStyle` V.italic) $ str " q:quit   s:toggle session selector   o:toggle options editor "
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

drawOptionsEditor :: Form Options Event Name -> Widget Name
drawOptionsEditor form =
  centerLayer $
    hLimitPercent 50 $
      borderWithLabel (str " Session Options ") $
        renderForm form

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
  vBox
    [ str $ "Memory:"
            ++ concatMap (\(k, v) -> " " ++ Text.unpack k ++ "=" ++ formatBytes v)
                (Map.toList _memory)
    , str $ " CPU Time: "
          ++ formatNs _cpu_ns
          ++ " | GC Time: "
          ++ formatNs _gc_cpu_ns
          ++ (if _activeJobs > 0 then " | Active jobs: " ++ show _activeJobs else "")
    ]

formatBytes :: Int -> String
formatBytes = go ["b", "Kb", "Mb", "Gb", "Tb", "Pb"]
  where
    go :: [String] -> Int -> String
    go (unit:units) n
      | n >= 10_000 = go units (n `div` 1_000)
      | otherwise = show n ++ unit

formatNs :: Int -> String
formatNs ns =
  let s :: Double = fromIntegral ns / 1_000_000_000.0
   in showFFloat (Just 3) s "s"

handleCustomEvent :: SessionEvent -> EventM Name Session ()
handleCustomEvent StartSession{} = pure () -- handled in handleEvent
handleCustomEvent (EndSession end) = do
  modifying endTime (const $ Just end)
  handleCustomEvent (AddContent "Session ended")
handleCustomEvent (AddContent newContent) = do
  when (newContent /= "") $ modifying content (++ (newContent ++ "\n"))
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
              { _memory = fromIntegral <$> msg ^. Instr.memory
              , _gc_cpu_ns = fromIntegral $ msg ^. Instr.gcCpuNs
              , _cpu_ns = fromIntegral $ msg ^. Instr.cpuNs
              }
        _ -> pure ()

handleEvent :: BrickEvent Name Event -> EventM Name State ()
handleEvent (AppEvent SendOptions) = do
  opts <- use options
  sendOpts <- preuse (sessions . listSelectedElementL)
  for_ sendOpts $ \(_, s) -> do
    liftIO $ _sendOptions s (formState opts)
handleEvent (AppEvent (SessionEvent sid (StartSession start sendOpts))) = do
  modifying
    (sessions . listElementsL)
    ( \m ->
        let i = Seq.length m + 1
            stitle = "Session " ++ show i ++ "  " ++ take 19 (iso8601Show start)
         in Seq.insertAt 0 (sid, mkSession stitle start sendOpts) m
    )
  sessions . listSelectedL .= Just 0
  handleEvent (AppEvent SendOptions)
handleEvent (AppEvent (SessionEvent sid evt)) = zoom (sessions . listElementsL . each . filtered ((== sid) . fst) . _2) (handleCustomEvent evt)
handleEvent (VtyEvent evt) = do
  ss <- use showSessionsSelector
  oe <- use showOptionsEditor
  if ss
    then do
      let hide = modifying showSessionsSelector not
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        V.EvKey (V.KChar 's') [] -> hide
        _ -> zoom sessions (handleListEvent evt)
      vScrollToEnd (viewportScroll Main)
    else
      if oe
        then do
          let hide = do
                modifying showOptionsEditor not
                handleEvent (AppEvent SendOptions)
          case evt of
            V.EvKey V.KEsc [] -> hide
            V.EvKey V.KEnter [] -> hide
            _ -> zoom options (handleFormEvent (VtyEvent evt))
        else case evt of
          V.EvKey V.KEsc [] -> halt
          V.EvKey V.KDown [] -> vScrollBy (viewportScroll Main) 1
          V.EvKey V.KUp [] -> vScrollBy (viewportScroll Main) (-1)
          V.EvKey V.KPageDown [] -> vScrollPage (viewportScroll Main) Down
          V.EvKey V.KPageUp [] -> vScrollPage (viewportScroll Main) Up
          V.EvKey (V.KChar 'q') [] -> halt
          V.EvKey (V.KChar 's') [] -> do
            modifying showSessionsSelector not
          V.EvKey (V.KChar 'o') [] -> do
            modifying showOptionsEditor not
          _ -> pure ()
handleEvent MouseDown{} = pure ()
handleEvent MouseUp{} = pure ()

app :: App State Event Name
app =
  App
    { appDraw = drawUI
    , appStartEvent = pure ()
    , appHandleEvent = handleEvent
    , appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (editFocusedAttr, brightWhite `on` blue)
            ]
    , appChooseCursor = showFirstCursor
    }