{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UI (module UI, customMainWithDefaultVty) where

import Brick.AttrMap (attrMap)
import Brick.Forms (Form, FormFieldState, editTextField, formState, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Main (App (..), customMainWithDefaultVty, halt, showFirstCursor)
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.Core (Padding (..), hLimitPercent, joinBorders, modifyDefAttr, padRight, str, strWrap, vBox, vLimitPercent, withBorderStyle, (<+>))
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (GenericList, handleListEvent, list, listElementsL, listSelectedAttr, listSelectedElement, listSelectedElementL, listSelectedFocusedAttr, listSelectedL, renderList)
import Control.Monad.IO.Class (liftIO)
import Data.Fixed (Fixed (..), Pico)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time (UTCTime(..), diffUTCTime, fromGregorian, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Internal.Cache (Options (..), defaultOptions)
import Lens.Micro.Platform (Lens', each, filtered, lens, makeLenses, modifying, packed, preuse, use, zoom, (.=), _2)
import Network.GRPC.Common.Protobuf (Proto, (^.))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Instr

data Name = ActiveTasks | TaskDetails | ModuleSelector | ModuleDetails | SessionSelector | OptionsEditor | OEExtraGhcOptions
  deriving stock (Eq, Ord, Show)

type SessionId = FilePath

data Event
  = SendOptions
  | SetTime UTCTime
  | SessionEvent SessionId SessionEvent

data SessionEvent
  = StartSession UTCTime (Options -> IO ())
  | EndSession UTCTime
  | InstrEvent (Proto Instr.Event)

data State = State
  { _sessions :: GenericList Name Seq.Seq (SessionId, Session)
  , _options :: Form Options Event Name
  , _currentFocus :: Name
  , _currentTime :: UTCTime
  }

data Session = Session
  { _title :: String
  , _activeTasks :: GenericList Name Seq.Seq Task
  , _modules :: GenericList Name Seq.Seq Module
  , _sesStartTime :: UTCTime
  , _sesEndTime :: Maybe UTCTime
  , _stats :: Stats
  , _sendOptions :: Options -> IO ()
  }

data Task = Task
  { _taskName :: String
  , _taskStartTime :: UTCTime
  , _failure :: Maybe String
  }

data Module = Module
  { _modName :: String
  , _content :: String
  , _modCompileTime :: Maybe Pico
  }

mkSession :: String -> UTCTime -> (Options -> IO ()) -> Session
mkSession _title _startTime _sendOptions =
  Session
    { _title
    , _activeTasks = list ActiveTasks Seq.empty 1
    , _modules = list ModuleSelector Seq.empty 1
    , _sesStartTime = _startTime
    , _sesEndTime = Nothing
    , _stats = Stats mempty 0 0 0
    , _sendOptions
    }

data Stats = Stats
  { _memory :: Map.Map Text.Text Int -- in bytes
  , _gc_cpu_ns :: Int
  , _cpu_ns :: Int
  , _activeTaskCount :: Int
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

upsertAscSeq :: (Ord b) => (a -> b) -> a -> Seq.Seq a -> (Int, Seq.Seq a)
upsertAscSeq meas x as = binSearch 0 (Seq.length as - 1)
  where
    binSearch l r
      | l > r = (l, Seq.insertAt l x as)
      | otherwise =
          let m = (l + r) `div` 2
              x' = Seq.index as m
              b' = meas x'
          in if meas x < b'
                then binSearch l (m - 1)
                else
                  if meas x > b'
                    then binSearch (m + 1) r
                    else (m, Seq.update m x as)

initialState :: State
initialState =
  State
    { _sessions = list SessionSelector Seq.empty 1
    , _options = newForm optionFields defaultOptions
    , _currentFocus = ModuleSelector
    , _currentTime = UTCTime (fromGregorian 1970 1 1) 0
    }

optionFields :: [Options -> FormFieldState Options Event Name]
optionFields =
  [ (str "Extra GHC Options: " <+>) @@= editTextField ghcOptionsLens OEExtraGhcOptions (Just 1)
  ]

drawUI :: State -> [Widget Name]
drawUI State{..} =
  ( case _currentFocus of
      SessionSelector -> [drawSessionSelector _sessions]
      OptionsEditor -> [drawOptionsEditor _options]
      TaskDetails -> let task = session >>= listSelectedElement . _activeTasks in maybe [] (pure . drawTaskDetails . snd) task
      ModuleDetails -> let mdl = session >>= listSelectedElement . _modules in maybe [] (pure . drawModuleDetails . snd) mdl
      _ -> []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                  (drawSession _currentFocus _currentTime)
                  session
          , modifyDefAttr (`V.withStyle` V.italic) $ str " q:quit   Enter:show details   s:toggle session selector   o:toggle options editor "
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement _sessions

popup :: Int -> String -> Widget Name -> Widget Name
popup size popupTitle content =
  centerLayer $
    hLimitPercent size $
      vLimitPercent size $
        borderWithLabel (str $ " " ++ popupTitle ++ " ") content

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{..} =
  popup 70 _taskName $ strWrap $ maybe "" id _failure

drawModuleDetails :: Module -> Widget Name
drawModuleDetails Module{..} =
  popup 70 _modName $
    vBox
      [ str $ "Compile time: " ++ maybe "" (formatPs . (\(MkFixed n) -> n)) _modCompileTime
      , strWrap _content
      ]

drawSessionSelector :: GenericList Name Seq.Seq (SessionId, Session) -> Widget Name
drawSessionSelector ss =
  popup 50 "Select session" $ renderList drawOption True ss
 where
  drawOption isSel (_, Session{..}) =
    str $
      concat @[]
        [ if isSel then "> " else "  "
        , _title
        , " - "
        , maybe "Running..." (take 19 . iso8601Show) _sesEndTime
        ]

drawOptionsEditor :: Form Options Event Name -> Widget Name
drawOptionsEditor form = popup 50 "Session Options" $ renderForm form

drawSession :: Name -> UTCTime -> Session -> Widget Name
drawSession current now Session{..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ _title ++ " ") $
    vBox
      [ vLimitPercent 30 $ renderList drawTask (current == ActiveTasks) _activeTasks
      , hBorder
      , renderList drawModule (current == ModuleSelector) _modules
      , hBorder
      , drawStats _stats
      ]
 where
  drawTask _ Task{..} =
    padRight Max (str _taskName) <+> str (maybe (formatPico $ nominalDiffTimeToSeconds (max 0 (diffUTCTime now _taskStartTime))) (const "Failure") _failure)
  drawModule _ Module{..} =
    padRight Max (str _modName) <+> str (maybe "" formatPico _modCompileTime)

drawStats :: Stats -> Widget Name
drawStats Stats{..} =
  vBox
    [ str $
        "Memory:"
          ++ concatMap
            (\(k, v) -> " " ++ Text.unpack k ++ "=" ++ formatBytes v)
            (Map.toList _memory)
    , str $
        " CPU Time: "
          ++ formatPs (1000 * _cpu_ns)
          ++ " | GC Time: "
          ++ formatPs (1000 * _gc_cpu_ns)
          ++ (if _activeTaskCount > 0 then " | Active jobs: " ++ show _activeTaskCount else "")
    ]

formatBytes :: (Integral a, Show a) => a -> String
formatBytes = format ["b", "Kb", "Mb", "Gb", "Tb", "Pb"]

formatPs :: (Integral a, Show a) => a -> String
formatPs = format ["ps", "ns", "µs", "ms", "s"]

formatPico :: Pico -> String
formatPico (MkFixed n) = formatPs n

format :: (Integral a, Show a) => [String] -> a -> String
format [unit] n = show n ++ unit
format (unit : units) n
  | n >= 10_000 = format units (n `div` 1_000)
  | otherwise = show n ++ unit
format [] _ = error "No units given"

stripEscSeqs :: String -> String
stripEscSeqs [] = []
stripEscSeqs ('\ESC' : '[' : xs) = stripEscSeqs (drop 1 (dropWhile (/= 'm') xs))
stripEscSeqs (x : xs) = x : stripEscSeqs xs

handleCustomEvent :: SessionEvent -> EventM Name Session ()
handleCustomEvent StartSession{} = pure () -- handled in handleEvent
handleCustomEvent (EndSession end) = do
  modifying sesEndTime (const $ Just end)
handleCustomEvent (InstrEvent evt) =
  case evt ^. Instr.maybe'compileStart of
    Just cs -> do
      time <- liftIO $ getCurrentTime
      modifying
        (activeTasks . listElementsL)
        (Seq.insertAt 0 (Task (Text.unpack $ cs ^. Instr.target) time Nothing))
      modifying (activeTasks . listSelectedL) (Just . maybe 0 succ)
      modifying (stats . activeTaskCount) succ
    _ -> case evt ^. Instr.maybe'compileEnd of
      Just ce -> do
        modifying (stats . activeTaskCount) pred
        tasks <- use (activeTasks . listElementsL)
        let content = stripEscSeqs (Text.unpack $ ce ^. Instr.stderr)
            target' = Text.unpack $ ce ^. Instr.target
            target = if target' == "" then takeWhile (/= ':') content else target'
        case Seq.breakl ((== target) . _taskName) tasks of
          (before, task@(Task name start _) Seq.:<| after) -> do
            if ce ^. Instr.exitCode == 0
              then do
                activeTasks . listElementsL .= (before <> after)
                modifying (activeTasks . listSelectedL) (fmap $ \i -> if i > length before then i - 1 else i)
                mods <- use (modules . listElementsL)
                end <- liftIO $ getCurrentTime
                let time = nominalDiffTimeToSeconds (diffUTCTime end start)
                let (i, mods') = upsertAscSeq _modName (Module name content (Just time)) mods
                modifying (modules . listSelectedL) (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))
                modules . listElementsL .= mods'
              else do
                activeTasks . listElementsL .= before <> (task{_failure = Just content} Seq.<| after)
          _ -> pure ()
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
handleEvent (AppEvent (SetTime t)) = currentTime .= t
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
  current <- use currentFocus
  case current of
    SessionSelector -> do
      let hide = currentFocus .= ModuleSelector
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        V.EvKey (V.KChar 's') [] -> hide
        _ -> zoom sessions (handleListEvent evt)
    OptionsEditor -> do
      let hide = do
            currentFocus .= ModuleSelector
            handleEvent (AppEvent SendOptions)
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> zoom options (handleFormEvent (VtyEvent evt))
    TaskDetails -> do
      let hide = currentFocus .= ActiveTasks
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> zoom (sessions . listSelectedElementL . _2 . activeTasks) (handleListEvent evt)
    ModuleDetails -> do
      let hide = currentFocus .= ModuleSelector
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> zoom (sessions . listSelectedElementL . _2 . modules) (handleListEvent evt)
    _ -> case evt of
      V.EvKey V.KEsc [] -> halt
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey (V.KChar 's') [] -> do
        currentFocus .= SessionSelector
      V.EvKey (V.KChar 'o') [] -> do
        currentFocus .= OptionsEditor
      V.EvKey (V.KChar '\t') [] -> do
        currentFocus .= case current of
          ActiveTasks -> ModuleSelector
          ModuleSelector -> ActiveTasks
          _ -> current
      V.EvKey V.KEnter [] -> do
        currentFocus .= case current of
          ActiveTasks -> TaskDetails
          ModuleSelector -> ModuleDetails
          _ -> current
      _ ->
        if current == ActiveTasks
          then zoom (sessions . listSelectedElementL . _2 . activeTasks) (handleListEvent evt)
          else zoom (sessions . listSelectedElementL . _2 . modules) (handleListEvent evt)
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
            , (listSelectedAttr, brightWhite `on` brightBlack)
            , (listSelectedFocusedAttr, brightWhite `on` blue)
            ]
    , appChooseCursor = showFirstCursor
    }