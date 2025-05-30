{-# LANGUAGE TemplateHaskell #-}

module UI (module UI, customMainWithDefaultVty) where

import Brick.AttrMap (attrMap)
import Brick.Forms (Form, FormFieldState, editTextField, formState, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Main (App (..), customMainWithDefaultVty, halt, showFirstCursor)
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (joinBorders, modifyDefAttr, str, vBox, withBorderStyle, (<+>))
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (handleListEvent, listSelectedAttr, listSelectedElement, listSelectedElementL, listSelectedFocusedAttr)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Internal.Cache (Options (..), defaultOptions)
import Lens.Micro.Platform (Lens', lens, makeLenses, packed, preuse, use, zoom, (.=), _2)
import UI.ActiveTasks qualified as ActiveTasks
import UI.ModuleSelector qualified as ModuleSelector
import UI.Session qualified as Session
import UI.SessionSelector qualified as SessionSelector
import UI.Types (Name (..))
import UI.Utils (popup)

data Event
  = SendOptions
  | SetTime UTCTime
  | SessionSelectorEvent SessionSelector.Event

data State = State
  { _sessions :: SessionSelector.State
  , _options :: Form Options Event Name
  , _currentFocus :: Name
  , _currentTime :: UTCTime
  }

makeLenses ''State

ghcOptionsLens :: Lens' Options Text.Text
ghcOptionsLens =
  lens
    ((.extraGhcOptions))
    (\opts s -> opts{extraGhcOptions = s})
    . packed

initialState :: State
initialState =
  State
    { _sessions = SessionSelector.initialState
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
      SessionSelector -> [SessionSelector.draw _sessions]
      OptionsEditor -> [drawOptionsEditor _options]
      TaskDetails -> let task = session >>= listSelectedElement . Session._activeTasks in maybe [] (pure . ActiveTasks.drawTaskDetails . snd) task
      ModuleDetails -> let mdl = session >>= listSelectedElement . Session._modules in maybe [] (pure . ModuleSelector.drawModuleDetails . snd) mdl
      _ -> []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                  (Session.draw _currentFocus _currentTime)
                  session
          , modifyDefAttr (`V.withStyle` V.italic) $ str " q:quit   Enter:show details   s:toggle session selector   o:toggle options editor "
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement _sessions

drawOptionsEditor :: Form Options Event Name -> Widget Name
drawOptionsEditor form = popup 50 "Session Options" $ renderForm form

handleEvent :: BrickEvent Name Event -> EventM Name State ()
handleEvent (AppEvent (SetTime t)) = currentTime .= t
handleEvent (AppEvent SendOptions) = do
  opts <- use options
  sendOpts <- preuse (sessions . listSelectedElementL)
  for_ sendOpts $ \(_, s) -> do
    liftIO $ Session._sendOptions s (formState opts)
handleEvent (AppEvent (SessionSelectorEvent evt)) =
  zoom sessions (SessionSelector.handleEvent evt)
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
        _ -> zoom (sessions . listSelectedElementL . _2 . Session.activeTasks) (handleListEvent evt)
    ModuleDetails -> do
      let hide = currentFocus .= ModuleSelector
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> zoom (sessions . listSelectedElementL . _2 . Session.modules) (handleListEvent evt)
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
          then zoom (sessions . listSelectedElementL . _2 . Session.activeTasks) (handleListEvent evt)
          else zoom (sessions . listSelectedElementL . _2 . Session.modules) (handleListEvent evt)
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