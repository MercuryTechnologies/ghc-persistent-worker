module Ghc.Ui.Event.Main where

import Brick.Forms (Form, FormFieldState, editTextField, formState, handleFormEvent, newForm, (@@=))
import Brick.Main (getVtyHandle, halt, suspendAndResume')
import Brick.Types (BrickEvent (..), EventM)
import Brick.Widgets.Core (str, (<+>))
import Brick.Widgets.List (listSelectedElementL)
import Control.Exception (handle)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Monoid (First (..))
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Generics (Generic)
import Ghc.Ui.ActiveTasks qualified as ActiveTasks
import Ghc.Ui.Data.ServerApi (ServerApi (..))
import Ghc.Ui.GhcDebug (debug)
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Ghc.Ui.Session qualified as Session
import Ghc.Ui.Session (Worker (..))
import Ghc.Ui.SessionSelector qualified as SessionSelector
import Ghc.Ui.Types (Name (..), WorkerId)
import Ghc.Ui.Utils (handleListEventOf)
import Graphics.Vty (Event (..), Key (..), Output (..), Vty (..))
import Internal.Debug (debugSocketPath)
import Lens.Micro.Platform (Lens', Traversal', _2, each, filtered, lens, packed, preuse, use, zoom, (.=))
import Types.State (Options (..), defaultOptions)
import Types.Target (TargetSpec)

data MainEvent =
  SendOptions (Maybe WorkerId)
  |
  SetTime UTCTime
  |
  SessionSelectorEvent SessionSelector.Event
  |
  TriggerRebuild WorkerId TargetSpec

data MainState =
  MainState {
    sessions :: SessionSelector.State,
    options :: Form Options Event Name,
    currentFocus :: Name,
    currentTime :: UTCTime
  }
  deriving stock (Generic)

ghcOptionsLens :: Lens' Options Text.Text
ghcOptionsLens =
  lens
    (.extraGhcOptions)
    (\opts s -> opts{extraGhcOptions = s})
    . packed

initialState :: MainState
initialState =
  MainState {
    sessions = SessionSelector.initialState,
    options = newForm optionFields defaultOptions,
    currentFocus = ModuleSelector,
    currentTime = UTCTime (fromGregorian 1970 1 1) 0
  }

optionFields :: [Options -> FormFieldState Options Event Name]
optionFields =
  [ (str "Extra GHC Options: " <+>) @@= editTextField ghcOptionsLens OEExtraGhcOptions (Just 1)
  ]

currentSession :: Traversal' MainState Session.State
currentSession = #sessions . listSelectedElementL . _2

beep :: EventM Name MainState ()
beep = do
  vty <- getVtyHandle
  liftIO $ vty.outputIface.ringTerminalBell

withTarget' :: Bool -> (WorkerId -> TargetSpec -> EventM Name MainState ()) -> EventM Name MainState ()
withTarget' forRebuild handler = do
  current <- use #currentFocus
  First mtarget <- case current of
    ActiveTasks -> zoom (currentSession . #activeTasks) (First <$> ActiveTasks.getSelectedTarget)
    ModuleSelector -> zoom (currentSession . #modules) (First <$> ModuleSelector.getSelectedTarget forRebuild)
    _ -> pure (First Nothing)
  case mtarget of
    Nothing -> beep
    Just (wid, target) -> handler wid target

withTarget :: (WorkerId -> TargetSpec -> EventM Name MainState ()) -> EventM Name MainState ()
withTarget = withTarget' False

withTargetForRebuild :: (WorkerId -> TargetSpec -> EventM Name MainState ()) -> EventM Name MainState ()
withTargetForRebuild = withTarget' True

handleEvent :: ServerApi -> BrickEvent Name MainEvent -> EventM Name MainState ()
handleEvent api@ServerApi {..} = \case
  (AppEvent (SetTime t)) ->
    #currentTime .= t
  AppEvent (SendOptions mwid) -> do
    opts <- use #options
    workers <- use (currentSession . #workers)
    let workers' = case mwid of
          Nothing -> workers
          Just wid -> filter (\ w -> w.workerId == wid) (workers :: [Worker])
    for_ workers' $ \worker -> do
      liftIO $
        handle @IOError (\_ -> pure ()) $
          sendOptions worker.connection (formState opts)
  AppEvent (TriggerRebuild wid target) -> do
    mworker <- preuse (currentSession . #workers . each . filtered (\ w -> w.workerId == wid))
    for_ mworker $ \worker -> do
      liftIO $ triggerRebuild worker.connection target
  AppEvent (SessionSelectorEvent evt) ->
    zoom #sessions (SessionSelector.handleEvent evt)
  VtyEvent evt -> do
    current <- use #currentFocus
    case current of
      SessionSelector -> do
        let hide = #currentFocus .= ModuleSelector
        case evt of
          EvKey KEsc [] -> hide
          EvKey KEnter [] -> hide
          EvKey (KChar 's') [] -> hide
          _ -> handleListEventOf #sessions evt
      OptionsEditor -> do
        let
          hide = do
            #currentFocus .= ModuleSelector
            handleEvent api (AppEvent (SendOptions Nothing))
        case evt of
          EvKey KEsc [] -> hide
          EvKey KEnter [] -> hide
          _ -> zoom #options (handleFormEvent (VtyEvent evt))
      TaskDetails -> do
        let hide = #currentFocus .= ActiveTasks
        case evt of
          EvKey KEsc [] -> hide
          EvKey KEnter [] -> hide
          _ -> handleListEventOf (currentSession . #activeTasks) evt
      ModuleDetails -> do
        let hide = #currentFocus .= ModuleSelector
        case evt of
          EvKey KEsc [] -> hide
          EvKey KEnter [] -> hide
          _ -> handleListEventOf (currentSession . #modules) evt
      _ -> case evt of
        EvKey KEsc [] -> halt
        EvKey (KChar 'q') [] -> halt
        EvKey (KChar 's') [] -> do
          #currentFocus .= SessionSelector
        EvKey (KChar 'o') [] -> do
          #currentFocus .= OptionsEditor
        EvKey (KChar 'd') [] -> do
          withTarget $ \_wid target ->
            suspendAndResume' $
              debug (debugSocketPath target)
        EvKey (KChar 'r') [] -> do
          withTargetForRebuild $ \wid target ->
            handleEvent api (AppEvent (TriggerRebuild wid target))
        EvKey (KChar '\t') [] -> do
          #currentFocus .= case current of
            ActiveTasks -> ModuleSelector
            ModuleSelector -> ActiveTasks
            _ -> current
        EvKey KEnter [] -> do
          withTarget \_ _ ->
            #currentFocus .= case current of
              ActiveTasks -> TaskDetails
              ModuleSelector -> ModuleDetails
              _ -> current
        _ -> case current of
          ActiveTasks -> handleListEventOf (currentSession . #activeTasks) evt
          ModuleSelector -> handleListEventOf (currentSession . #modules) evt
          _ -> pure ()
  MouseDown {} -> pure ()
  MouseUp {} -> pure ()
