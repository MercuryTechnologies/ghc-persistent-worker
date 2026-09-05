module Ghc.Ui.Event.Main where

import Brick.Forms (FormFieldState, editTextField, formState, newForm, (@@=))
import Brick.Main (getVtyHandle, halt, suspendAndResume')
import Brick.Types (BrickEvent (..), EventM)
import Brick.Widgets.Core (txt, (<+>))
import Brick.Widgets.List (listSelectedElementL)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Monoid (First (..))
import Data.Time (UTCTime (..), fromGregorian)
import Ghc.Ui.Data.Main (MainEvent (..), MainState (..))
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.ServerApi (ServerApi (..))
import Ghc.Ui.Data.Session (SessionState, Worker (..))
import qualified Ghc.Ui.Data.Sessions as Sessions
import Ghc.Ui.Data.WorkerId (WorkerId (..))
import Ghc.Ui.Event.Popup (focus, handleForm, handleListEventOf, listKeyEvent, openPopup, popupKeyEvent)
import qualified Ghc.Ui.Event.Sessions as Sessions
import Ghc.Ui.Event.Tasks qualified as Tasks
import Ghc.Ui.GhcDebug (debug)
import Ghc.Ui.ModuleSelector qualified as ModuleSelector
import Graphics.Vty (Event (..), Key (..), Output (..), Vty (..))
import Internal.Debug (debugSocketPath)
import Lens.Micro.Platform (Traversal', _2, packed, use, zoom, (.=))
import Types.State (Options (..), defaultOptions)
import Types.Target (TargetSpec)

initialState :: MainState
initialState =
  MainState {
    sessions = Sessions.initialState,
    options = newForm optionFields defaultOptions,
    currentFocus = ModuleSelector,
    previousFocus = ModuleSelector,
    currentTime = UTCTime (fromGregorian 1970 1 1) 0
  }

optionFields :: [Options -> FormFieldState Options Event Name]
optionFields =
  [(txt "Extra GHC Options: " <+>) @@= editTextField (#extraGhcOptions . packed) OEExtraGhcOptions (Just 1)]

currentSession :: Traversal' MainState SessionState
currentSession = #sessions . listSelectedElementL . _2

-- TODO replace with op log
beep :: EventM n s ()
beep = do
  vty <- getVtyHandle
  liftIO $ vty.outputIface.ringTerminalBell

withTarget' :: Bool -> (WorkerId -> TargetSpec -> EventM Name MainState ()) -> EventM Name MainState ()
withTarget' forRebuild handler = do
  current <- use #currentFocus
  First mtarget <- case current of
    Tasks -> zoom (currentSession . #activeTasks) (First <$> Tasks.getSelectedTarget)
    ModuleSelector -> zoom (currentSession . #modules) (First <$> ModuleSelector.getSelectedTarget forRebuild)
    _ -> pure (First Nothing)
  case mtarget of
    Nothing -> beep
    Just (wid, target) -> handler wid target

withTarget :: (WorkerId -> TargetSpec -> EventM Name MainState ()) -> EventM Name MainState ()
withTarget = withTarget' False

withTargetForRebuild :: (WorkerId -> TargetSpec -> EventM Name MainState ()) -> EventM Name MainState ()
withTargetForRebuild = withTarget' True

selectWorkers :: Maybe WorkerId -> Map WorkerId Worker -> [Worker]
selectWorkers = \case
  Just target -> maybeToList . Map.lookup target
  Nothing -> Map.elems

forWorkers ::
  Maybe WorkerId ->
  (Worker -> EventM Name MainState ()) ->
  EventM Name MainState ()
forWorkers spec f = do
  workers <- selectWorkers spec <$> use (currentSession . #workers)
  traverse_ f workers

sendOptions ::
  ServerApi ->
  Maybe WorkerId ->
  EventM Name MainState ()
sendOptions api spec = do
  opts <- use #options
  forWorkers spec \ worker ->
    liftIO $ api.sendOptions worker.connection (formState opts)

triggerBuild :: ServerApi -> WorkerId -> TargetSpec -> EventM Name MainState ()
triggerBuild api workerId target = do
  forWorkers (Just workerId) \ worker ->
    liftIO $ api.triggerRebuild worker.connection target

handleMainEvent :: ServerApi -> MainEvent -> EventM Name MainState ()
handleMainEvent api = \case
  SetTime t ->
    #currentTime .= t

  SendOptions target ->
    sendOptions api target

  TriggerRebuild worker target ->
    triggerBuild api worker target

  SessionSelectorEvent evt ->
    zoom #sessions (Sessions.handleEvent evt)

handleGlobalKey :: ServerApi -> Name -> Event -> Key -> EventM Name MainState ()
handleGlobalKey api current event = \case
  KEsc -> halt

  KChar 'q' -> halt

  KChar 's' ->
    openPopup Sessions

  KChar 'o' ->
    openPopup OptionsEditor

  KChar 'd' ->
    withTarget \ _ target ->
      suspendAndResume' $ debug (debugSocketPath target)

  KChar 'r' ->
    withTargetForRebuild \ wid target ->
      handleEvent api (AppEvent (TriggerRebuild wid target))

  KChar '\t' ->
    focus case current of
      Tasks -> ModuleSelector
      ModuleSelector -> Tasks
      _ -> current

  KEnter ->
    -- This ensures the cursor is not on a separator
    -- TODO improve
    withTarget \ _ _ ->
      openPopup case current of
        Tasks -> TaskDetails
        ModuleSelector -> ModuleDetails
        _ -> current

  _ -> case current of
    Tasks -> handleListEventOf (currentSession . #activeTasks) event
    ModuleSelector -> handleListEventOf (currentSession . #modules) event
    _ -> pure ()

vtyEvent :: ServerApi -> Name -> Event -> EventM Name MainState ()
vtyEvent api = \case
  Sessions ->
    listKeyEvent #sessions True

  OptionsEditor ->
    popupKeyEvent True (handleForm #options) do
      sendOptions api Nothing

  -- TODO why does this use a list event handler? (and ModuleDetails)
  TaskDetails ->
    listKeyEvent (currentSession . #activeTasks) False

  ModuleDetails ->
    listKeyEvent (currentSession . #modules) False

  current -> \case
    event@(EvKey key []) -> handleGlobalKey api current event key
    _ -> pure ()

handleEvent :: ServerApi -> BrickEvent Name MainEvent -> EventM Name MainState ()
handleEvent api = \case
  AppEvent event -> handleMainEvent api event
  VtyEvent event -> do
    current <- use #currentFocus
    vtyEvent api current event
  MouseDown {} -> pure ()
  MouseUp {} -> pure ()
