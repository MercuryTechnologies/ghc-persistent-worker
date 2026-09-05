module Ghc.Ui.Data.Main where

import Brick.Forms (Form)
import Data.Time (UTCTime (..))
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.Sessions (SessionsEvent, SessionsState)
import Ghc.Ui.Data.WorkerId (WorkerId)
import Graphics.Vty (Event (..))
import Types.State (Options (..))
import Types.Target (TargetSpec)

data MainEvent =
  SendOptions (Maybe WorkerId)
  |
  SetTime UTCTime
  |
  SessionSelectorEvent SessionsEvent
  |
  TriggerRebuild WorkerId TargetSpec

data MainState =
  MainState {
    sessions :: SessionsState,
    options :: Form Options Event Name,
    currentFocus :: Name,
    previousFocus :: Name,
    currentTime :: UTCTime
  }
  deriving stock (Generic)
