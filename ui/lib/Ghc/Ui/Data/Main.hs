module Ghc.Ui.Data.Main where

import Data.Time (UTCTime (..))
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.Sessions (SessionsEvent, SessionsState)
import Ghc.Ui.Data.WorkerId (WorkerId)
import Types.Target (TargetSpec)

data MainEvent =
  SetTime UTCTime
  |
  SessionSelectorEvent SessionsEvent
  |
  TriggerRebuild WorkerId TargetSpec

data MainState =
  MainState {
    sessions :: SessionsState,
    currentFocus :: Name,
    previousFocus :: Name,
    currentTime :: UTCTime
  }
  deriving stock (Generic)
