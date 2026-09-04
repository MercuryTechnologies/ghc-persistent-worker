module Ghc.Ui.Data.Main where

import Brick.Forms (Form)
import Data.Time (UTCTime (..))
import GHC.Generics (Generic)
import Ghc.Ui.SessionSelector qualified as SessionSelector
import Ghc.Ui.Types (Name (..), WorkerId)
import Graphics.Vty (Event (..))
import Types.State (Options (..))
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
