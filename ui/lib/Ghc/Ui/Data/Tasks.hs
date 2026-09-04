module Ghc.Ui.Data.Tasks where

import Brick.Widgets.List (GenericList, list)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Ghc.Ui.Data.Name (Name (Tasks))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Types.Target (TargetSpec (..))

data Task =
  Task {
    target :: TargetSpec,
    startTime :: UTCTime,
    failure :: Maybe String,
    worker :: WorkerId,
    debuggable :: Bool
  }

type TasksState = GenericList Name Seq Task

initialState :: TasksState
initialState = list Tasks [] 1
