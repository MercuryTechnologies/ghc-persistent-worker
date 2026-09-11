module Ghc.Ui.Data.Tasks where

import Brick.Widgets.List (GenericList, list)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Ghc.Ui.Data.Name (Name (Tasks))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Types.Api (RequestId, Target)

data Task =
  Task {
    target :: Target,
    -- | The worker request that compiles the target, which tells apart two compilations of the same target.
    -- 'Nothing' for a rebuild that the worker did not accept.
    requestId :: Maybe RequestId,
    startTime :: UTCTime,
    failure :: Maybe String,
    worker :: WorkerId,
    debuggable :: Bool
  }

type TasksState = GenericList Name Seq Task

initialState :: TasksState
initialState = list Tasks [] 1
