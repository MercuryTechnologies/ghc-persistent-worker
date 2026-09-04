module Ghc.Ui.Data.Sessions where

import Brick.Widgets.List (GenericList, list)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Ghc.Ui.Data.Name (Name (Sessions))
import Ghc.Ui.Data.Session (Id, SessionEvent, SessionState (..))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Network.GRPC.Client (Connection)

type SessionsState = GenericList Name Seq (Id, SessionState)

data SessionsEvent =
  StartSession Id UTCTime
  |
  EndSession Id
  |
  Session Id SessionEvent
  |
  AddWorker Id WorkerId UTCTime Connection
  |
  RemoveWorker Id WorkerId

initialState :: SessionsState
initialState = list Sessions [] 1
