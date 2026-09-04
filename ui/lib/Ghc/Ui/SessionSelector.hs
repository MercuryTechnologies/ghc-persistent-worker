module Ghc.Ui.SessionSelector where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedL, renderList)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Ghc.Ui.Data.Session as Session
import Ghc.Ui.Data.Session (SessionEvent, SessionState (..))
import Ghc.Ui.Session qualified as Session
import Ghc.Ui.Types (Name (SessionSelector), WorkerId)
import Ghc.Ui.Utils (popup)
import Lens.Micro.Platform (Traversal', _2, each, filtered, modifying, preuse, zoom, (.=))
import Network.GRPC.Client (Connection)

type State = GenericList Name Seq (Session.Id, SessionState)

data Event
  = StartSession Session.Id UTCTime
  | EndSession Session.Id
  | Session Session.Id SessionEvent
  | AddWorker Session.Id WorkerId UTCTime Connection
  | RemoveWorker Session.Id WorkerId

initialState :: State
initialState = list SessionSelector [] 1

draw :: State -> Widget Name
draw ss =
  popup 50 "Select session" $ renderList drawOption True ss
 where
  drawOption isSel (_, SessionState {..}) =
    str $
      concat @[]
        [ if isSel then "> " else "  "
        , title
        , " - "
        , show (length workers)
        , " workers"
        ]

sessionLens :: Session.Id -> Traversal' State SessionState
sessionLens sid =
  listElementsL . each . filtered ((== sid) . fst) . _2

handleEvent :: Event -> EventM Name State ()
handleEvent (AddWorker sid wid time sendOpts) = do
  session <- preuse (sessionLens sid)
  case session of
    Nothing -> handleEvent (StartSession sid time)
    _ -> pure ()
  zoom (sessionLens sid) $ do
    modifying #workers (Session.Worker wid sendOpts mempty :)
    modifying #sesStartTime (min time)
handleEvent (RemoveWorker sid wid) = do
  zoom (sessionLens sid) $ Session.removeWorker wid
handleEvent (StartSession sid start) = do
  modifying
    listElementsL
    ( \m ->
        let i = Seq.length m + 1
            stitle = "Session " ++ show i ++ "  " ++ take 19 (iso8601Show start)
         in Seq.insertAt 0 (sid, Session.initialState stitle start) m
    )
  listSelectedL .= Just 0
handleEvent (EndSession sid) = do
  end <- liftIO getCurrentTime
  modifying (sessionLens sid . #sesEndTime) (const $ Just end)
handleEvent (Session sid evt) = zoom (sessionLens sid) (Session.handleEvent evt)
