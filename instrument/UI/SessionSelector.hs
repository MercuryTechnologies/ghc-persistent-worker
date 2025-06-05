module UI.SessionSelector where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedL, renderList)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Internal.State (Options)
import Lens.Micro.Platform (Traversal', each, filtered, modifying, preuse, zoom, (.=), _2)
import UI.Session qualified as Session
import UI.Types (Name (SessionSelector))
import UI.Utils (popup)

type State = GenericList Name Seq.Seq (Session.Id, Session.State)

data Event
  = StartSession Session.Id UTCTime
  | EndSession Session.Id
  | SessionEvent Session.Id Session.Event
  | AddWorker Session.Id Session.WorkerId UTCTime (Options -> IO ())
  | RemoveWorker Session.Id Session.WorkerId

initialState :: State
initialState = list SessionSelector Seq.empty 1

draw :: State -> Widget Name
draw ss =
  popup 50 "Select session" $ renderList drawOption True ss
 where
  drawOption isSel (_, Session.Session{..}) =
    str $
      concat @[]
        [ if isSel then "> " else "  "
        , _title
        , " - "
        , show (length _workers)
        , " workers"
        ]

sessionLens :: Session.Id -> Traversal' State (Session.State)
sessionLens sid =
  listElementsL . each . filtered ((== sid) . fst) . _2

handleEvent :: Event -> EventM Name State ()
handleEvent (AddWorker sid wid time sendOpts) = do
  session <- preuse (sessionLens sid)
  case session of
    Nothing -> handleEvent (StartSession sid time)
    _ -> pure ()
  zoom (sessionLens sid) $ do
    modifying Session.workers (Session.Worker wid sendOpts mempty :)
    modifying Session.sesStartTime (min time)
handleEvent (RemoveWorker sid wid) = do
  zoom (sessionLens sid) $ Session.removeWorker wid
handleEvent (StartSession sid start) = do
  modifying
    listElementsL
    ( \m ->
        let i = Seq.length m + 1
            stitle = "Session " ++ show i ++ "  " ++ take 19 (iso8601Show start)
         in Seq.insertAt 0 (sid, Session.mkSession stitle start) m
    )
  listSelectedL .= Just 0
handleEvent (EndSession sid) = do
  end <- liftIO getCurrentTime
  modifying (sessionLens sid . Session.sesEndTime) (const $ Just end)
handleEvent (SessionEvent sid evt) = zoom (sessionLens sid) (Session.handleEvent evt)
