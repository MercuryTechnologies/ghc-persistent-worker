module UI.SessionSelector where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedL, renderList)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Internal.Cache (Options)
import Lens.Micro.Platform (Traversal', each, filtered, modifying, zoom, (.=), _2)
import UI.Session qualified as Session
import UI.Types (Name (SessionSelector))
import UI.Utils (popup)

type State = GenericList Name Seq.Seq (Session.Id, Session.State)

data Event
  = StartSession Session.Id UTCTime (Options -> IO ())
  | EndSession Session.Id UTCTime
  | SessionEvent Session.Id Session.Event

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
        , maybe "Running..." (take 19 . iso8601Show) _sesEndTime
        ]

sessionLens :: Session.Id -> Traversal' State (Session.State)
sessionLens sid =
  listElementsL . each . filtered ((== sid) . fst) . _2

handleEvent :: Event -> EventM Name State ()
handleEvent (StartSession sid start sendOpts) = do
  modifying
    (listElementsL)
    ( \m ->
        let i = Seq.length m + 1
            stitle = "Session " ++ show i ++ "  " ++ take 19 (iso8601Show start)
         in Seq.insertAt 0 (sid, Session.mkSession stitle start sendOpts) m
    )
  listSelectedL .= Just 0
handleEvent (EndSession sid end) = do
  modifying (sessionLens sid . Session.sesEndTime) (const $ Just end)
handleEvent (SessionEvent sid evt) = zoom (sessionLens sid) (Session.handleEvent evt)