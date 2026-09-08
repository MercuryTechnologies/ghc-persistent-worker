module Ghc.Ui.Event.Sessions where

import Brick.Types (EventM)
import Brick.Widgets.List (listElementsL, listSelectedL)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence qualified as Seq
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Ghc.Ui.Data.Name (Name)
import qualified Ghc.Ui.Data.Session as Session
import Ghc.Ui.Data.Session (SessionState (..))
import Ghc.Ui.Data.Sessions (SessionsEvent (..), SessionsState)
import qualified Ghc.Ui.Event.Session as Session
import Lens.Micro.Platform (Traversal', _2, at, each, filtered, modifying, preuse, zoom, (.=), (?=))

sessionLens :: Session.Id -> Traversal' SessionsState SessionState
sessionLens sid =
  listElementsL . each . filtered ((== sid) . fst) . _2

handleEvent :: SessionsEvent -> EventM Name SessionsState ()
handleEvent (AddWorker sid wid time sendOpts) = do
  session <- preuse (sessionLens sid)
  case session of
    Nothing -> handleEvent (StartSession sid time)
    _ -> pure ()
  zoom (sessionLens sid) $ do
    #workers . at wid ?= Session.Worker wid sendOpts mempty
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
