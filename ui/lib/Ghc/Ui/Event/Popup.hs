module Ghc.Ui.Event.Popup where

import Brick.Forms (Form, handleFormEvent)
import Brick.Types (BrickEvent (..), EventM)
import Brick.Widgets.List (GenericList, Splittable, handleListEvent, handleListEventVi)
import Control.Monad (unless)
import Ghc.Ui.Data.Main (MainState (..))
import Ghc.Ui.Data.Name (Name (..))
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform (Traversal', use, zoom, (.=))

handleListEventOf :: (Foldable t, Splittable t, Ord n) => Traversal' s (GenericList n t e) -> Event -> EventM n s ()
handleListEventOf lens = zoom lens . handleListEventVi handleListEvent

focus :: Name -> EventM Name MainState ()
focus target = #currentFocus .= target

openPopup :: Name -> EventM Name MainState ()
openPopup target = do
  current <- use #currentFocus
  #previousFocus .= current
  focus target

closePopup :: EventM Name MainState ()
closePopup = do
  target <- use #previousFocus
  #currentFocus .= target

popupKeyEvent ::
  Bool ->
  (Event -> EventM Name MainState ()) ->
  EventM Name MainState () ->
  Event ->
  EventM Name MainState ()
popupKeyEvent enter fallback finalize = \case
  EvKey KEsc [] -> do
    closePopup
    unless enter do
      finalize
  EvKey KEnter [] | enter -> do
    closePopup
    finalize
  event -> fallback event

listKeyEvent ::
  Foldable t =>
  Splittable t =>
  Traversal' MainState (GenericList Name t e) ->
  Bool ->
  Event ->
  EventM Name MainState ()
listKeyEvent lens enter =
  popupKeyEvent enter (handleListEventOf lens) (pure ())

-- | @handleFormEvent@ updates the text in the lens immediately after each key press
handleForm ::
  Traversal' MainState (Form s e Name) ->
  Event ->
  EventM Name MainState ()
handleForm lens =
  zoom lens . handleFormEvent . VtyEvent
