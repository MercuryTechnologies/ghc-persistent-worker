module UI.ModuleSelector where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, vBox, (<+>), withAttr)
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedElementL, listSelectedL, renderList)
import Control.Monad (when)
import Data.Fixed (Fixed (..), Pico)
import Data.Sequence qualified as Seq
import Lens.Micro.Platform (modifying, preuse, use, (.=))
import Types.State (Target (..))
import UI.Types (Name (ModuleSelector), WorkerId, disabledAttr)
import UI.Utils (formatPico, formatPs, popup, upsertAscSeq)

type State = GenericList Name Seq.Seq Module

initialState :: State
initialState = list ModuleSelector Seq.empty 1

data Module = Module
  { _modTarget :: Target
  , _content :: String
  , _modCompileTime :: Maybe Pico
  , _fromWorker :: WorkerId
  , _disabled :: Bool
  }

draw :: Name -> State -> Widget Name
draw current = renderList drawModule (current == ModuleSelector)
 where
  drawModule _ Module{_modTarget = Target name, ..} =
    (if _disabled then withAttr disabledAttr else id) $
      padRight Max (str name) <+> str (maybe "" formatPico _modCompileTime)

drawModuleDetails :: Module -> Widget Name
drawModuleDetails Module{_modTarget = Target name, ..} =
  popup 70 name $
    vBox
      [ str $ "Compile time: " ++ maybe "" (formatPs . (\(MkFixed n) -> n)) _modCompileTime
      , strWrap _content
      ]

addModule :: Target -> String -> Maybe Pico -> WorkerId -> EventM Name State ()
addModule (Target "") _ _ _ = pure () -- TODO: Filter out earlier
addModule target content compileTime wid = do
  mods <- use listElementsL
  let (i, mods') = upsertAscSeq _modTarget (Module target content compileTime wid False) mods
  listElementsL .= mods'
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

getSelectedTarget :: Bool -> EventM Name State (Maybe (WorkerId, Target))
getSelectedTarget forRebuild = do
  mtask <- preuse listSelectedElementL
  when forRebuild $ modifying listSelectedElementL (\m -> m {_disabled = True})
  pure $ mtask >>= \Module{_fromWorker = wid, _modTarget = target, _disabled} -> if forRebuild && _disabled then Nothing else Just (wid, target)

removeWorker :: WorkerId -> EventM Name State ()
removeWorker wid = do
  modifying listElementsL \mods ->
    fmap (\m -> if m._fromWorker == wid then m{_disabled = True} else m) mods
