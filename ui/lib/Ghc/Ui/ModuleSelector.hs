module Ghc.Ui.ModuleSelector where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, vBox, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedElementL, listSelectedL, renderList)
import Control.Monad (when)
import Data.Fixed (Fixed (..), Pico)
import Data.Sequence qualified as Seq
import Ghc.Ui.Attr (disabledAttr)
import Ghc.Ui.Data.Name (Name (ModuleSelector))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Ghc.Ui.Render.Format (formatPico, formatPs)
import Ghc.Ui.Utils (popup, upsertAscSeq)
import Lens.Micro.Platform (modifying, preuse, use, (.=))
import Types.Target (TargetSpec (..), renderTargetSpec)

type State = GenericList Name Seq.Seq Module

initialState :: State
initialState = list ModuleSelector Seq.empty 1

data Module =
  Module {
    modTarget :: TargetSpec,
    content :: String,
    modCompileTime :: Maybe Pico,
    fromWorker :: WorkerId,
    disabled :: Bool
  }

draw :: Name -> State -> Widget Name
draw current = renderList drawModule (current == ModuleSelector)
 where
  drawModule _ Module{modTarget = name, ..} =
    (if disabled then withAttr disabledAttr else id) $
      padRight Max (str (renderTargetSpec name)) <+> str (maybe "" formatPico modCompileTime)

drawModuleDetails :: Module -> Widget Name
drawModuleDetails Module{modTarget = name, ..} =
  popup 70 (renderTargetSpec name) $
    vBox
      [ str $ "Compile time: " ++ maybe "" (formatPs . (\(MkFixed n) -> n)) modCompileTime
      , strWrap content
      ]

addModule :: TargetSpec -> String -> Maybe Pico -> WorkerId -> EventM Name State ()
addModule target content compileTime wid = do
  mods <- use listElementsL
  let (i, mods') = upsertAscSeq (.modTarget) (Module target content compileTime wid False) mods
  listElementsL .= mods'
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

getSelectedTarget :: Bool -> EventM Name State (Maybe (WorkerId, TargetSpec))
getSelectedTarget forRebuild = do
  mtask <- preuse listSelectedElementL
  when forRebuild $ modifying listSelectedElementL (\m -> m {disabled = True})
  pure $ mtask >>= \Module{fromWorker = wid, modTarget = target, disabled} -> if forRebuild && disabled then Nothing else Just (wid, target)

removeWorker :: WorkerId -> EventM Name State ()
removeWorker wid = do
  modifying listElementsL \mods ->
    fmap (\m -> if m.fromWorker == wid then m{disabled = True} else m) mods
