module UI.ModuleSelector where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, vBox, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedL, renderList)
import Data.Fixed (Fixed (..), Pico)
import Data.Sequence qualified as Seq
import Lens.Micro.Platform (modifying, use, (.=))
import UI.Types (Name (ModuleSelector))
import UI.Utils (formatPico, formatPs, popup, upsertAscSeq)

type State = GenericList Name Seq.Seq Module

initialState :: State
initialState = list ModuleSelector Seq.empty 1

data Module = Module
  { _modName :: String
  , _content :: String
  , _modCompileTime :: Maybe Pico
  }

draw :: Name -> State -> Widget Name
draw current = renderList drawModule (current == ModuleSelector)
 where
  drawModule _ Module{..} =
    padRight Max (str _modName) <+> str (maybe "" formatPico _modCompileTime)

drawModuleDetails :: Module -> Widget Name
drawModuleDetails Module{..} =
  popup 70 _modName $
    vBox
      [ str $ "Compile time: " ++ maybe "" (formatPs . (\(MkFixed n) -> n)) _modCompileTime
      , strWrap _content
      ]

addModule :: String -> String -> Maybe Pico -> EventM Name State ()
addModule "" _ _ = pure () -- TODO: Filter out earlier
addModule name content compileTime = do
  mods <- use listElementsL
  let (i, mods') = upsertAscSeq _modName (Module name content compileTime) mods
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))
  listElementsL .= mods'
