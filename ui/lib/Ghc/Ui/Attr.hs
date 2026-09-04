module Ghc.Ui.Attr where

import Brick.AttrMap (AttrName, attrName)

disabledAttr :: AttrName
disabledAttr = attrName "disabled"

canDebugAttr :: AttrName
canDebugAttr = attrName "canDebug"
