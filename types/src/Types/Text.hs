module Types.Text where

import qualified Data.Text as Text
import Data.Text (Text)

showText :: Show a => a -> Text
showText = Text.pack . show
