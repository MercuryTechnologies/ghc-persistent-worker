module Ghc.Ui.Data.WorkerId where

import Data.Text (Text)

newtype WorkerId =
  WorkerId { text :: Text }
  deriving stock (Eq, Ord, Show)
