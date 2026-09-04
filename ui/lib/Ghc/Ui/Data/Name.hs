module Ghc.Ui.Data.Name where

data Name =
  Tasks
  |
  TaskDetails
  |
  ModuleSelector
  |
  ModuleDetails
  |
  Sessions
  |
  OptionsEditor
  |
  OEExtraGhcOptions
  deriving stock (Eq, Ord, Show)
