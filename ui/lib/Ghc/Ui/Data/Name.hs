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
  SessionSelector
  |
  OptionsEditor
  |
  OEExtraGhcOptions
  deriving stock (Eq, Ord, Show)
