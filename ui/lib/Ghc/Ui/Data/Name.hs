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
  deriving stock (Eq, Ord, Show)
