module UI.Types where

data Name
  = ActiveTasks
  | TaskDetails
  | ModuleSelector
  | ModuleDetails
  | SessionSelector
  | OptionsEditor
  | OEExtraGhcOptions
  deriving stock (Eq, Ord, Show)
