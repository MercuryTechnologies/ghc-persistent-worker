module Types.GhcHandler where

-- | Selects the worker implementation.
data WorkerMode = WorkerMakeMode
  deriving stock (Eq, Show)
