module Types.GhcHandler where

-- | Selects the worker implementation.
data WorkerMode =
  WorkerMakeMode
  |
  WorkerOneshotMode
  deriving stock (Eq, Show)
