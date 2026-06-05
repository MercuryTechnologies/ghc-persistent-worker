module Types.BuildPlan.Incremental (
  BuckHashesPath (..),
) where

import System.OsPath.Extra (OsPath)

newtype BuckHashesPath =
  BuckHashesPath { path :: OsPath }
  deriving stock (Eq, Show, Ord)
