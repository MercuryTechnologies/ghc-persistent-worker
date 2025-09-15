{-# LANGUAGE DeriveAnyClass #-}
module Types.Instrument where

import Data.Binary (Binary)
import Data.Map (Map)
import GHC.Generics (Generic)

data Event
  = CompileStart { target :: String, canDebug :: Bool }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String }
  | Stats { memory :: Map String Int, cpuNs :: Int, gcCpuNs :: Int }
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)