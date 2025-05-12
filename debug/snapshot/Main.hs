module Main where

import GHC.Debug.Client
import GHC.Debug.Snapshot

main :: IO ()
main = withDebuggeeConnect "/tmp/ghc-debug" (\d -> makeSnapshot d "/home/tek/snapshot")
