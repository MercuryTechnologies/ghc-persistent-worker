module Main where

import GHC.Debug.Client
import GHC.Debug.Snapshot
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  here <- getCurrentDirectory
  let path = here </> "snapshot"
  withDebuggeeConnect "/tmp/ghc-debug" (\d -> makeSnapshot d path)
  putStrLn ("Wrote snapshot to " ++ path)
