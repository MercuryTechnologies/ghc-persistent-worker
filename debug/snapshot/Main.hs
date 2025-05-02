module Main where

import Data.Functor ((<&>))
import Data.List (sort)
import GHC.Debug.Client
import GHC.Debug.Snapshot
import System.Directory (getCurrentDirectory, getHomeDirectory, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  here <- getCurrentDirectory
  let path = here </> "snapshot"
  socket <- getArgs >>= \case
    [p] -> pure p
    _ -> do
      home <- getHomeDirectory
      let socketDir = home </> ".local/share/ghc-debug/debuggee/sockets"
      (reverse . sort <$> listDirectory socketDir) <&> \case
        [] -> "/tmp/ghc-debug"
        f : _ -> socketDir </> f
  putStrLn ("Connecting to socket at " ++ socket)
  withDebuggeeConnect socket \ d -> makeSnapshot d path
  putStrLn ("Wrote snapshot to " ++ path)
