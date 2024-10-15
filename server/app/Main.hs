module Main where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (replicateM_)
import qualified Data.IntMap as IM
import Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as OA
import Pool (Pool (..))
import Server (runServer, serve, spawnWorker)

-- cli args
data Option = Option
  { optionNumWorkers :: Int,
    optionGHC :: FilePath,
    optionSocket :: FilePath,
    optionPkgDbs :: [FilePath]
  }

p_option :: Parser Option
p_option =
  Option
    <$> OA.option OA.auto
        ( OA.long "num"
            <> OA.short 'n'
            <> OA.help "number of workers"
        )
    <*> OA.strOption
        ( OA.long "ghc"
            <> OA.help "GHC path"
        )
    <*> OA.strOption
        ( OA.long "socket-file"
            <> OA.help "UNIX socket file accepting compilation requests"
        )
    <*> OA.many (OA.strOption (OA.long "package-db" <> OA.help "Package DB Path"))

main :: IO ()
main = do
  opts <- OA.execParser (OA.info (p_option <**> OA.helper) OA.fullDesc)
  let n = optionNumWorkers opts
      ghcPath = optionGHC opts
      socketPath = optionSocket opts
      dbPaths = optionPkgDbs opts
  let thePool = Pool
        { poolLimit = n,
          poolNewWorkerId = 1,
          poolNewJobId = 1,
          poolStatus = IM.empty,
          poolHandles = []
        }

  ref <- newTVarIO thePool
  replicateM_ n $ spawnWorker ghcPath dbPaths ref
  runServer socketPath (serve ghcPath dbPaths ref)
