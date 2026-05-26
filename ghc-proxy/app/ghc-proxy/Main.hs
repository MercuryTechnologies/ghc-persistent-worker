module Main where

import Control.Concurrent (MVar)
import Control.Exception (SomeException (..), displayException, try)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Internal.Log (dbg, newLogger)
import Internal.Metadata (proxyMetadata)
import Internal.State (newState)
import Prelude hiding (log)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Types.Args (Args (..))
import Types.BuckArgs (parseBuckArgs, toGhcArgs)
import Types.Env (Env (..))
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Log (Log, Logger (..), TraceId (..), newLog)

envFromArgs :: [String] -> IO (Env, MVar Log)
envFromArgs argv = do
  sourceHashes <- optionalEnv "buck_source_hashes"
  buckArgs <- either parseError pure (parseBuckArgs (CommandEnv sourceHashes) (RequestArgs argv))
  args <- toGhcArgs buckArgs Nothing
  state <- newState
  log <- newLog (TraceId . show <$> args.unit)
  pure (Env {log = newLogger log, state, args}, log)
  where
    parseError msg =
      error ("ghc-proxy: Parsing Buck args failed: " ++ msg)

    optionalEnv key = lookupEnv key <&> foldMap \ value -> [(key, value)]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  (env, _) <- envFromArgs =<< getArgs
  result <- try (proxyMetadata env)
  traverse_ dbg =<< env.log.flush
  case result of
    Right True ->
      pure ()
    Right False -> do
      hPutStrLn stderr ("ghc-proxy: Metadata failed without exception")
      exitFailure
    Left (err :: SomeException) -> do
      hPutStrLn stderr ("ghc-proxy: " ++ displayException err)
      exitFailure
