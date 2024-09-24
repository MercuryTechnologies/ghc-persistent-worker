{-# language OverloadedRecordDot #-}

module Compile where

import AbiHash (readAbiHash)
import Args (Args (..), CompileResult (..))
import Control.Monad.IO.Class (liftIO)
import GHC (Ghc, Phase, getSession)
import GHC.Driver.Phases (StopPhase (NoStop))
import GHC.Driver.Pipeline (oneShot)

compile :: Args -> [(String, Maybe Phase)] -> Ghc (Maybe CompileResult)
compile args srcs = do
  hsc_env <- getSession
  liftIO (oneShot hsc_env NoStop srcs)
  abiHash <- readAbiHash hsc_env args.abiOut
  pure (Just CompileResult {abiHash})
