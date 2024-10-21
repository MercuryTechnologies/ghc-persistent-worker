{-# language OverloadedRecordDot #-}

module Internal.Compile where

import Internal.AbiHash (readAbiHash)
import Internal.Args (Args (..), CompileResult (..))
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
