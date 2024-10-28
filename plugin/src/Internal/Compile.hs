module Internal.Compile where

import Control.Monad.IO.Class (liftIO)
import GHC (Ghc, Phase, getSession)
import GHC.Driver.Phases (StopPhase (NoStop))
import GHC.Driver.Pipeline (oneShot)

compile :: [(String, Maybe Phase)] -> Ghc ()
compile srcs = do
  hsc_env <- getSession
  liftIO (oneShot hsc_env NoStop srcs)
