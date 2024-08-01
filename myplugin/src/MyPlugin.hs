module MyPlugin (frontendPlugin) where

import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Monad (Ghc)
import GHC.Driver.Phases (Phase)
import GHC.Driver.Plugins (FrontendPlugin (..), defaultFrontendPlugin)

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin
  {
    frontend = doNothing
  }

doNothing :: [String] -> [(String, Maybe Phase)] -> Ghc ()
doNothing flags args = do
    liftIO $ print flags
    liftIO $ print args

