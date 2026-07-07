module InterfacePathTest where

import Control.Monad.IO.Class (liftIO)
import GHC (mkModuleName)
import GHC.Driver.DynFlags (DynFlags (..))
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Main (initHscEnv)
import GHC.Paths (libdir)
import Hedgehog (TestT, (===))
import Internal.Cache.Hpt (canonicalInterfacePath)
import System.OsPath.Extra (toOsPath)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)

test_canonicalInterfacePath :: TestT IO ()
test_canonicalInterfacePath = do
  dflags0 <- liftIO ((.hsc_dflags) <$> initHscEnv (Just libdir))
  let
    dflags = dflags0 {hiDir = Just "out", hiSuf_ = "dyn_hi", dynHiSuf_ = "dyn_dyn_hi"}
    path d name = canonicalInterfacePath d (mkModuleName name)
  Just (toOsPath "out/Data/Vector.dyn_hi") === path dflags "Data.Vector"
  Just (toOsPath "out/Data/Vector.dyn_hi-boot") === path dflags "Data.Vector-boot"
  Just (toOsPath "out/Data/Vector.dyn_dyn_hi") === path dflags {dynamicNow = True} "Data.Vector"
  Nothing === path dflags {hiDir = Nothing} "Data.Vector"

test_interfacePath :: TestTree
test_interfacePath =
  testGroup "interface path" [
    unitTest "successful" test_canonicalInterfacePath
  ]
