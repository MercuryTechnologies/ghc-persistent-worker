module Incremental.Flow where

import Control.Concurrent (MVar, modifyMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (controlT)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (LazyByteString, fromStrict, toStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC (DynFlags (..), Ghc, GhcMode (..), getSession, getSessionDynFlags)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (TestT)
import Incremental.FlowData (Dep (..), Mod (..), PDep (..), Unit (..), jmn, unit1, unit2, utf8)
import Internal.Cache.Metadata (loadCachedUnits)
import Internal.DynFlags (buckLocation, modifyActiveUnitFlags)
import Internal.Metadata (prepareMetadataSession)
import Internal.Session (withDynFlags)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, osp, (<.>), (</>))
import System.OsPath.Extra (fromOsPath, toOsPath)
import Test.Run (testSession, testSessionSuccess)
import Test.Target (ghcOptions)
import Types.Args (Args (..), emptyArgs)
import Types.BuildPlan.Incremental (IncrementalState (..), IncrementalStatePath (..))
import Types.CachedDeps (
  CachedBuildPlan (..),
  CachedBuildPlans (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  )
import Types.Env (Env (..))
import Types.FeatureFlags (defaultFeatureFlags)
import Types.Log (Logger)
import Types.State (WorkerState)

cachedModule :: OsPath -> Mod -> CachedModule
cachedModule srcDir Mod {unit = munit, name, home, package} =
  CachedModule {
    source = srcDir </> toOsPath munit </> toOsPath name <.> [osp|hs|],
    modules = (.mname) <$> home,
    packages = [CachedPackageDep {id = unitId, modules = (.mname) <$> mods} | PDep {unitId, mods} <- package],
    flags = []
  }

writeUnitCache :: OsPath -> OsPath -> Unit -> IO CachedBuildPlans
writeUnitCache tmp srcDir unit = do
  OsPath.writeFile argsPath (fromStrict (utf8 (unlines (ghcOptions unit.id.raw []))))
  Aeson.encodeFile (fromOsPath cachedUnitPath) cachedUnit
  pure (CachedBuildPlans [CachedBuildPlan unit1.id cachedUnitPath])
  where
    argsPath = tmp </> toOsPath (unit.name ++ "-args")

    cachedUnitPath = tmp </> toOsPath (unit.name ++ "-cached-unit.json")

    cachedUnit =
      CachedUnit {
        build_plan = Just modules,
        cache = Nothing,
        unit_args = Just argsPath,
        unit_buck_args = Nothing,
        dep_units = Nothing
      }

    modules = Map.fromList [(jmn m.name, cachedModule srcDir m) | m <- unit.mods]

-- | Decode the incremental state file written next to a build plan.
readIncrementalState :: IncrementalStatePath -> IO IncrementalState
readIncrementalState (IncrementalStatePath path) =
  either fail pure =<< Aeson.eitherDecodeFileStrict' (fromOsPath path)

replaceTemp :: OsPath -> LazyByteString -> LazyByteString
replaceTemp temp =
  fromStrict . encodeUtf8 . Text.replace (Text.pack (fromOsPath temp)) "temp" . decodeUtf8 . toStrict

restoreUnit ::
  HasCallStack =>
  MVar WorkerState ->
  CachedBuildPlans ->
  TestT IO ()
restoreUnit state plans =
  withFrozenCallStack do
    testSession "restore unit" state \ env -> do
      hsc_env <- getSession
      dflags0 <- getSessionDynFlags
      -- TODO this is also called in prepareMetadataSession, maybe it's superfluous
      --
      -- Also in loadCachedUnits, unit1 is determined to be missing in both invocations
      --
      -- Also runMetadata is executed twice, so there could be some weirdness with controlT and TestT
      --
      -- It is, the error happens in rebuild but it shows another exception in initial
      void $ liftIO $ modifyMVar state \ s -> loadCachedUnits env.log dflags0 plans defaultFeatureFlags (s, hsc_env)

runMetadata :: String -> MVar WorkerState -> (Logger -> TestT Ghc a) -> TestT IO a
runMetadata desc state prog =
  testSessionSuccess ("metadata " ++ desc) state args \ env -> do
    controlT \ lowerT ->
      flip (withDynFlags env) (map buckLocation args.ghcOptions) \ dflags _ -> do
        void $ prepareMetadataSession env dflags
        modifyActiveUnitFlags \ d -> d {ghcMode = MkDepend}
        lowerT (prog env.log)
    where
      args = (emptyArgs []) {ghcOptions = ghcOptions unit2.id.raw [(unit1.id.raw, Nothing)]}
