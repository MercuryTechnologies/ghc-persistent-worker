-- | Description: A worker that is not restarted between builds must not serve the first build's unit state to the
-- second. Each sequence feeds several builds to one long-lived worker and the last build alone to a fresh worker; the
-- last module must come out of both identical, which it does only when the worker revalidates the state it kept.
module StaleUnitTest where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (for_, toList)
import Data.IORef (readIORef)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Traversable (for)
import GHC (getSession, getSessionDynFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (DynFlags (..), GhcMode (..), targetProfile)
import GHC.Fingerprint (Fingerprint)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Types.Avail (availNames)
import GHC.Types.Name (getOccString)
import GHC.Unit (stringToUnitId)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Module.ModDetails (emptyModDetails)
import GHC.Unit.Module.ModIface (mi_exports, mi_src_hash)
import Hedgehog (TestT, assert, footnote, (===))
import Hedgehog.Internal.Property (failWith)
import Internal.AbiHash (showAbiHash)
import Internal.Cache.Hpt (interfaceStale, readIfaceHeader)
import Internal.Cache.Metadata (addHomeUnitTo, flagsFingerprint)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Metadata (computeMetadata)
import Internal.Session (simpleSessionWithDebugLog, withGhcMakeModule)
import Internal.State (newState)
import Prelude hiding (log)
import System.Directory.Extra (createDirectoryIfMissing, removeFile)
import System.OsPath.Extra (OsPath, fromOsPath, osp, (<.>), (</>))
import Test.Build (compileTarget, metadataArgs)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), UnitKey (..))
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Log (withTestLog)
import Test.PackageDb (ModuleSpec (..))
import Test.Path (compileTmpDir, moduleName, moduleOutputBase, unitName, unitTmpDir)
import Test.Run (transientSession, unitTest)
import Test.Target (fileTarget)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..), emptyArgs)
import Types.BuckArgs (IsInterpreted (Compiled))
import Types.Env (Env (..))
import Types.Target (TargetSpec (..))

-- | One build as Buck sends it to a worker it did not restart: the unit's extra GHC args, every module's source, then
-- the compiles in order.
data Build =
  Build {
    extraArgs :: [String],
    sources :: [(BuildModule, ByteString)],
    compiles :: [ModuleKey]
  }

-- | What one build step produced. Failures carry GHC's own text so the test output quotes the panic.
data Step =
  Step {
    label :: String,
    ok :: Bool,
    output :: [String]
  }

-- | The produced interface of one module.
data Iface =
  Iface {
    exports :: [String],
    abi :: String
  }
  deriving stock (Eq, Show)

-- | Run one worker task with its own log, keeping the diagnostics and fatal errors so a failure can quote them.
-- The task's args replace the env's, as the server does, so only the task directory is prepared here.
runStep :: SessionEnv -> String -> OsPath -> (Env -> IO Bool) -> IO Step
runStep env label taskDir action =
  withTestLog False label \ (log, logVar) -> do
    createDirectoryIfMissing True (fromOsPath (env.tempDir </> taskDir))
    result <- try (action env.env {log})
    TestLog {diagnostics, fatal} <- readIORef logVar
    let logged = [d.rendered | d <- diagnostics] ++ fatal
    pure case result of
      Right ok -> Step {label, ok, output = logged}
      Left (e :: SomeException) -> Step {label, ok = False, output = logged ++ [displayException e]}

runBuild :: SessionEnv -> UnitKey -> Build -> IO [Step]
runBuild env unit Build {extraArgs, sources, compiles} = do
  for_ sources \ (BuildModule {key}, content) ->
    fileTarget (fromOsPath env.sourceDir) (stringToUnitId (unitName unit)) ModuleSpec {name = moduleName key, content, boot = False}
  metadata <- runStep env "metadata" (unitTmpDir unit) \ taskEnv ->
    fst <$> computeMetadata taskEnv {args = unitArgs {ghcOptions = unitArgs.ghcOptions ++ extraArgs}}
  compiled <- for compiles \ key ->
    runStep env ("compile " ++ moduleName key) (compileTmpDir key) \ taskEnv -> do
      let compileEnv = taskEnv {args = env.shared.baseArgs}
          target = compileTarget key
      result <- withGhcMakeModule Compiled target compileEnv \ _targetSpec -> do
        modifyGlobalFlags \ d -> d {ghcMode = CompManager}
        compileModuleWithDepsInHpt compileEnv.log (TargetModule target)
      pure (isJust result)
  pure (metadata : compiled)
  where
    unitArgs = metadataArgs env GenUnit {key = unit, depUnits = [], modules = map fst sources}

readIface :: SessionEnv -> ModuleKey -> TestT IO Iface
readIface env key =
  transientSession [] do
    hsc_env@HscEnv {hsc_dflags, hsc_NC} <- getSession
    iface <- liftIO (readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace path)
    pure Iface {
      exports = sort (concatMap (map getOccString . availNames) (mi_exports iface)),
      abi = showAbiHash hsc_env iface
    }
  where
    path = fromOsPath (env.tempDir </> moduleOutputBase key <.> [osp|dyn_hi|])

-- | One worker state gets every build in order; a fresh one gets only the last. The last build's module must come out
-- of both the same, and the fresh one is checked against the expected export list so the reference itself is sound.
staleSequence :: IO TestEnv -> String -> ModuleKey -> [String] -> NonEmpty Build -> TestT IO ()
staleSequence testEnv name key expectedExports builds = do
  shared <- liftIO testEnv
  long <- liftIO (newSessionEnv shared)
  fresh <- liftIO (newSessionEnv shared)
  longSteps <- liftIO (concat <$> traverse (runBuild long unit1) (toList builds))
  freshSteps <- liftIO (runBuild fresh unit1 (NonEmpty.last builds))
  checkSteps ("long-lived worker (" ++ name ++ ")") longSteps
  checkSteps ("fresh worker (" ++ name ++ ")") freshSteps
  longIface <- readIface long key
  freshIface <- readIface fresh key
  footnote ("long-lived worker: " ++ show longIface)
  footnote ("fresh worker: " ++ show freshIface)
  freshIface.exports === expectedExports
  longIface.exports === freshIface.exports
  longIface.abi === freshIface.abi
  where
    checkSteps worker steps =
      for_ (nonEmpty [s | s <- steps, not s.ok]) \ failed ->
        failWith Nothing $ intercalate "\n" $ concat [(worker ++ ": " ++ s.label ++ " failed") : s.output | s <- toList failed]

unit1 :: UnitKey
unit1 = UnitKey 1

k, m, k2 :: ModuleKey
k = ModuleKey {unit = unit1, number = 1, errorVariant = Nothing}
m = ModuleKey {unit = unit1, number = 2, errorVariant = Nothing}
k2 = ModuleKey {unit = unit1, number = 3, errorVariant = Nothing}

plain :: ModuleKey -> BuildModule
plain key = BuildModule {key, deps = [], th = False, bindings = 1, extDeps = []}

source :: [ByteString] -> ByteString
source = ByteString.unlines

kValue :: Int -> ByteString
kValue n = source ["module Unit1Module1 where", "value_1_1 :: Int", "value_1_1 = " <> ByteString.pack (show n)]

kValueAndExtra :: ByteString
kValueAndExtra = kValue 1 <> source ["value_1_1_1 :: Int", "value_1_1_1 = 100"]

kCpp :: ByteString
kCpp = source [
  "{-# LANGUAGE CPP #-}",
  "module Unit1Module1 where",
  "value_1_1 :: Int",
  "value_1_1 = 1",
  "#ifdef FOO",
  "value_1_1_foo :: Int",
  "value_1_1_foo = 42",
  "#endif"
  ]

-- | The splice puts K's value into a declaration name, because a value-only change does not move the ABI hash.
mSplice :: ByteString
mSplice = source [
  "{-# LANGUAGE TemplateHaskell #-}",
  "module Unit1Module2 where",
  "import Language.Haskell.TH (mkName, sigD, valD, varP, normalB, conT)",
  "import Unit1Module1 (value_1_1)",
  "$(let n = mkName (\"spliced_\" ++ show value_1_1) in sequence [sigD n (conT ''Int), valD (varP n) (normalB [| value_1_1 |]) []])"
  ]

mImportsK :: ByteString
mImportsK = source ["module Unit1Module2 where", "import Unit1Module1", "value_1_2 :: Int", "value_1_2 = value_1_1 + 1"]

k2Source :: ByteString
k2Source = source ["module Unit1Module3 where", "value_1_3 :: Int", "value_1_3 = 5"]

-- | The importer also gains an export that needs the new module, because a recompile from its old source text would
-- otherwise produce the same export list and, the change being value-only, the same ABI hash.
mImportsKAndK2 :: ByteString
mImportsKAndK2 = source [
  "module Unit1Module2 where",
  "import Unit1Module1",
  "import Unit1Module3",
  "value_1_2 :: Int",
  "value_1_2 = value_1_1 + value_1_3",
  "value_1_2_3 :: Int",
  "value_1_2_3 = value_1_3"
  ]

-- | GHC's own flag fingerprint of a unit whose only extra args are the given ones, obtained through the same
-- 'flagsFingerprint' the worker uses to decide eviction.
flagFingerprint :: [String] -> IO Fingerprint
flagFingerprint extra = do
  st <- newState
  result <- simpleSessionWithDebugLog st (emptyArgs []) {ghcOptions = ghcOptions'} do
    hsc0 <- getSession
    dflags <- getSessionDynFlags
    (hsc1, unit) <- liftIO (addHomeUnitTo hsc0 dflags)
    liftIO (flagsFingerprint hsc1 unit dflags)
  pure (fromMaybe (error "flagFingerprint: session failed") result)
  where
    ghcOptions' = ["-hide-all-packages", "-package", "base", "-this-unit-id", "ffp", "-dynamic", "-fPIC"] ++ extra

-- | Compile K once through a worker and hand its produced interface, wrapped in a 'HomeModInfo', to a test of the
-- interface staleness check. The details and linkable are unused by 'interfaceStale', so they are left empty.
withCompiledK :: IO TestEnv -> (HscEnv -> HomeModInfo -> OsPath -> IO ()) -> TestT IO ()
withCompiledK testEnv use = do
  shared <- liftIO testEnv
  env <- liftIO (newSessionEnv shared)
  steps <- liftIO (runBuild env unit1 Build {extraArgs = [], sources = [(plain k, kValue 1)], compiles = [k]})
  for_ (nonEmpty [s | s <- steps, not s.ok]) \ failed ->
    failWith Nothing (intercalate "\n" [s.label ++ " failed\n" ++ intercalate "\n" s.output | s <- toList failed])
  let path = env.tempDir </> moduleOutputBase k <.> [osp|dyn_hi|]
  transientSession [] do
    hsc_env <- getSession
    liftIO do
      iface <- readBinIface (targetProfile hsc_env.hsc_dflags) hsc_env.hsc_NC IgnoreHiWay QuietBinIFace (fromOsPath path)
      let hmi = HomeModInfo {hm_iface = iface, hm_details = emptyModDetails, hm_linkable = HomeModLinkable Nothing Nothing}
      use hsc_env hmi path

test_staleUnit :: TestTree
test_staleUnit =
  withTestEnv \ testEnv ->
    testGroup "stale unit state across builds" [
      unitTest "source changes, same args: the second build exports the new binding" $
        staleSequence testEnv "source change" k ["value_1_1", "value_1_1_1"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1)], compiles = [k]},
          Build {extraArgs = [], sources = [(plain k, kValueAndExtra)], compiles = [k]}
        ],
      unitTest "unit args change: -DFOO added, the second build exports the CPP-gated binding" $
        staleSequence testEnv "unit args change" k ["value_1_1", "value_1_1_foo"] [
          Build {extraArgs = [], sources = [(plain k, kCpp)], compiles = [k]},
          Build {extraArgs = ["-DFOO"], sources = [(plain k, kCpp)], compiles = [k]}
        ],
      unitTest "TH splice reads a changed module: the second build's splice sees the new value" $
        staleSequence testEnv "TH splice" m ["spliced_100"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1), ((plain m) {th = True}, mSplice)], compiles = [k, m]},
          Build {extraArgs = [], sources = [(plain k, kValue 100), ((plain m) {th = True}, mSplice)], compiles = [k, m]}
        ],
      unitTest "module added to a known unit: the importer's second build sees the new module's binding" $
        staleSequence testEnv "module added" m ["value_1_2", "value_1_2_3"] [
          Build {extraArgs = [], sources = [(plain k, kValue 1), (plain m, mImportsK)], compiles = [k, m]},
          Build {
            extraArgs = [],
            sources = [(plain k, kValue 1), (plain k2, k2Source), (plain m, mImportsKAndK2)],
            compiles = [k, k2, m]
          }
        ],
      unitTest "flag fingerprint ignores output dirs but tracks a preprocessor define" do
        rootA <- liftIO (flagFingerprint ["-odir", "/tmp/root-a", "-hidir", "/tmp/root-a"])
        rootB <- liftIO (flagFingerprint ["-odir", "/tmp/root-b", "-hidir", "/tmp/root-b"])
        withFoo <- liftIO (flagFingerprint ["-odir", "/tmp/root-a", "-hidir", "/tmp/root-a", "-DFOO"])
        footnote ("two roots: " ++ show rootA ++ " vs " ++ show rootB)
        rootA === rootB
        assert (rootA /= withFoo),
      unitTest "interface header source hash agrees with the full interface" $
        withCompiledK testEnv \ _ hmi path ->
          readIfaceHeader (fromOsPath path) >>= \case
            Left e -> ioError (userError e)
            Right srcHash
              | srcHash == mi_src_hash hmi.hm_iface -> pure ()
              | otherwise -> ioError (userError "header source hash disagrees with mi_src_hash"),
      unitTest "interface check fails closed on a missing sidecar and passes on a matching one" $
        withCompiledK testEnv \ hsc_env hmi path -> do
          let sidecar = fromOsPath path ++ ".hash"
          missing <- interfaceStale hsc_env hmi path
          writeFile sidecar (showAbiHash hsc_env hmi.hm_iface)
          matching <- interfaceStale hsc_env hmi path
          writeFile sidecar "deadbeefdeadbeefdeadbeefdeadbeef"
          wrong <- interfaceStale hsc_env hmi path
          removeFile sidecar
          checkEq "missing sidecar" missing (Just "no .hash beside the interface")
          checkEq "matching sidecar" matching Nothing
          checkEq "wrong sidecar" wrong (Just "ABI hash changed")
    ]
  where
    checkEq what actual expected
      | actual == expected = pure ()
      | otherwise = ioError (userError (what ++ ": got " ++ show actual ++ ", expected " ++ show expected))
