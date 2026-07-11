module ByteCodeCacheTest where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable (showPprUnsafe)
import Hedgehog (TestT, annotate, assert, failure, (===))
import Test.Build (compileTarget, metadataArgs, runCompile, runMetadata)
import Test.Bytecode (enableByteCodeCacheLimit, enableLazyByteCode, loadedBcos)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), ModuleSource (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (BcoCacheEntry (..), MakeState (..))
import Types.Target (ModuleTarget (..))

keyA, keyB, keyC, keyD :: ModuleKey
keyA = ModuleKey {unit = 0, number = 0, errorVariant = Nothing}
keyB = ModuleKey {unit = 1, number = 0, errorVariant = Nothing}
keyC = ModuleKey {unit = 1, number = 1, errorVariant = Nothing}
keyD = ModuleKey {unit = 1, number = 2, errorVariant = Nothing}

modA, modB, modC, modD :: BuildModule
modA = BuildModule {key = keyA, deps = mempty, th = False, bindings = 3, extDeps = mempty}
modB = BuildModule {key = keyB, deps = mempty, th = False, bindings = 3, extDeps = mempty}
modC = BuildModule {key = keyC, deps = Set.singleton keyA, th = True, bindings = 1, extDeps = mempty}
modD = BuildModule {key = keyD, deps = Set.singleton keyB, th = True, bindings = 1, extDeps = mempty}

unit0, unit1 :: GenUnit BuildModule
unit0 = GenUnit {key = 0, depUnits = mempty, modules = [modA]}
unit1 = GenUnit {key = 1, depUnits = Set.singleton 0, modules = [modB, modC, modD]}

moduleSource :: BuildModule -> ModuleSource
moduleSource BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

allModules :: Map.Map ModuleKey ModuleSource
allModules =
  Map.fromList [(m.key, moduleSource m) | m <- [modA, modB, modC, modD]]

moduleFor :: ModuleKey -> Module
moduleFor key = (compileTarget key).mod

buildUpToB :: SessionEnv -> IO ()
buildUpToB sessionEnv = do
  writeProjectSources sessionEnv.sourceDir allModules
  _ <- runMetadata sessionEnv (metadataArgs sessionEnv) unit0
  _ <- runCompile sessionEnv plainArgs keyA
  _ <- runMetadata sessionEnv (metadataArgs sessionEnv) unit1
  _ <- runCompile sessionEnv plainArgs keyB
  pure ()
  where
    plainArgs _ = (sessionEnv.shared.baseArgs, mempty)

buildUpToC :: SessionEnv -> IO ()
buildUpToC sessionEnv = do
  buildUpToB sessionEnv
  _ <- runCompile sessionEnv plainArgs keyC
  pure ()
  where
    plainArgs _ = (sessionEnv.shared.baseArgs, mempty)

sizeOfA :: Env -> TestT IO Int
sizeOfA env = do
  WorkerState {make} <- liftIO $ readMVar env.state
  case Map.lookup (moduleFor keyA) make.bcoCache of
    Nothing -> do
      annotate "module A was not tracked in the bytecode cache after compiling its dependent splice"
      failure
    Just entry -> pure entry.size

test_evictBySize :: TestTree
test_evictBySize =
  withTestEnv \ getTestEnv ->
    unitTest "bytecode cache: LRU eviction by size" do
      testEnv <- liftIO getTestEnv
      sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
      liftIO (buildUpToB sessionEnv)
      _ <- liftIO (runCompile sessionEnv (const (sessionEnv.shared.baseArgs, mempty)) keyC)

      limit <- sizeOfA sessionEnv.env
      let args = (enableByteCodeCacheLimit limit sessionEnv.shared).baseArgs
      _ <- liftIO $ runCompile sessionEnv (\ _ -> (args, mempty)) keyD

      WorkerState {make} <- liftIO $ readMVar sessionEnv.env.state
      [showPprUnsafe (moduleFor keyB)] === (showPprUnsafe <$> Map.keys make.bcoCache)

      -- Only modules that are actually referenced by a TH splice (via 'Language.Haskell.TH.Syntax.lift') are ever
      -- linked into the interpreter's loader, tracked by 'Internal.State.Linkables.linkablesSelect' via
      -- 'ldAllLinkables' and mirrored 1:1 into 'MakeState.bcoCache'. Module A is spliced by C, module B is spliced
      -- by D; C and D themselves are never splice targets of anything (D depends only on B, not on C), so they are
      -- never loaded into the interpreter at all -- independently of the LRU eviction exercised above, which only
      -- ever unloads modules that were tracked in 'bcoCache' in the first place.
      bcos <- loadedBcos sessionEnv.env
      let modules = [modFs | (_, modFs, _) <- bcos]
      assert (not (any (== "Unit0Module0") modules))
      assert (any (== "Unit1Module0") modules)
      assert (not (any (== "Unit1Module1") modules))
      assert (not (any (== "Unit1Module2") modules))

test_touchNoEviction :: TestTree
test_touchNoEviction =
  withTestEnv \ getTestEnv ->
    unitTest "bytecode cache: touch without eviction" do
      testEnv <- liftIO getTestEnv
      sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))

      liftIO $ buildUpToB sessionEnv
      let args = (enableByteCodeCacheLimit 1000000 sessionEnv.shared).baseArgs
      _ <- liftIO $ runCompile sessionEnv (\ _ -> (args, mempty)) keyC

      WorkerState {make} <- liftIO $ readMVar sessionEnv.env.state

      (showPprUnsafe <$> Map.keys make.bcoCache) === [showPprUnsafe (moduleFor keyA)]
