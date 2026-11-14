{-# LANGUAGE QuasiQuotes #-}

-- | Extra libraries, the @-l@ options of a unit, in the worker's shared
-- session.
--
-- GHC loads the @-l@ libraries of every home unit of the session when its
-- loader starts.
-- In the worker, the session holds every unit the worker has seen, so a
-- compile must not load the libraries of a unit it does not depend on, but
-- must still load those of the units it does depend on.
-- See "Internal.State.ExtraLibs".
module ExtraLibsTest (test_extraLibs) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog (TestT, (===))
import System.Directory (createDirectoryIfMissing)
import System.OsPath.Extra (fromOsPath, osp, (</>))
import qualified GHC.Paths
import System.Process (callProcess, readProcess)
import Test.Build (initialCompileArgs, metadataArgs, runCompile, runMetadata)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), ModuleSource (..))
import Test.Data.Scheduler (RequestResult (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Path (moduleSourcePath, unitDir)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree, testGroup)
import Types.Args (Args (..))

buildModule :: Int -> Bool -> [ModuleKey] -> BuildModule
buildModule unit th deps =
  BuildModule {
    key = moduleKey unit,
    deps = Set.fromList deps,
    th,
    bindings = 1,
    extDeps = mempty
  }

moduleKey :: Int -> ModuleKey
moduleKey unit = ModuleKey {unit = fromIntegral unit, number = 1, errorVariant = Nothing}

source :: BuildModule -> ModuleSource
source BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

-- | Add library options to a unit's metadata arguments, as Buck adds those
-- of @extra_libraries@.
withLibrary :: FilePath -> String -> Args -> Args
withLibrary dir lib args =
  args {ghcOptions = args.ghcOptions ++ ["-L" ++ dir, "-l" ++ lib]}

-- | Unit 1 names a library that cannot be loaded, and unit 2, which does not
-- depend on unit 1, runs a splice.
--
-- Compilation of unit 2 should succeed.
testUnrelatedLibrary :: TestEnv -> TestT IO ()
testUnrelatedLibrary testEnv = do
  env <- liftIO (newSessionEnv testEnv)
  let module1 = buildModule 1 False []
      module2 = buildModule 2 True []
      unit1 = GenUnit {key = 1, depUnits = mempty, modules = [module1]}
      unit2 = GenUnit {key = 2, depUnits = mempty, modules = [module2]}
      missingDir = fromOsPath (env.tempDir </> [osp|no-libraries|])
  results <- liftIO do
    writeProjectSources env.sourceDir (Map.fromList [(m.key, source m) | m <- [module1, module2]])
    sequence [
      runMetadata env (withLibrary missingDir "ghc_worker_test_missing" . metadataArgs env) unit1,
      runMetadata env (metadataArgs env) unit2,
      runCompile env (initialCompileArgs env) module1.key,
      runCompile env (initialCompileArgs env) module2.key
      ]
  results === replicate 4 RequestSuccess

-- | The C compiler GHC is configured with, which exists wherever GHC compiles.
cCompiler :: IO FilePath
cCompiler = do
  info <- read <$> readProcess GHC.Paths.ghc ["--info"] ""
  maybe (fail "ghc --info has no C compiler command") pure $
    lookup "C compiler command" (info :: [(String, String)])

-- | A C library with one function.
cLibrarySource :: String
cLibrarySource =
  unlines [
    "int ghc_worker_test_answer(void) { return 42; }"
  ]

-- | Unit 1 imports a function from its extra library.
unit1Source :: String
unit1Source =
  unlines [
    "module Unit1Module1 where",
    "",
    "foreign import ccall unsafe \"ghc_worker_test_answer\" answer :: IO Int",
    "",
    "value_1_1 :: Int",
    "value_1_1 = 1"
  ]

-- | Unit 2 calls that function in a splice, so the library must be loaded when the splice runs.
unit2Source :: String
unit2Source =
  unlines [
    "{-# LANGUAGE TemplateHaskell #-}",
    "module Unit2Module1 where",
    "",
    "import Language.Haskell.TH.Syntax (lift, runIO)",
    "import Unit1Module1 (answer)",
    "",
    "value_2_1 :: Int",
    "value_2_1 = $(runIO answer >>= lift)"
  ]

-- | Unit 1 has an extra library with a foreign import, and unit 2, which
-- depends on unit 1, calls it in a splice.
--
-- The worker must load unit 1's library before the splice runs.
testDependencyLibrary :: TestEnv -> TestT IO ()
testDependencyLibrary testEnv = do
  env <- liftIO (newSessionEnv testEnv)
  let module1 = buildModule 1 False []
      module2 = buildModule 2 True [module1.key]
      unit1 = GenUnit {key = 1, depUnits = mempty, modules = [module1]}
      unit2 = GenUnit {key = 2, depUnits = Set.singleton 1, modules = [module2]}
      libDir = fromOsPath (env.tempDir </> [osp|libraries|])
      srcDir = fromOsPath env.sourceDir
  results <- liftIO do
    createDirectoryIfMissing True libDir
    writeFile (libDir ++ "/answer.c") cLibrarySource
    cc <- cCompiler
    callProcess cc
      ["-shared", "-fPIC", "-o", libDir ++ "/libghc_worker_test_answer.so", libDir ++ "/answer.c"]
    createDirectoryIfMissing True (srcDir ++ "/" ++ fromOsPath (unitDir 1))
    createDirectoryIfMissing True (srcDir ++ "/" ++ fromOsPath (unitDir 2))
    writeFile (srcDir ++ "/" ++ fromOsPath (moduleSourcePath module1.key)) unit1Source
    writeFile (srcDir ++ "/" ++ fromOsPath (moduleSourcePath module2.key)) unit2Source
    sequence [
      runMetadata env (withLibrary libDir "ghc_worker_test_answer" . metadataArgs env) unit1,
      runMetadata env (metadataArgs env) unit2,
      runCompile env (initialCompileArgs env) module1.key,
      runCompile env (initialCompileArgs env) module2.key
      ]
  results === replicate 4 RequestSuccess

-- | A C library with one function, for 'testAnnotationLibrary'.
annotationLibrarySource :: String
annotationLibrarySource =
  unlines [
    "int ghc_worker_test_annotation(void) { return 42; }"
  ]

-- | Unit 1 offers the answer of its own library as a pure value, for an
-- annotation. The library and its function have names of their own, so that
-- no other test of this process has loaded them before.
unit1AnnSource :: String
unit1AnnSource =
  unlines [
    "module Unit1Module1 where",
    "",
    "import System.IO.Unsafe (unsafePerformIO)",
    "",
    "foreign import ccall unsafe \"ghc_worker_test_annotation\" answer :: IO Int",
    "",
    "answerPure :: Int",
    "answerPure = unsafePerformIO answer",
    "",
    "value_1_1 :: Int",
    "value_1_1 = 1"
  ]

-- | Unit 2 has neither Template Haskell nor a plugin, but an annotation that
-- calls the function, which GHC evaluates in the interpreter.
unit2AnnSource :: String
unit2AnnSource =
  unlines [
    "module Unit2Module1 where",
    "",
    "import Unit1Module1 (answerPure)",
    "",
    "{-# ANN module (answerPure :: Int) #-}",
    "",
    "value_2_1 :: Int",
    "value_2_1 = 2"
  ]

-- | As 'testDependencyLibrary', but unit 2 depends on unit 1's library through
-- an annotation.
testAnnotationLibrary :: TestEnv -> TestT IO ()
testAnnotationLibrary testEnv = do
  env <- liftIO (newSessionEnv testEnv)
  let module1 = buildModule 1 False []
      module2 = buildModule 2 False [module1.key]
      unit1 = GenUnit {key = 1, depUnits = mempty, modules = [module1]}
      unit2 = GenUnit {key = 2, depUnits = Set.singleton 1, modules = [module2]}
      libDir = fromOsPath (env.tempDir </> [osp|annotation-libraries|])
      srcDir = fromOsPath env.sourceDir
  results <- liftIO do
    createDirectoryIfMissing True libDir
    writeFile (libDir ++ "/annotation.c") annotationLibrarySource
    cc <- cCompiler
    callProcess cc
      [ "-shared", "-fPIC", "-o", libDir ++ "/libghc_worker_test_annotation.so"
      , libDir ++ "/annotation.c"
      ]
    createDirectoryIfMissing True (srcDir ++ "/" ++ fromOsPath (unitDir 1))
    createDirectoryIfMissing True (srcDir ++ "/" ++ fromOsPath (unitDir 2))
    writeFile (srcDir ++ "/" ++ fromOsPath (moduleSourcePath module1.key)) unit1AnnSource
    writeFile (srcDir ++ "/" ++ fromOsPath (moduleSourcePath module2.key)) unit2AnnSource
    sequence [
      runMetadata env (withLibrary libDir "ghc_worker_test_annotation" . metadataArgs env) unit1,
      runMetadata env (metadataArgs env) unit2,
      runCompile env (initialCompileArgs env) module1.key,
      runCompile env (initialCompileArgs env) module2.key
      ]
  results === replicate 4 RequestSuccess

test_extraLibs :: TestTree
test_extraLibs =
  withTestEnv \ getTestEnv ->
    testGroup "extra libraries" [
      unitTest "a splice does not load an unrelated unit's library" do
        env <- liftIO getTestEnv
        testUnrelatedLibrary env,
      unitTest "a splice loads a dependency's library" do
        env <- liftIO getTestEnv
        testDependencyLibrary env,
      unitTest "an annotation loads a dependency's library" do
        env <- liftIO getTestEnv
        testAnnotationLibrary env
    ]
