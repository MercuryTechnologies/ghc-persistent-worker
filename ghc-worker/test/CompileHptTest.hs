module CompileHptTest where

import Control.Monad (unless, when)
import Data.Char (toUpper)
import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Data.Maybe (catMaybes, isNothing)
import GHC (DynFlags (..), Ghc, GhcException (..), GhcMode (..), mkGeneralLocated)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Utils.Monad (MonadIO (..))
import GHC.Utils.Outputable (ppr, showPprUnsafe, text, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (dbg, dbgp, dbgs, newLog)
import Internal.Metadata (computeMetadata)
import Internal.Session (Env (..), withGhcMhu)
import Internal.State.Stats (logMemStats)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import System.FilePath (dropExtension, takeBaseName, takeExtension, takeFileName, (</>))
import TestSetup (Conf (..), Module (..), ModuleSpec (..), Unit (..), UnitSpec (..), withProject)
import Types.Args (Args (..))
import Types.State (Target (..), TargetSpec (..))

-- | Parse command line flags, used to create unit-specific @DynFlags@.
unitFlags :: [String] -> HscEnv -> Ghc DynFlags
unitFlags args HscEnv {hsc_logger, hsc_dflags = dflags0} = do
  (dflags, _, warns) <- parseDynamicFlagsCmdLine dflags0 (map (mkGeneralLocated "no loc") args)
  liftIO $ printOrThrowDiagnostics hsc_logger (initPrintConfig dflags) (initDiagOpts dflags) (GhcDriverMessage <$> warns)
  pure dflags

stepMetadata :: Conf -> Unit -> [Unit] -> IO ()
stepMetadata Conf {state, tmp, args0} unit deps = do
  log <- newLog Nothing
  createDirectoryIfMissing False sessionTmpDir
  names <- listDirectory unit.dir
  let srcs = [unit.dir </> name | name <- names, takeExtension name == ".hs"]
      env = Env {log, state, args = args srcs}
  dbgp (text ">>> metadata for" <+> ppr unit.uid)
  (success, _) <- computeMetadata env
  unless success do
    liftIO $ throwGhcExceptionIO (ProgramError "Metadata failed")
  where
    args srcs = args0 {
      ghcOptions = mkDependArgs ++ unitDepArgs ++ args0.ghcOptions ++ srcs,
      tempDir = Just sessionTmpDir
    }

    unitDepArgs = concat [["-package-id", name]  | Unit {name} <- deps]

    mkDependArgs = [
      "-i",
      "-hide-all-packages",
      "-this-unit-id",
      showPprUnsafe unit.uid,
      "-dep-json=" ++ (sessionTmpDir </> "dep.json"),
      "-dep-makefile=" ++ (sessionTmpDir </> "dep.make"),
      "-include-pkg-deps",
      "-package", "template-haskell"
      ]

    sessionTmpDir = tmp </> "tmp" </> unit.name

stepCompile :: Conf -> Module -> IO ()
stepCompile Conf {state, tmp, args0} Module {unit, src} = do
  log <- newLog Nothing
  let env = Env {log, state, args}
  liftIO $ createDirectoryIfMissing False sessionTmpDir
  result <- liftIO $ withGhcMhu env \ _ target -> do
    dbg ""
    dbg (">>> compiling " ++ takeFileName target.path)
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    compileModuleWithDepsInHpt log (TargetSource target)
  when (isNothing result) do
      liftIO $ throwGhcExceptionIO (ProgramError "Compile failed")
  dbgs result
  where
    args =
      args0 {
        ghcOptions = args0.ghcOptions ++ fileOptions,
        -- Ensure that each module gets a separate temp directory, to resemble circumstances in a Buck build
        tempDir = Just sessionTmpDir
      }

    sessionTmpDir = tmp </> "tmp" </> takeBaseName src

    fileOptions =
      [
        "-i",
        "-this-unit-id",
        unit,
        "-o",
        tmp </> "out" </> (modName ++ ".dyn_o"),
        "-ohi",
        tmp </> "out" </> (modName ++ ".dyn_hi"),
        src
      ]

    modName = dropExtension (takeFileName src)

errContent :: String
errContent =
  unlines [
    "{-# language DerivingStrategies #-}",
    "module Err where",
    "num :: Int",
    "num = 5",
    "newtype N = N Int deriving newtype Num"
  ]

bugContent :: String
bugContent =
  unlines [
    -- "{-# options -ddump-tc -ddump-tc-trace #-}",
    "module Bug where",
    "import Language.Haskell.TH",
    "import Language.Haskell.TH.Syntax",
    "import Err",
    "bug :: ExpQ",
    "bug = lift @_ @Int num",
    "n :: N",
    "n = 1"
  ]

main1 :: String
main1 =
  unlines
    [
      "{-# language TemplateHaskell #-}",
      "module Main where",
      "import Bug",
      "main :: IO ()",
      "main = print $(bug)"
    ]

{-

-- unit-a:

module Err where
num :: Int
num = 5


-- unit-main:

module Bug where
import Err
bug :: ExpQ
bug = lift @_ @Int num

module Main where
import Bug
main :: IO ()
main = print $(bug)

-}

targets1 :: NonEmpty UnitSpec
targets1 =
  [
    UnitSpec {
      name = "unit-a",
      deps = [],
      modules = [
        ModuleSpec "Err" errContent
      ]
    },
    UnitSpec {
      name = "unit-main",
      deps = ["unit-a"],
      modules = [
        ModuleSpec "Bug" bugContent,
        ModuleSpec "Main" main1
      ]
    }
  ]

useTh :: Bool
useTh = True

modType1 :: Char -> Int -> [String] -> ModuleSpec
modType1 unitTag n deps =
  ModuleSpec modName content
  where
    content =
      unlines $
        ("module " ++ modName ++ " where") :
        "import Language.Haskell.TH (ExpQ)" :
        "import Language.Haskell.TH.Syntax (lift)" :
        depImports ++
        if useTh
        then [
          binding ++ " :: ExpQ",
          binding ++ " = lift @_ @Int 5"
        ]
        else [
          binding ++ " :: Int",
          binding ++ " = 5"
        ]
    depImports = ["import " ++ d | d <- deps]
    modName = toUpper unitTag : show n
    binding = unitTag : show n

sumTh :: [String] -> String
sumTh =
  foldl' (\ z a -> z ++ " + $(" ++ a ++ ")") "0"

modType2 :: Char -> Int -> [String] -> [String] -> ModuleSpec
modType2 unitTag n deps thDeps =
  ModuleSpec modName content
  where
    content =
      unlines $
        "{-# language TemplateHaskell #-}" :
        ("module " ++ modName ++ " where") :
        depImports ++
        [
          binding ++ " :: Int",
          binding ++ " = " ++ if useTh
          then sumTh thDeps
          else "5"
        ]
    depImports = ["import " ++ d | d <- deps]
    modName = toUpper unitTag : show n
    binding = unitTag : show n

mainContent :: [(Char, Int)] -> String
mainContent deps =
  unlines $
    "{-# language TemplateHaskell #-}" :
    "module Main where" :
    ["import " ++ toUpper c : show i | (c, i) <- deps] ++
    [
      "import Bug",
      "main :: IO ()",
      "main = do",
      "  if False then print $(bug) else pure ()",
      "  print (" ++ (if useTh then sumTh names else intercalate " + " ("0" : names)) ++ ")"
    ]
  where
    names = [c : show i | (c, i) <- deps]

targets2 :: NonEmpty UnitSpec
targets2 =
  [
    UnitSpec {
      name = "unit-b",
      deps = [],
      modules = [
        ModuleSpec "Err" errContent,
        modType1 'b' 1 [],
        modType1 'b' 2 [],
        modType1 'b' 3 []
      ]
    },
    UnitSpec {
      name = "unit-a",
      deps = ["unit-b"],
      modules = [
        modType1 'a' 0 ["B2"],
        modType2 'a' 1 ["B1"] ["b1"],
        modType1 'a' 2 ["A0", "A1", "B2", "B3"]
      ]
    },
    UnitSpec {
      name = "unit-main",
      deps = ["unit-a", "unit-b"],
      modules = [
        ModuleSpec "Bug" bugContent,
        ModuleSpec "Main" (mainContent [
          ('a', 0),
          ('a', 2),
          ('b', 1)
        ])
      ]
    }
  ]

removeGhcTmpDir :: Conf -> IO ()
removeGhcTmpDir conf = do
  dirs <- liftIO (listDirectory (conf.tmp </> "tmp"))
  for_ dirs \ ghcTmpRel -> do
    let ghcTmp = conf.tmp </> "tmp" </> ghcTmpRel
    liftIO $ removeDirectoryRecursive ghcTmp

data TestStep =
  StepMetadata Unit [Unit]
  |
  StepCompile Module

testSteps :: NonEmpty Unit -> NonEmpty TestStep
testSteps units = do
  unit@Unit {..} <- units
  StepMetadata unit (catMaybes [byName !? dep | dep <- deps]) :| (StepCompile <$> NonEmpty.toList modules)
  where
    byName = Map.fromList [(unit.name, unit) | unit <- NonEmpty.toList units]

runStep :: Conf -> TestStep -> IO ()
runStep conf = \case
  StepMetadata unit deps -> stepMetadata conf unit deps
  StepCompile module_ -> stepCompile conf module_

testWorker :: (Conf -> NonEmpty UnitSpec) -> IO ()
testWorker mkSpecs = do
  log <- newLog Nothing
  logMemStats "initial" log
  withProject (pure . mkSpecs) \ conf units -> do
    let steps = testSteps units
    traverse_ (runStep conf) steps
    -- Simulate the case of Buck deleting the temp dir and recompiling a module, which goes unnoticed by GHC because
    -- it tracks all created temp dirs in an @IORef@ that the worker shares across sessions.
    -- When initializing a new module session, all dirs already present in the state are assumed to exist on disk and
    -- not recreated.
    -- As a consequence, it will try to write the assembly files @ghc_1.s@ to a nonexistent directory, so we have to
    -- ensure that each session gets its own @TmpFs@.
    liftIO (removeGhcTmpDir conf)
    runStep conf (NonEmpty.last steps)
    dbgs =<< listDirectory (conf.tmp </> "out")

-- | A very simple test consisting of two home units, using a transitive TH dependency across unit boundaries.
test_compileHpt :: IO ()
test_compileHpt =
  testWorker (const targets1)
