module Test1 where

import Control.Concurrent (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper)
import Data.Foldable (for_, traverse_)
import Data.List (intercalate, stripPrefix)
import Data.List.Extra (replace)
import Data.List.NonEmpty (NonEmpty)
import GHC (DynFlags (..), Ghc, GhcMode (..), ModLocation (..), getSession)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GHC.Unit (installedModuleEnvElts, stringToUnitId)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Utils.Outputable (SDoc, hcat, ppr, text, vcat, (<+>))
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCacheWith, finderEnv)
import Internal.CompileHpt (compileHpt)
import Internal.CompileMake (step1, step2)
import Internal.Debug (entries, showModGraph, showUnitEnv)
import Internal.Log (dbg, dbgp, dbgs, newLog)
import Internal.Session (Env (..), runSession, withGhcGeneral, withGhcInSession)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

showFinderCache :: MVar Cache -> FilePath -> IO SDoc
showFinderCache var tmp = do
  Cache {finder} <- readMVar var
  modules <- finderEnv finder
  pure $ vcat [hcat [ppr m, ":"] <+> showLoc r | (m, r) <- installedModuleEnvElts modules]
  where
    showLoc = \case
      InstalledFound ModLocation {ml_hs_file} _
        | Just path <- ml_hs_file
        , Just rel <- stripPrefix tmp path
        -> text (dropWhile ('/' ==) rel)
        | otherwise
        -> "external"
      _ -> "no loc"

showEnv :: MVar Cache -> FilePath -> Ghc ()
showEnv cache tmp = do
  HscEnv {..} <- getSession
  unit_env <- liftIO (showUnitEnv hsc_unit_env)
  finder <- liftIO (showFinderCache cache tmp)
  dbgp $ entries [
    ("targets", ppr hsc_targets),
    ("mod_graph", showModGraph hsc_mod_graph),
    ("finder", finder),
    ("unit_env", unit_env)
    ]

data Conf =
  Conf {
    tmp :: FilePath,
    cache :: MVar Cache,
    args0 :: Args
  }

data UnitMod =
  UnitMod {
    src :: FilePath,
    unit :: String,
    deps :: [String],
    content :: String
  }
  deriving stock (Eq, Show)

sanitize :: FilePath -> [String] -> String
sanitize tmp =
  replace tmp "$tmp"  . unwords

one :: NonEmpty (String, String, [String]) -> Conf -> UnitMod -> IO ()
one _ Conf {..} UnitMod {unit, src, deps} = do
  dbg "---------------------------------"
  log <- newLog True
  let env = Env {log, cache, args}
  -- dbg (sanitize tmp fileOptions)
  result <- withGhcGeneral env \ specific target -> do
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    -- showEnv cache tmp
    result <- compileHpt specific target
    -- showEnv cache tmp
    pure result
  dbgs result
  pure ()
  where
    args =
      args0 {
        ghcOptions = args0.ghcOptions ++ fileOptions
      }

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
      ] ++
      concat [["-package", dep] | dep <- deps]

    modName = dropExtension (takeFileName src)

unitMod :: Conf -> [String] -> String -> String -> String -> UnitMod
unitMod conf deps modName unit content =
  UnitMod {
    src = conf.tmp </> "src" </> unit </> modName ++ ".hs",
    unit,
    ..
  }

useTh :: Bool
useTh = True

modType1 :: Conf -> [String] -> Char -> Int -> [String] -> UnitMod
modType1 conf pdeps unitTag n deps =
  unitMod conf pdeps modName unit content
  where
    unit = "unit-" ++ [unitTag]
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

modType2 :: Conf -> [String] -> Char -> Int -> [String] -> [String] -> UnitMod
modType2 conf pdeps unitTag n deps thDeps =
  unitMod conf pdeps modName unit content
  where
    unit = "unit-" ++ [unitTag]
    content =
      unlines $
        "{-# language TemplateHaskell #-}" :
        ("module " ++ modName ++ " where") :
        depImports ++
        [
          binding ++ " :: Int",
          if useTh
          then binding ++ " = " ++ foldl' (\ z a -> z ++ " + $(" ++ a ++ ")") "0" thDeps
          else binding ++ " = 5"
        ]
    depImports = ["import " ++ d | d <- deps]
    modName = toUpper unitTag : show n
    binding = unitTag : show n

a1Content :: String
a1Content =
  "module Dep1 where\na1 :: Int\na1 = 5"

mainContent :: [(Char, Int)] -> String
mainContent deps =
  unlines $
    "module Main where" :
    ["import " ++ toUpper c : show i | (c, i) <- deps] ++
    [
      "main :: IO ()",
      "main = print (" ++ intercalate " + " ("0" : [c : show i | (c, i) <- deps]) ++ ")"
    ]

baseArgs :: FilePath -> FilePath -> Args
baseArgs ghc_dir tmp =
  Args {
    topdir = Just (ghc_dir </> "lib/ghc-9.10.1/lib"),
    workerTargetId = Just "test",
    env = mempty,
    binPath = [],
    tempDir = Just (tmp </> "tmp"),
    ghcPath = Nothing,
    ghcOptions = (artifactDir =<< ["o", "hie", "dump"]) ++ [
      "-fwrite-ide-info",
      "-no-link",
      "-dynamic",
      "-fwrite-if-simplified-core",
      "-fprefer-byte-code",
      -- "-fpackage-db-byte-code",
      -- "-shared",
      "-fPIC",
      "-osuf",
      "dyn_o",
      "-hisuf",
      "dyn_hi"
      -- , "-v"
      -- , "-ddump-if-trace"
    ]
  }
  where
    artifactDir a = ["-" ++ a ++ "dir", tmp </> "out"]

targets1 :: Conf -> [UnitMod]
targets1 conf =
  [
    m1 'b' 1 [],
    m1 'b' 2 [],
    m1d [] 'a' 0 [],
    modType2 conf ["unit-b"] 'a' 1 ["B1"] ["b1"],
    m1 'b' 3 [],
    m1d ["unit-b"] 'a' 2 ["A0", "A1", "B2", "B3"],
    unitMod conf ["unit-a", "unit-b"] "Main" "main" (mainContent [
      ('a', 0),
      ('a', 2),
      ('b', 1)
    ])
  ]
  where
    m1 = modType1 conf []

    m1d = modType1 conf

withProject ::
  (Conf -> [UnitMod] -> IO ()) ->
  IO ()
withProject use =
  withSystemTempDirectory "buck-worker-test" \ tmp -> do
    for_ @[] ["src", "tmp", "out"] \ dir ->
      createDirectoryIfMissing False (tmp </> dir)
    cache <- emptyCacheWith CacheFeatures {
      hpt = True,
      loader = False,
      enable = True,
      names = False,
      finder = True,
      eps = False
    }
    ghc_dir <- getEnv "ghc_dir"
    let conf = Conf {tmp, cache, args0 = baseArgs ghc_dir tmp}
    -- dbg (sanitize tmp conf.args0.ghcOptions)
    let targets = targets1 conf
    for_ targets \ UnitMod {src, content} -> do
      createDirectoryIfMissing False (takeDirectory src)
      writeFile src content
    use conf targets
    dbgs =<< listDirectory (tmp </> "out")

testWorker :: IO ()
testWorker =
  withProject \ conf mods -> do
    let
      units = [
        ("unit-b", conf.tmp </> "src/unit-b", []),
        ("unit-a", conf.tmp </> "src/unit-a", ["unit-b"]),
        ("main", conf.tmp </> "src/main", ["unit-b", "unit-a"])
        ]
    traverse_ (one units conf) mods

testMake :: IO ()
testMake =
  withProject \ Conf {..} targets -> do
    log <- newLog True
    let env = Env {log, cache, args = args0}
        unitArgs =
          [
            ["-i", "-i" ++ (tmp </> "src/unit-b"), "-this-unit-id", "unit-b"],
            ["-i", "-i" ++ (tmp </> "src/unit-a"), "-this-unit-id", "unit-a", "-package-id", "unit-b"],
            ["-i", "-i" ++ (tmp </> "src/main"), "-this-unit-id", "main", "-package-id", "unit-a", "-package-id", "unit-b"]
          ]
    _ <- runSession True env $ withGhcInSession env \ _ -> do
      modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
      step1 unitArgs
      showEnv cache tmp
      step2 [(src, stringToUnitId unit) | UnitMod {src, unit} <- targets]
      showEnv cache tmp
      pure (Just ())
    pure ()

test1 :: IO ()
test1 = testWorker
