module Test1 where

import Control.Concurrent (MVar)
import Data.Char (toUpper)
import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import Data.List.Extra (replace)
import GHC.Unit (stringToUnitId)
import Internal.Args (Args (..))
import Internal.Cache (Cache, CacheFeatures (..), emptyCacheWith)
import Internal.CompileHpt (compileHpt)
import Internal.CompileMake (step1, step2)
import Internal.Debug (showEnv)
import Internal.Log (dbg, dbgs, newLog)
import Internal.Session (Env (..), runSession, withGhc, withGhcInSession)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import GHC.Driver.Monad (modifySession)
import GHC (DynFlags (..), GhcMode (..))
import GHC.Driver.Env (hscUpdateFlags)
import Data.List.NonEmpty (NonEmpty)

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
one units Conf {..} UnitMod {unit, src, deps = _} = do
  dbg "---------------------------------"
  log <- newLog True
  let env = Env {log, cache, args}
  -- dbg (sanitize tmp fileOptions)
  result <- withGhc env \ target -> do
    result <- compileHpt tmp units target
    showEnv cache tmp
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
        "-o",
        tmp </> "out" </> (modName ++ ".dyn_o"),
        "-ohi",
        tmp </> "out" </> (modName ++ ".dyn_hi"),
        "-dynohi",
        tmp </> "out" </> (modName ++ ".dyn_hi"),
        "-this-unit-id",
        unit,
        src
      ]

    modName = dropExtension (takeFileName src)

unitMod :: Conf -> [String] -> String -> String -> String -> UnitMod
unitMod conf deps modName unit content =
  UnitMod {
    src = conf.tmp </> "src" </> unit </> modName ++ ".hs",
    unit,
    ..
  }

modType1 :: Conf -> [String] -> Char -> Int -> [String] -> UnitMod
modType1 conf pdeps unitTag n deps =
  unitMod conf pdeps modName unit content
  where
    unit = "unit-" ++ [unitTag]
    content =
      unlines $
        ("module " ++ modName ++ " where") :
        depImports ++
        [
          binding ++ " :: Int",
          binding ++ " = 5"
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
    ghcOptions = (artifactDir =<< ["o", "hie", "dump", "stub"]) ++ [
      -- "-i" ++ (tmp </> "out"),
      "-dynamic",
      "-shared",
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
    m1d ["unit-b"] 'a' 1 ["B1"],
    m1 'b' 3 [],
    m1d ["unit-b"] 'a' 2 ["A1", "B2", "B3"],
    unitMod conf ["unit-a", "unit-b"] "Main" "main" (mainContent [
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
    _ <- runSession env $ withGhcInSession env \ _ -> do
      modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
      step1 unitArgs
      showEnv cache tmp
      step2 [(src, stringToUnitId unit) | UnitMod {src, unit} <- targets]
      showEnv cache tmp
      pure (Just ())
    pure ()

test1 :: IO ()
test1 = testWorker
