module Test1 where

import Control.Concurrent (MVar)
import Data.Char (toUpper)
import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import Data.List.Extra (replace)
import Internal.Args (Args (..))
import Internal.Cache (Cache, CacheFeatures (..), emptyCacheWith)
import Internal.CompileHpt (compileHpt)
import Internal.CompileMake (step1)
import Internal.Debug (showEnv)
import Internal.Log (dbg, dbgs, newLog)
import Internal.Session (Env (Env, args, cache, log), runSession, withGhc, withGhcInSession)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath (dropExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

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
    content :: String
  }
  deriving stock (Eq, Show)

sanitize :: FilePath -> [String] -> String
sanitize tmp =
  replace tmp "$tmp"  . unwords

one :: Conf -> UnitMod -> IO ()
one Conf {..} UnitMod {unit, src} = do
  dbg "---------------------------------"
  log <- newLog True
  let env = Env {log, cache, args}
  -- dbg (sanitize tmp fileOptions)
  result <- withGhc env \ target -> do
    result <- compileHpt target
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

unitMod :: Conf -> String -> Char -> String -> UnitMod
unitMod conf modName unitTag content =
  UnitMod {
    src = conf.tmp </> "src" </> modName ++ ".hs",
    unit = "unit-" ++ [unitTag],
    ..
  }

modType1 :: Conf -> Char -> Int -> [String] -> UnitMod
modType1 conf unitTag n deps =
  unitMod conf modName unitTag content
  where
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
      "main = putStrLn (" ++ intercalate " + " [c : show i | (c, i) <- deps] ++ ")"
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
      "-i" ++ (tmp </> "out"),
      "-dynamic",
      "-fPIC",
      "-osuf",
      "dyn_o",
      "-hisuf",
      "dyn_hi"
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
    m1 'a' 1 [],
    m1 'b' 3 [],
    -- m1 'a' 2 ["A1", "B2", "B3"],
    unitMod conf "Main.hs" 'a' (mainContent [
      -- ('a', 2),
      ('b', 3)
    ])
  ]
  where
    m1 = modType1 conf

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
    for_ targets \ UnitMod {src, content} -> writeFile src content
    use conf targets

test1 :: IO ()
test1 =
  withProject (traverse_ . one)

test2 :: IO ()
test2 =
  withProject \ Conf {..} _ -> do
    log <- newLog True
    let env = Env {log, cache, args = args0}
        unitArgs =
          [
            ["-i" ++ (tmp </> "src"), "-this-unit-id", "a"],
            ["-i" ++ (tmp </> "src"), "-this-unit-id", "b"]
          ]
    _ <- runSession env $ withGhcInSession env \ _ -> do
      step1 unitArgs
      showEnv cache tmp
      pure (Just ())
    pure ()
