module Test1 where

import Control.Concurrent (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.List (intercalate, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import GHC (DynFlags (..), Ghc, GhcMode (..), ModLocation (..), getSession)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Monad (modifySession)
import GHC.Unit (UnitId, installedModuleEnvElts, stringToUnitId)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Utils.Outputable (SDoc, hcat, ppr, text, vcat, (<+>))
import Internal.Cache (Cache (..), finderEnv)
import Internal.CompileMake (step1, step2)
import Internal.Debug (entries, showModGraph, showUnitEnv)
import Internal.Log (dbgp, newLog)
import Internal.Session (Env (..), runSession, withGhcInSession)
import Prelude hiding (log)
import System.Environment (getEnv)
import System.FilePath ((</>))
import TestMake (makeModule)
import TestSetup (Conf (..), UnitConf, UnitMod (..), createDbExternal, withProject)

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

unitMod :: Conf -> [String] -> String -> String -> String -> UnitMod
unitMod conf deps name unit content =
  UnitMod {
    name,
    src = conf.tmp </> "src" </> unit </> name ++ ".hs",
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

sumTh :: [String] -> String
sumTh =
  foldl' (\ z a -> z ++ " + $(" ++ a ++ ")") "0"

modType2 :: Conf -> [String] -> Char -> Int -> [String] -> [String] -> UnitMod
modType2 conf pdeps unitTag n deps thDeps =
  unitMod conf ("clock" : "extra" : pdeps) modName unit content
  where
    unit = "unit-" ++ [unitTag]
    content =
      unlines $
        "{-# language TemplateHaskell #-}" :
        ("module " ++ modName ++ " where") :
        depImports ++
        [
          "import Data.List.Extra (headDef)",
          binding ++ " :: Int",
          binding ++ " = headDef 0 [0] + " ++ if useTh
          then sumTh thDeps
          else "5"
        ]
    depImports = ["import " ++ d | d <- deps]
    modName = toUpper unitTag : show n
    binding = unitTag : show n

a1Content :: String
a1Content =
  "module Dep1 where\na1 :: Int\na1 = 5"

errContent :: String
errContent =
  unlines [
    "module Err where",
    "num :: Int",
    "num = 5"
  ]

bugContent :: String
bugContent =
  unlines [
    "module Bug where",
    "import Language.Haskell.TH",
    "import Language.Haskell.TH.Syntax",
    "import Err",
    "bug :: ExpQ",
    "bug = lift @_ @Int num"
  ]

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

targets1 :: Conf -> [UnitMod]
targets1 conf =
  [
    unitMod conf [] "Err" "unit-a" errContent,
    unitMod conf ["unit-a"] "Bug" "unit-main" bugContent,
    unitMod conf [] "Main" "unit-main" main1
  ]

targets2 :: Conf -> [UnitMod]
targets2 conf =
  [
    unitMod conf [] "Err" "unit-b" errContent,
    m1 'b' 1 [],
    m1 'b' 2 [],
    m1d ["unit-b"] 'a' 0 ["B2"],
    modType2 conf ["unit-b"] 'a' 1 ["B1"] ["b1"],
    m1 'b' 3 [],
    m1d ["unit-b"] 'a' 2 ["A0", "A1", "B2", "B3"],
    unitMod conf ["unit-b"] "Bug" "unit-main" bugContent,
    unitMod conf ["unit-a", "unit-b", "clock", "extra"] "Main" "unit-main" (mainContent [
      ('a', 0),
      ('a', 2),
      ('b', 1)
    ])
  ]
  where
    m1 = modType1 conf []

    m1d = modType1 conf

mkExternalDeps :: Conf -> IO (Map UnitId String)
mkExternalDeps conf = do
  extraDir <- getEnv "extra_dir"
  clockDir <- getEnv "clock_dir"
  clockDb <- createDbExternal conf (stringToUnitId "clock") clockDir
  extraDb <- createDbExternal conf (stringToUnitId "extra") extraDir
  pure $ Map.fromList [
    (stringToUnitId "clock", clockDb),
    (stringToUnitId "extra", extraDb)
    ]

withDeps ::
  (Conf -> [UnitMod]) ->
  (Conf -> [UnitConf] -> Map UnitId String -> [UnitMod] -> IO ()) ->
  IO ()
withDeps mkTargets use =
  withProject (pure . mkTargets) \ conf units targets -> do
    external <- mkExternalDeps conf
    use conf units external targets

testWorker :: (Conf -> [UnitMod]) -> IO ()
testWorker mkTargets =
  withDeps mkTargets \ conf units external mods ->
    evalStateT (traverse_ (makeModule conf units external) mods) Set.empty

testMake :: IO ()
testMake =
  withDeps targets1 \ Conf {..} _ _ targets -> do
    log <- newLog True
    let env = Env {log, cache, args = args0}
        unitArgs =
          [
            ["-i", "-i" ++ (tmp </> "src/unit-b"), "-this-unit-id", "unit-b"],
            ["-i", "-i" ++ (tmp </> "src/unit-a"), "-this-unit-id", "unit-a", "-package-id", "unit-b"],
            ["-i", "-i" ++ (tmp </> "src/main"), "-this-unit-id", "unit-main", "-package-id", "unit-a", "-package-id", "unit-b"]
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
test1 = testWorker targets1
