module Test1 where

import Control.Concurrent (MVar, readMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Data.Char (toUpper)
import Data.Foldable (find, fold, for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.List (intercalate, stripPrefix)
import Data.List.Extra (nubOrd)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!?))
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (
  DynFlags (..),
  Ghc,
  GhcException (..),
  GhcMode (..),
  ModLocation (..),
  getSession,
  guessTarget,
  setTargets,
  unLoc,
  )
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Make (depanal)
import GHC.Driver.Monad (modifySession, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Unit (UnitId, installedModuleEnvElts, stringToUnitId, unitIdString)
import GHC.Unit.Finder (InstalledFindResult (..))
import GHC.Utils.Outputable (SDoc, hcat, ppr, showPprUnsafe, text, vcat, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), CacheFeatures (..), emptyCacheWith, finderEnv, updateModuleGraph)
import Internal.CompileHpt (adaptHp, compileHpt)
import Internal.CompileMake (step1, step2)
import Internal.Debug (entries, showModGraph, showUnitEnv)
import Internal.Log (dbg, dbgp, dbgs, newLog)
import Internal.Session (Env (..), dummyLocation, runSession, withGhcGeneral, withGhcInSession)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, runProcess_)

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
    name :: String,
    src :: FilePath,
    unit :: String,
    deps :: [String],
    content :: String
  }
  deriving stock (Eq, Show)

data UnitConf =
  UnitConf {
    uid :: UnitId,
    db :: FilePath,
    mods :: NonEmpty UnitMod,
    externalDeps :: [UnitId]
  }
  deriving stock (Eq)

loadModuleGraph :: Env -> UnitMod -> Ghc ()
loadModuleGraph env UnitMod {src} = do
  module_graph <- withTempSession id do
    names <- liftIO $ listDirectory dir
    let srcs = [dir </> name | name <- names, takeExtension name == ".hs"]
    targets <- mapM (\s -> guessTarget s Nothing Nothing) srcs
    setTargets targets
    depanal [] True
  liftIO $ updateModuleGraph env.cache module_graph
  where
    dir = takeDirectory src

one :: Conf -> [UnitConf] -> Map UnitId String -> UnitMod -> StateT (Set String) IO ()
one Conf {..} units external umod@UnitMod {unit, src, deps} = do
  dbg "---------------------------------"
  log <- newLog True
  let env = Env {log, cache, args}
  firstTime <- state \ seen ->
    if Set.member unit seen
    then (False, seen)
    else (True, Set.insert unit seen)
  when firstTime do
    success <- fmap (fromMaybe False) $ liftIO $ runSession False env \ argv -> do
      dbg ("metadata for " ++ unit)
      (_, withPackageId) <- liftIO $ readMVar cache <&> \case
        Cache {hug}
          | Just h <- hug
          -> fmap dummyLocation <$> adaptHp h (unLoc <$> argv)
          | otherwise
          -> (mempty, argv)
      let dbs = concat [["-package-db", db] ++ concat (mapMaybe externalDb externalDeps) | UnitConf {db, externalDeps} <- units]
      flip (withGhcInSession env) (withPackageId ++ fmap dummyLocation dbs) \ _ -> do
        modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
        initializeSessionPlugins
        loadModuleGraph env umod
        pure (Just True)
    unless success do
      liftIO $ throwGhcExceptionIO (ProgramError "Metadata failed")
  let dbs = concat (fold externalDepDbs)
  result <- liftIO $ withGhcGeneral env {args = env.args {ghcOptions = dbs ++ env.args.ghcOptions}} \ specific target -> do
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    result <- compileHpt specific target
    pure result
  when (isNothing result) do
      liftIO $ throwGhcExceptionIO (ProgramError "Compile failed")
  dbgs result
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

    externalDepDbs = do
      UnitConf {externalDeps} <- unitConf
      pure (mapMaybe externalDb externalDeps)

    unitConf = find ((== stringToUnitId unit) . (.uid)) units

    externalDb dep = external !? dep <&> \ dir -> ["-package-db", dir]

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

mainContent :: [(Char, Int)] -> String
mainContent deps =
  unlines $
    "{-# language TemplateHaskell #-}" :
    "module Main where" :
    ["import " ++ toUpper c : show i | (c, i) <- deps] ++
    [
      "main :: IO ()",
      "main = print (" ++ (if useTh then sumTh names else intercalate " + " ("0" : names)) ++ ")"
    ]
  where
    names = [c : show i | (c, i) <- deps]

baseArgs :: FilePath -> FilePath -> Args
baseArgs topdir tmp =
  Args {
    topdir = Just topdir,
    workerTargetId = Just "test",
    env = mempty,
    binPath = [],
    tempDir = Just (tmp </> "tmp"),
    ghcPath = Nothing,
    ghcOptions = (artifactDir =<< ["o", "hie", "dump"]) ++ [
      "-fwrite-ide-info",
      "-no-link",
      "-dynamic",
      -- "-fwrite-if-simplified-core",
      "-fbyte-code-and-object-code",
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
    m1d ["unit-b"] 'a' 0 ["B2"],
    modType2 conf ["unit-b"] 'a' 1 ["B1"] ["b1"],
    m1 'b' 3 [],
    m1d ["unit-b"] 'a' 2 ["A0", "A1", "B2", "B3"],
    unitMod conf ["unit-a", "unit-b", "clock", "extra"] "Main" "main" (mainContent [
      ('a', 0),
      ('a', 2),
      ('b', 1)
    ])
  ]
  where
    m1 = modType1 conf []

    m1d = modType1 conf

dbConf :: String -> String -> [UnitMod] -> String
dbConf srcDir unit mods =
  unlines $ [
    "name: " ++ unit,
    "version: 1.0",
    "id: " ++ unit,
    "key: " ++ unit,
    "import-dirs: " ++ srcDir,
    "exposed: True",
    "exposed-modules:"
  ] ++
  exposed
  where
    exposed = [name | UnitMod {name} <- mods]

createDb :: String -> String -> String -> IO String
createDb ghcPkg dir confFile = do
  dbg ("create db for " ++ confFile ++ " at " ++ db)
  createDirectoryIfMissing False db
  runProcess_ (proc ghcPkg ["--package-db", db, "recache"])
  runProcess_ (proc ghcPkg ["--package-db", db, "register", "--force", confFile])
  pure db
  where
    db = dir </> "package.conf.d"

createDbUnitMod :: String -> Set String -> NonEmpty UnitMod -> IO UnitConf
createDbUnitMod ghcPkg homeUnits mods@(mod0 :| _) = do
  let uid = stringToUnitId mod0.unit
  writeFile confFile (dbConf dir mod0.unit (toList mods))
  db <- createDb ghcPkg dir confFile
  pure UnitConf {..}
  where
    externalDeps = stringToUnitId <$> filter (not . flip Set.member homeUnits) (nubOrd (concat ((.deps) <$> mods)))
    confFile = dir </> (mod0.unit ++ ".conf")
    dir = takeDirectory mod0.src

createDbExternal :: String -> String -> String -> UnitId -> String -> IO String
createDbExternal ghcPkg tmp libPath unit storePath = do
  name <- listDirectory pcd <&> \case
      [conf] -> conf
      ds -> error ("weird package.conf.d dir for " ++ showPprUnsafe unit ++ " contains /= 1 entries: " ++ show ds)
  let confFile = pcd </> name
  createDirectoryIfMissing False dir
  createDb ghcPkg dir confFile
  where
    dir = tmp </> unitIdString unit
    pcd = storePath </> libPath </> "package.conf.d"

withProject ::
  (Conf -> [UnitConf] -> Map UnitId String -> [UnitMod] -> IO ()) ->
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
    libPath <- listDirectory (ghc_dir </> "lib") <&> \case
      [d] -> "lib" </> d </> "lib"
      ds -> error ("weird GHC lib dir contains /= 1 entries: " ++ show ds)
    let topdir = ghc_dir </> libPath
        conf = Conf {tmp, cache, args0 = baseArgs topdir tmp}
        targets = targets1 conf
    for_ targets \ UnitMod {src, content} -> do
      createDirectoryIfMissing False (takeDirectory src)
      writeFile src content
    let unitMods = NonEmpty.groupAllWith (.unit) targets
        homeUnits = Set.fromList [unit | UnitMod {unit} :| _ <- unitMods]
    let ghcPkg = ghc_dir </> "bin/ghc-pkg"
    units <- traverse (createDbUnitMod ghcPkg homeUnits) unitMods
    extraDir <- getEnv "extra_dir"
    clockDir <- getEnv "clock_dir"
    let db = createDbExternal ghcPkg (tmp </> "tmp") libPath
    clockDb <- db (stringToUnitId "clock") clockDir
    extraDb <- db (stringToUnitId "extra") extraDir
    let external = Map.fromList [(stringToUnitId "clock", clockDb), (stringToUnitId "extra", extraDb)]
    use conf units external targets
    -- dbgs =<< listDirectory (tmp </> "out")

testWorker :: IO ()
testWorker =
  withProject \ conf units external mods ->
    evalStateT (traverse_ (one conf units external) mods) Set.empty

testMake :: IO ()
testMake =
  withProject \ Conf {..} _ _ targets -> do
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
