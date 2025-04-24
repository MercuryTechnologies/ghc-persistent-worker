module CompileHptTest where

import Control.Concurrent (readMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Data.Char (toUpper)
import Data.Foldable (for_, traverse_)
import Data.Functor (void, (<&>))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (
  DynFlags (..),
  Ghc,
  GhcException (..),
  GhcMode (..),
  ModSummary (..),
  ModuleGraph,
  getSession,
  guessTarget,
  mgModSummaries,
  mkModuleGraph,
  ms_mod_name,
  setTargets,
  unLoc,
  )
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags, hsc_home_unit)
import GHC.Driver.Make (downsweep)
import GHC.Driver.Monad (modifySession, modifySessionM, withTempSession)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.Error (unionManyMessages)
import GHC.Unit (homeUnitId, moduleUnitId)
import GHC.Unit.Env (UnitEnv (..), unitEnv_union)
import GHC.Unit.Finder (addHomeModuleToFinder)
import GHC.Utils.Outputable (ppr, text, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.Args (Args (..))
import Internal.Cache (Cache (..), Target (..), mergeHugs, newFinderCache, updateModuleGraph)
import Internal.CompileHpt (adaptHp, compileModuleWithDepsInHpt, initUnit)
import Internal.Debug (showHugShort, showModGraph)
import Internal.Log (dbg, dbgp, dbgs, newLog)
import Internal.Session (Env (..), dummyLocation, withGhcInSession, withGhcMhu, withUnitSpecificOptions)
import Prelude hiding (log)
import System.Directory (listDirectory, removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>), takeBaseName)
import TestSetup (Conf (..), UnitConf (..), UnitMod (..), withProject)
import Data.List.NonEmpty (NonEmpty)

debugState :: Bool
debugState = False

-- | Approximate synthetic reproduction of what happens when the metadata step is performed by the worker.
loadModuleGraph :: Env -> UnitMod -> [String] -> Ghc ModuleGraph
loadModuleGraph env UnitMod {src} specific = do
  cache <- liftIO $ readMVar env.cache
  (_, module_graph) <- withTempSession (maybe id restoreHug cache.hug . maybe id restoreModuleGraph cache.moduleGraph) do
    modifySessionM \ hsc_env -> do
      hsc_FC <- liftIO $ newFinderCache env.cache cache (Target "metadata")
      pure hsc_env {hsc_FC}
    dbg (unwords specific)
    initUnit specific
    when debugState do
      mg <- hsc_mod_graph <$> getSession
      dbgp ("existing graph:" <+> showModGraph mg)
      dbg "hug:"
      dbgp . showHugShort . ue_home_unit_graph . hsc_unit_env =<< getSession
    names <- liftIO $ listDirectory dir
    let srcs = [dir </> name | name <- names, takeExtension name == ".hs"]
    targets <- mapM (\s -> guessTarget s Nothing Nothing) srcs
    setTargets targets
    hsc_env1 <- getSession
    (errs, graph_nodes) <- liftIO $ downsweep hsc_env1 [] [] True
    let
      mod_graph = mkModuleGraph graph_nodes
    pure (unionManyMessages errs, mod_graph)
  when debugState do
    dbgp (text "unit module graph:" <+> showModGraph module_graph)
  liftIO $ updateModuleGraph env.cache module_graph
  pure module_graph
  where
    dir = takeDirectory src

    restoreModuleGraph mg e = e {hsc_mod_graph = mg}

    restoreHug hug e =
      e {hsc_unit_env = e.hsc_unit_env {ue_home_unit_graph = unitEnv_union mergeHugs hug e.hsc_unit_env.ue_home_unit_graph}}

-- | Compile a single module using 'compileHpt' roughly like it happens when Buck requests it.
-- If the module's home unit has not been encountered before, simulate the metadata step by doctoring the session,
-- running downsweep, and merging the resulting module graph into the persisted state.
makeModule :: Conf -> [UnitConf] -> UnitMod -> StateT (Set String) IO ()
makeModule Conf {..} units umod@UnitMod {unit, src, deps} = do
  log <- newLog True
  liftIO $ createDirectoryIfMissing False sessionTmpDir
  let env = Env {log, cache, args}
      envC = env {args = env.args {ghcOptions = env.args.ghcOptions}}
  firstTime <- state \ seen ->
    if Set.member unit seen
    then (False, seen)
    else (True, Set.insert unit seen)
  when firstTime do
    success <- fmap (fromMaybe Nothing) $ liftIO $ withUnitSpecificOptions False env \ env1 specific argv -> do
      (_, withPackageId) <- liftIO $ readMVar cache <&> \case
        Cache {hug}
          | Just h <- hug
          -> fmap dummyLocation <$> adaptHp h (unLoc <$> argv)
          | otherwise
          -> (mempty, argv)
      flip (withGhcInSession env1) (withPackageId ++ fmap dummyLocation dbsM) \ _ -> do
        dbg ""
        dbg (">>> metadata for " ++ unit)
        modifySession $ hscUpdateFlags \ d -> d {ghcMode = MkDepend}
        initializeSessionPlugins
        graph <- loadModuleGraph env1 umod specific
        pure (Just (Just graph))
    when debugState do
      liftIO $ readMVar cache >>= \ Cache {..} -> for_ moduleGraph \ mg ->
        dbgp (text "updated module graph:" <+> showModGraph mg)
    void $ case success of
      Just module_graph ->
        liftIO $ withGhcMhu envC \ specific _ -> do
          -- TODO while this makes the modules discoverable in @depanal@, it also causes @compileModuleWithDepsInHpt@ to break,
          -- when it looks up @unit-main:Err@ for some reason.
          when False do
            dbg ""
            dbg (">>> updating Finder")
            modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
            initUnit specific
            hsc_env <- getSession
            liftIO $ for_ (mgModSummaries module_graph) \ m -> do
              when (homeUnitId (hsc_home_unit hsc_env) == moduleUnitId (ms_mod m)) do
                dbgp ("add module:" <+> ppr (homeUnitId (hsc_home_unit hsc_env)) <+> ppr (ms_mod_name m))
                void $ addHomeModuleToFinder (hsc_FC hsc_env) (hsc_home_unit hsc_env) (ms_mod_name m) (ms_location m)
          pure (Just ())
      Nothing ->
        liftIO $ throwGhcExceptionIO (ProgramError "Metadata failed")
  result <- liftIO $ withGhcMhu envC \ specific target -> do
    dbg ""
    dbg (">>> compiling " ++ takeFileName target.get)
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    compileModuleWithDepsInHpt specific target
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
        -- "-i" ++ takeDirectory src,
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

    dbsM = concat [["-package-db", db]  | UnitConf {db} <- units]

unitMod :: Conf -> [String] -> String -> String -> String -> UnitMod
unitMod conf deps name unit content =
  UnitMod {
    name,
    src = conf.tmp </> "src" </> unit </> name ++ ".hs",
    unit,
    ..
  }

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

targets1 :: Conf -> NonEmpty UnitMod
targets1 conf =
  [
    unitMod conf [] "Err" "unit-a" errContent,
    unitMod conf ["unit-a"] "Bug" "unit-main" bugContent,
    unitMod conf [] "Main" "unit-main" main1
  ]

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
  unitMod conf (pdeps) modName unit content
  where
    unit = "unit-" ++ [unitTag]
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

targets2 :: Conf -> NonEmpty UnitMod
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
    unitMod conf ["unit-a", "unit-b"] "Main" "unit-main" (mainContent [
      ('a', 0),
      ('a', 2),
      ('b', 1)
    ])
  ]
  where
    m1 = modType1 conf []

    m1d = modType1 conf

removeGhcTmpDir :: Conf -> IO ()
removeGhcTmpDir conf = do
  dirs <- liftIO (listDirectory (conf.tmp </> "tmp"))
  for_ dirs \ ghcTmpRel -> do
    let ghcTmp = conf.tmp </> "tmp" </> ghcTmpRel
    liftIO $ removeDirectoryRecursive ghcTmp

testWorker :: (Conf -> NonEmpty UnitMod) -> IO ()
testWorker mkTargets =
  withProject (pure . mkTargets) \ conf units targets ->
    flip evalStateT Set.empty do
      traverse_ (makeModule conf units) targets
      -- Simulate the case of Buck deleting the temp dir and recompiling a module, which goes unnoticed by GHC because
      -- it tracks all created temp dirs in an @IORef@ that the worker shares across sessions.
      -- When initializing a new module session, all dirs already present in the state are assumed to exist on disk and
      -- not recreated.
      -- As a consequence, it will try to write the assembly files @ghc_1.s@ to a nonexistent directory, so we have to
      -- ensure that each session gets its own @TmpFs@.
      liftIO (removeGhcTmpDir conf)
      makeModule conf units (NonEmpty.last targets)

-- | A very simple test consisting of two home units, using a transitive TH dependency across unit boundaries.
test_compileHpt :: IO ()
test_compileHpt =
  testWorker targets1
