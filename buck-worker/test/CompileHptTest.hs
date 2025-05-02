{-# language OverloadedLists #-}

module CompileHptTest where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Data.Char (toUpper)
import qualified Data.Foldable as Foldable
import Data.Foldable (fold, for_, traverse_)
import Data.Functor (void)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (DynFlags (..), Ghc, GhcException (..), GhcMode (..), GhcMonad (..), mkGeneralLocated, setUnitDynFlags)
import GHC.Debug.Stub (withGhcDebug)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Env (HscEnv (..), hscSetActiveUnitId, hscUpdateFlags)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Profiling.Eras (incrementUserEra)
import GHC.Unit (UnitId, UnitState (..), stringToUnitId, unitIdString)
import GHC.Unit.Env (HomeUnitEnv (..), HomeUnitGraph, UnitEnv (..), UnitEnvGraph (..), unitEnv_lookup_maybe)
import GHC.Utils.Monad (MonadIO (..))
import GHC.Utils.Panic (throwGhcExceptionIO)
import Internal.Args (Args (..))
import Internal.Cache (Target (..))
import Internal.CompileHpt (compileModuleWithDepsInHpt)
import Internal.Log (dbg, dbgs, newLog)
import Internal.Metadata (computeMetadataInSession)
import Internal.Session (Env (..), buckLocation, withGhcInSession, withGhcMhu, withUnitSpecificOptions)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))
import TestSetup (Conf (..), UnitConf (..), UnitMod (..), withProject)

debugState :: Bool
debugState = False

-- | Parse command line flags, used to create unit-specific @DynFlags@.
unitFlags :: [String] -> HscEnv -> Ghc DynFlags
unitFlags args HscEnv {hsc_logger, hsc_dflags = dflags0} = do
  (dflags, _, warns) <- parseDynamicFlagsCmdLine dflags0 (map (mkGeneralLocated "no loc") args)
  liftIO $ printOrThrowDiagnostics hsc_logger (initPrintConfig dflags) (initDiagOpts dflags) (GhcDriverMessage <$> warns)
  pure dflags

-- | Turn each @-package@ argument in the given list into a @-package-id@ argument if the following package name refers
-- to a home unit, and create a set of all mentioned home unit names.
-- This is needed for GHC to recognize home unit dependencies.
adaptHp :: HomeUnitGraph -> [String] -> (Set UnitId, [String])
adaptHp (UnitEnvGraph ueg) =
  spin mempty
  where
    spin seen = \case
      "-package" : p : rest
        | Map.member (stringToUnitId p) ueg
        -> (["-package-id", p] ++) <$> spin (Set.insert (stringToUnitId p) seen) rest
      arg : rest -> (arg :) <$> spin seen rest
      [] -> (seen, [])

-- | Return the given unit's dependencies.
homeUnitDeps :: HscEnv -> UnitId -> Maybe [UnitId]
homeUnitDeps hsc_env target = do
  HomeUnitEnv {homeUnitEnv_units = UnitState {homeUnitDepends}} <- unitEnv_lookup_maybe target hug
  pure homeUnitDepends
  where
    hug = hsc_env.hsc_unit_env.ue_home_unit_graph

-- | Assemble @-package-id@ arguments for the current unit's dependencies, omitting those that are present in the
-- provided set, which are the current module's deps specified by Buck.
homeUnitDepFlags :: HscEnv -> Set UnitId -> UnitId -> [String]
homeUnitDepFlags hsc_env explicit target =
  concat [["-package-id", unitIdString u] | u <- fold prev_deps, not (Set.member u explicit)]
  where
    prev_deps = homeUnitDeps hsc_env target

-- | Update the current unit's @DynFlags@ stored in the unit env, and reinitialize its unit state.
-- Since different modules in the same unit may have arbitrary subsets of the unit's package dependencies when Buck
-- compiles them, we take the union of existing and new dependencies.
initUnit :: [String] -> Ghc ()
initUnit specific = do
  hsc_env0 <- getSession
  let current = hsc_env0.hsc_unit_env.ue_current_unit
      (explicit, withPackageId) = adaptHp hsc_env0.hsc_unit_env.ue_home_unit_graph specific
      unitOptions = withPackageId ++ homeUnitDepFlags hsc_env0 explicit current
  dflags <- unitFlags unitOptions hsc_env0
  setUnitDynFlags current dflags
  modifySession (hscSetActiveUnitId current)

incEra :: MonadIO m => m ()
incEra =
  when enable do
    void $ liftIO $ incrementUserEra 1
  where
    enable = False

-- | Approximate synthetic reproduction of what happens when the metadata step is performed by the worker.
loadModuleGraph :: Env -> UnitMod -> [String] -> Ghc (Maybe Bool)
loadModuleGraph env UnitMod {src} specific = do
  names <- liftIO $ listDirectory dir
  let srcs = [dir </> name | name <- names, takeExtension name == ".hs"]
  computeMetadataInSession (initUnit specific) env srcs
  where
    dir = takeDirectory src

-- | Compile a single module using 'compileHpt' roughly like it happens when Buck requests it.
-- If the module's home unit has not been encountered before, simulate the metadata step by doctoring the session,
-- running downsweep, and merging the resulting module graph into the persisted state.
makeModule :: Conf -> [UnitConf] -> UnitMod -> StateT (Set String) IO ()
makeModule Conf {..} units umod@UnitMod {unit, src, deps} = do
  log <- newLog True
  liftIO $ createDirectoryIfMissing False sessionTmpDir
  let env = Env {log, cache, args}
      envM = env {args = env.args {ghcOptions = mkDependArgs ++ env.args.ghcOptions}}
  firstTime <- state \ seen ->
    if Set.member unit seen
    then (False, seen)
    else (True, Set.insert unit seen)
  when firstTime do
    incEra
    success <- fmap (fromMaybe Nothing) $ liftIO $ withUnitSpecificOptions False envM \ env1 specific argv -> do
      flip (withGhcInSession env1) (argv ++ fmap buckLocation dbsM) \ _ -> do
        dbg ""
        dbg (">>> metadata for " ++ unit)
        Just <$> loadModuleGraph env1 umod specific
    unless (success == Just True) do
      liftIO $ throwGhcExceptionIO (ProgramError "Metadata failed")
  incEra
  result <- liftIO $ withGhcMhu env \ _ target -> do
    dbg ""
    dbg (">>> compiling " ++ takeFileName target.get)
    modifySession $ hscUpdateFlags \ d -> d {ghcMode = CompManager}
    compileModuleWithDepsInHpt target
  when (isNothing result) do
      liftIO $ throwGhcExceptionIO (ProgramError "Compile failed")
  -- liftIO $ threadDelay 2_000_000
  dbgs result
  where
    args =
      args0 {
        ghcOptions = args0.ghcOptions ++ fileOptions,
        -- Ensure that each module gets a separate temp directory, to resemble circumstances in a Buck build
        tempDir = Just sessionTmpDir
      }

    mkDependArgs = [
      "-dep-json=" ++ (sessionTmpDir </> "dep.json"),
      "-dep-makefile=" ++ (sessionTmpDir </> "dep.make"),
      "-include-pkg-deps"
      ]

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
  Foldable.foldl' (\ z a -> z ++ " + $(" ++ a ++ ")") "0"

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
  withProject (pure . mkTargets) \ conf units targets -> do
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
    dbgs =<< listDirectory (conf.tmp </> "out")

-- | A very simple test consisting of two home units, using a transitive TH dependency across unit boundaries.
test_compileHpt :: IO ()
test_compileHpt = do
  withGhcDebug do
    testWorker targets1
    when True do
      liftIO $ threadDelay 5_000_000
