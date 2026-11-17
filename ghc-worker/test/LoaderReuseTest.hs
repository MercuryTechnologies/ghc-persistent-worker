-- | A reused server's Template Haskell compile of a unit whose library another server compiled.
--
-- On GHC 9.14 the worker is built without @-flinkables@ (9.14 has no @hsc_linkables@), so its splices run on GHC's
-- own loader. GHC preloads a server's command-line packages only on its first request
-- ('GHC.Linker.Loader.initLoaderState'). On a later request a splice links only the packages it finds by walking the
-- module graph from the module being compiled. A unit restored from its build plan carries none of its external
-- packages into that graph, so a splice that runs the restored library's code finds that library's package unloaded.
-- A worker process then runs the package's closures unresolved: "Could not load '…_closure', dependency
-- unresolved", then SIGSEGV with @--optimistic-linking@, on a worker's second or later request and never its first.
module LoaderReuseTest where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import qualified Data.Set as Set
import GHC.Linker.Types (LoadedPkgInfo (..), Loader (..), LoaderState (..))
import GHC.Types.Unique.DFM (eltsUDFM)
import GHC.Unit.Types (unitIdString)
import Hedgehog (annotateShow, assert, evalMaybe)
import Test.Build (initialStrategy, resumeStrategy, runSchedule)
import Test.Bytecode (envLoader)
import Test.Cache (writeResumeCache)
import Test.Data.Env (MaxJobs (..), SessionEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), TaskKey (..), UnitKey (..))
import Test.Data.Scheduler (Schedule (..), Task (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.ExtDep (createExtDepPackageDbs)
import Test.ProjectBuild.Property (assertNoFailures)
import Test.Resource.Project (allModuleSources, mkUnit, schedule)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)

-- | A library without TH that depends on package 0, and a TH unit that imports no package but runs the library's
-- code in its splices, as a TH module runs the code of the libraries it imports. Only the library unit's package
-- dependencies name package 0. An unrelated TH unit compiles first, so its request's preload doesn't load package 0.
dependencyUnits :: [GenUnit BuildModule]
dependencyUnits =
  [ mkUnit True 2 (Set.singleton 1) (UnitKey 2) Set.empty,
    mkUnit False 2 (Set.singleton 0) (UnitKey 0) Set.empty,
    mkUnit True 2 Set.empty (UnitKey 1) (Set.singleton (UnitKey 0))
  ]

-- | The library is compiled by another server, as buck2 hands a dependency target's compiles to whichever worker is
-- free. The server that compiles the TH unit restores the library's unit from its plan but never compiles it, so none
-- of the library's code is in its home package table. The unrelated TH unit compiles first, so the TH unit's compile
-- is the server's second request and gets no command-line preload.
--
-- The servers share this process, and the RTS linker is per process: once any server has run a splice that needs
-- package 0, its code stays loaded for every later one, and nothing is unresolved. So the first server compiles only
-- the library, which has no splices, and the test asserts what the reused server's loader loaded rather than waiting
-- for a crash.
test_loaderReuseDependencyElsewhere :: TestTree
test_loaderReuseDependencyElsewhere =
  withTestEnv \ getEnv ->
    unitTest "TH compile on a reused server of a library another server compiled" do
      env0 <- liftIO (newSessionEnv =<< getEnv)
      extDepDbs <- liftIO (createExtDepPackageDbs env0.tempDir extDeps)
      let env = env0 {extDepDbs, extDeps}
      liftIO (writeProjectSources env.sourceDir (allModuleSources dependencyUnits))
      let initial = schedule dependencyUnits
      first <- liftIO (runSchedule (MaxJobs 1) (initialStrategy env) Set.empty (Schedule [t | t <- initial.tasks, not (compiledHere t.key)]))
      assertNoFailures "first server" first
      cached <- liftIO (writeResumeCache env initial)
      fresh0 <- liftIO (newResumeSessionEnv env)
      let fresh = fresh0 {extDepDbs, extDeps}
          compiles = Schedule [t | t <- cached.tasks, compiledHere t.key]
          elsewhere = Set.fromList [t.key | t <- cached.tasks, not (compiledHere t.key)]
      second <- liftIO (runSchedule (MaxJobs 1) (resumeStrategy fresh False) elsewhere compiles)
      assertNoFailures "reused server" second
      -- The splice ran the library's code, so the library's package has to be loaded.
      Loader lsVar <- evalMaybe =<< liftIO (envLoader fresh.env)
      LoaderState {pkgs_loaded} <- evalMaybe =<< liftIO (readMVar lsVar)
      let loaded = [unitIdString (loaded_pkg_uid i) | i <- eltsUDFM pkgs_loaded]
      annotateShow loaded
      assert ("extdep0" `elem` loaded)
  where
    extDeps = fold [bm.extDeps | u <- dependencyUnits, bm <- u.modules]

    -- The library, UnitKey 0, and every unit's metadata ran on the first server.
    compiledHere = \case
      TaskCompile ModuleKey {unit} -> unit /= UnitKey 0
      TaskMeta _ -> False
