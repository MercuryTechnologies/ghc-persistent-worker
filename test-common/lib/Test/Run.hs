module Test.Run where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, toList)
import Data.IORef (IORef, readIORef)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (Ghc)
import GHC.Driver.Monad (reflectGhc, reifyGhc)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.Types.Error (diagnosticCodeNumber)
import Hedgehog (TestT, evalMaybe, property, test, withTests)
import Hedgehog.Internal.Property (failWith)
import Internal.Session (runSession, simpleSessionWithDebugLog)
import Internal.State (newState, newStateWith)
import Numeric.Natural (Natural)
import Prelude hiding (log)
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Data.TestLog (DiagnosticEntry (..), TestLog (..))
import Test.Log (newTestLog)
import Test.Tasty (TestName, TestTree, withResource)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (Args (..), emptyArgs)
import Types.Env (Env (..))
import Types.State (WorkerState)
import Types.State.Oneshot (OneshotCacheFeatures (..))

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))

acquireTemp :: FilePath -> IO FilePath
acquireTemp name = do
  tmpBase <- getCanonicalTemporaryDirectory
  createTempDirectory tmpBase name

-- | Use a temp dir for a Tasty test.
-- We use this instead of @withSystemTempDirectory@ because 'TestT' doesn't have @MonadMask@.
withTemp :: FilePath -> (IO FilePath -> TestTree) -> TestTree
withTemp name =
  withResource (acquireTemp name) removeDirectoryRecursive

-- | Convenience session runner that prints all log messages to stderr afterwards.
persistentSession :: MVar WorkerState -> [String] -> Ghc a -> TestT IO a
persistentSession state ghcOptions ma =
  evalMaybe =<< liftIO (simpleSessionWithDebugLog state (emptyArgs []) {ghcOptions} ma)

-- | Convenience session runner that creates a one-time use @WorkerState@ prints all log messages to stderr afterwards.
transientSession :: [String] -> Ghc a -> TestT IO a
transientSession ghcOptions ma = do
  state <- liftIO $ newState False
  persistentSession state ghcOptions ma

mkEnv :: IO (Env, IORef TestLog)
mkEnv = do
  state <- newStateWith OneshotCacheFeatures {
    loader = False,
    enable = True,
    names = False,
    finder = False,
    eps = False
  }
  (log, logVar) <- newTestLog
  pure (Env {
    log,
    state,
    args = emptyArgs []
  }, logVar)

lowerGhc ::
  forall b .
  ((forall a . Ghc a -> IO a) -> IO b) ->
  Ghc b
lowerGhc use =
  reifyGhc \ session ->
    use (flip reflectGhc session)

sessionFailed ::
  HasCallStack =>
  TestLog ->
  TestT IO a
sessionFailed TestLog {diagnostics, fatal} =
  withFrozenCallStack do
    failWith Nothing $ unlines $
      "The test session failed (returning Nothing)." : diagSection ++ fatalSection
  where
    diagSection =
      section "Diagnostics:" [d.rendered | d <- diagnostics]

    fatalSection =
      section "Fatal errors:" fatal

    section title msgs =
      if null msgs
      then []
      else "" : title : concat [["", msg] | msg <- msgs]

-- | Run a GHC session and fail with a Hedgehog error if the result is 'Nothing', indicating that an exception was
-- thrown.
-- Creates a fresh 'Env' and passes it to the callback.
-- The second argument passed to the callback is a lowering function from 'Ghc' to 'IO', allowing the test to be in 'IO'
-- at the top level and embedding a 'Ghc' program conveniently.
testSession ::
  HasCallStack =>
  (TestLog -> TestT IO ()) ->
  (Env -> (forall a . Ghc a -> IO a) -> IO (Maybe b)) ->
  TestT IO b
testSession checkLog prog = do
  (result, log) <- liftIO do
    (env, logVar) <- mkEnv
    result <- runSession env \ _ -> lowerGhc (prog env)
    log <- readIORef logVar
    pure (result, log)
  withFrozenCallStack do
    checkLog log
    maybe (sessionFailed log) pure result

-- | A handler for use with 'testSession' that ensures that only diagnostics were emitted that are present in the given
-- set of error codes.
expectDiagnostics ::
  Set Natural ->
  TestLog ->
  TestT IO ()
expectDiagnostics expected TestLog {diagnostics} =
  for_ (nonEmpty offenders) \ diags ->
    failWith Nothing $ unlines $ "The test session emitted unexpected diagnostics:" :
    concat [["", "Code " ++ show code ++ ":", "", msg] | (msg, code) <- toList diags]
  where
    offenders = [(d.rendered, d.code) | d <- diagnostics, unexpected d.code]

    unexpected = maybe False (not . flip Set.member expected . diagnosticCodeNumber)

expectNoDiagnostics :: TestLog -> TestT IO ()
expectNoDiagnostics = expectDiagnostics []
