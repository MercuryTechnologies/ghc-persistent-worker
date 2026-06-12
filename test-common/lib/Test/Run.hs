module Test.Run where

import Control.Concurrent (MVar)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (controlT)
import Data.Foldable (for_, toList)
import Data.IORef (IORef, readIORef)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (Ghc)
import GHC.Driver.Monad (reflectGhc, reifyGhc)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.Types.Error (diagnosticCodeNumber)
import Hedgehog (MonadTest, TestT, annotate, evalMaybe, failure, property, test, withTests, (===))
import Hedgehog.Internal.Property (failWith)
import Internal.Error (handleExceptions)
import Internal.Session (cleanupSession, initSession, runGhc, simpleSessionWithDebugLog)
import Internal.State (newState)
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

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))

assertJust ::
  forall a m .
  Eq a =>
  Show a =>
  Monad m =>
  HasCallStack =>
  a ->
  Maybe a ->
  TestT m ()
assertJust a mb =
  withFrozenCallStack do
    b <- evalMaybe mb
    a === b

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
persistentSession :: (MonadIO m, MonadTest m) => MVar WorkerState -> [String] -> Ghc a -> m a
persistentSession state ghcOptions ma =
  evalMaybe =<< liftIO (simpleSessionWithDebugLog state (emptyArgs []) {ghcOptions} ma)

-- | Convenience session runner that creates a one-time use @WorkerState@ prints all log messages to stderr afterwards.
transientSession :: (MonadIO m, MonadTest m) => [String] -> Ghc a -> m a
transientSession ghcOptions ma = do
  state <- liftIO newState
  persistentSession state ghcOptions ma

mkEnv :: IO (Env, IORef TestLog)
mkEnv = do
  state <- newState
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

sessionFailedMessage :: String -> TestLog -> String
sessionFailedMessage desc TestLog {diagnostics, fatal, messages} =
  unlines (headline : diagSection ++ fatalSection ++ debugSection)
  where
    headline = "The test session '" ++ desc ++ "' failed (returning Nothing)."

    diagSection = section "Diagnostics:" [d.rendered | d <- diagnostics]

    fatalSection = section "Fatal errors:" fatal

    debugSection = section "Debug messages:" messages

    section title msgs =
      if null msgs
      then []
      else "" : title : concat [["", msg] | msg <- msgs]

sessionFailed ::
  HasCallStack =>
  String ->
  TestLog ->
  TestT IO a
sessionFailed desc log =
  withFrozenCallStack do
    annotate (sessionFailedMessage desc log)
    failure

-- | A handler for use with 'testSession' that ensures that only diagnostics were emitted that are present in the given
-- set of error codes.
expectDiagnostics ::
  HasCallStack =>
  Set Natural ->
  TestLog ->
  TestT IO ()
expectDiagnostics expected TestLog {diagnostics} =
  withFrozenCallStack do
    for_ (nonEmpty offenders) \ diags ->
      failWith Nothing $ unlines $ "The test session emitted unexpected diagnostics:" :
      concat [["", "Code " ++ maybe "<unknown>" show code ++ ":", "", msg] | (msg, code) <- toList diags]
  where
    offenders = [(d.rendered, d.code) | d <- diagnostics, unexpected d.code]

    unexpected = maybe False (not . flip Set.member expected . diagnosticCodeNumber)

expectNoDiagnostics ::
  HasCallStack =>
  TestLog ->
  TestT IO ()
expectNoDiagnostics =
  withFrozenCallStack do
    expectDiagnostics []

checkSessionResult ::
  HasCallStack =>
  String ->
  (TestLog -> TestT IO ()) ->
  (Maybe a, TestLog) ->
  TestT IO a
checkSessionResult desc checkLog (result, log) = do
  checkLog log
  annotate (sessionFailedMessage desc log)
  evalMaybe result

-- | Run a GHC session with a fresh logger and return the result alongside the log.
testSessionMain ::
  MVar WorkerState ->
  Args ->
  (Env -> TestT Ghc a) ->
  TestT IO (Maybe a, TestLog)
testSessionMain state args prog = do
  (log, logVar) <- liftIO newTestLog
  let env = Env {log, state, args}
  session <- liftIO $ initSession env
  result <- controlT \ lowerTest ->
    withCleanup session $ runGhc session $ withHandler log do
      (result, journal) <- lowerTest (prog env)
      pure (Just <$> result, journal)
  logOutput <- liftIO $ readIORef logVar
  pure (result, logOutput)
  where
    withHandler log = handleExceptions log (Right Nothing, mempty)

    withCleanup session = flip finally (cleanupSession session)

-- | Run a GHC session and fail with a Hedgehog error if the result is 'Nothing', indicating that an exception was
-- thrown.
-- Creates an 'Env' from the args and a fresh test log and passes it to the callback.
-- The second argument passed to the callback is a lowering function from 'Ghc' to 'IO', allowing the test to be in 'IO'
-- at the top level and embedding a 'Ghc' program conveniently.
--
-- Takes another callback that may assert properties about the log, like expecting diagnostics.
--
-- Note: 'withFrozenCallStack' is used only for the assertion part.
-- This has the effect that Hedgehog displays the caller's location for assertions in 'checkSessionResult', while
-- leaving assertions in @prog@ unchanged.
-- However, it requires all of the variants of this function below to duplicate the implementation.
testSessionWith ::
  HasCallStack =>
  String ->
  MVar WorkerState ->
  Args ->
  (TestLog -> TestT IO ()) ->
  (Env -> TestT Ghc b) ->
  TestT IO b
testSessionWith desc state args checkLog prog = do
  result <- testSessionMain state args prog
  withFrozenCallStack do
    checkSessionResult desc checkLog result

testSessionSuccess ::
  HasCallStack =>
  String ->
  MVar WorkerState ->
  Args ->
  (Env -> TestT Ghc b) ->
  TestT IO b
testSessionSuccess desc state args prog = do
  result <- testSessionMain state args prog
  withFrozenCallStack do
    checkSessionResult desc expectNoDiagnostics result

-- Like 'testSessionWith', but takes only 'WorkerState', creates empty 'Args'
testSessionState ::
  HasCallStack =>
  String ->
  MVar WorkerState ->
  (TestLog -> TestT IO ()) ->
  (Env -> TestT Ghc b) ->
  TestT IO b
testSessionState desc state checkLog prog = do
  result <- testSessionMain state (emptyArgs []) prog
  withFrozenCallStack do
    checkSessionResult desc checkLog result

-- Like 'testSessionState', but:
-- - Creates a fresh 'WorkerState'
-- - Only requires the session not to be failed and emit no diagnostics, so takes no log check callback
testSessionFresh ::
  HasCallStack =>
  String ->
  (Env -> TestT Ghc b) ->
  TestT IO b
testSessionFresh desc prog = do
  state <- liftIO newState
  result <- testSessionMain state (emptyArgs []) prog
  withFrozenCallStack do
    checkSessionResult desc expectNoDiagnostics result

-- Like 'testSessionFresh', but takes a 'Ghc' program instead of providing the lowering function.
testSessionGhc ::
  HasCallStack =>
  String ->
  (Env -> Ghc a) ->
  TestT IO a
testSessionGhc desc prog = do
  state <- liftIO newState
  result <- testSessionMain state (emptyArgs []) (lift . prog)
  withFrozenCallStack do
    checkSessionResult desc expectNoDiagnostics result

-- Like 'testSessionState', but:
-- - Only requires the session not to be failed and emit no diagnostics, so takes no log check callback
-- - Takes a 'Ghc' program instead of providing the lowering function
testSession ::
  HasCallStack =>
  String ->
  MVar WorkerState ->
  (Env -> Ghc a) ->
  TestT IO a
testSession desc state prog = do
  result <- testSessionMain state (emptyArgs []) (lift . prog)
  withFrozenCallStack do
    checkSessionResult desc expectNoDiagnostics result
