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

-- | Parameters for a GHC session run in a test.
data TestSessionConfig a =
  TestSessionConfig {
    -- | Assert properties about the GHC log, like diagnostics.
    checkLog :: TestLog -> TestT IO (),

    -- | If the test has to prepare the state, it can be provided explicitly.
    state :: Maybe (MVar WorkerState),

    -- | Args can be customized.
    args :: Args,

    -- | The full test program.
    -- 'defTestGhc' provides a simpler signature for conveniece.
    program :: Env -> TestT Ghc a
  }

defTest ::
  (Env -> TestT Ghc a) ->
  TestSessionConfig a
defTest program =
  TestSessionConfig {
    checkLog = expectNoDiagnostics,
    state = Nothing,
    args = emptyArgs [],
    program
  }

defTestGhc ::
  (Env -> Ghc a) ->
  TestSessionConfig a
defTestGhc program =
  defTest (lift . program)

-- | Run a GHC session and fail with a Hedgehog error if the result is 'Nothing', indicating that an exception was
-- thrown.
-- Creates an 'Env' from the args and a fresh test log and passes it to the callback.
--
-- 'checkLog' may assert properties about the log, like expecting diagnostics.
--
-- Note: 'withFrozenCallStack' is used only for the assertion part.
-- This has the effect that Hedgehog displays the caller's location for assertions in 'checkSessionResult', while
-- leaving assertions in @prog@ unchanged.
testSession ::
  HasCallStack =>
  String ->
  TestSessionConfig a ->
  TestT IO a
testSession desc conf = do
  state <- maybe (liftIO newState) pure conf.state
  result <- testSessionMain state conf.args conf.program
  withFrozenCallStack do
    checkSessionResult desc conf.checkLog result
