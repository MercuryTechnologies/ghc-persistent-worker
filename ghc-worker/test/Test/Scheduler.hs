module Test.Scheduler where

import Control.Concurrent.Async (Async, async, waitAnyCatch, waitCatch)
import Control.Exception (Exception, SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Lens (at, contains, use, uses, (%=), (.=))
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State.Strict (StateT, execStateT)
import Data.Coerce (coerce)
import Data.Foldable (toList, traverse_)
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Set (Set, isSubsetOf)
import System.Timeout (timeout)
import Test.Data.Env (MaxJobs (..))
import Test.Data.Scheduler (
  Capacity (..),
  Task (..),
  RequestFailure (..),
  RequestOutput (..),
  RequestResult (..),
  Schedule (..),
  SchedulerEnv (..),
  SchedulerState (..),
  Status (..),
  )

type Scheduler key task = ReaderT (SchedulerEnv key task) (StateT (SchedulerState key task) IO)

-- | Safely wait for the request to be processed by the dispatch function and analyze its result.
-- If the task takes longer than a second, consider it failed.
--
-- GHC throws exceptions liberally and we want to be able to associate a panic with the request that caused it, so we
-- catch anything.
-- @AsyncException@ is always critical, of course.
executeRequest :: (task -> IO RequestResult) -> Task key task -> IO (RequestOutput key)
executeRequest dispatch Task {key, value = request} = do
  result <- try (timeout 10_000_000 (dispatch request)) >>= \case
    Right (Just result) ->
      pure result
    Right Nothing ->
      pure (RequestFailure (RequestFatal "Request took longer than 1s"))
    Left (exc :: SomeException) ->
      case fromException exc of
        Just (e :: SomeAsyncException) ->
          throwIO e
        Nothing ->
          pure (RequestFailure (RequestFatal (show exc)))
  pure RequestOutput {key, result}

initScheduler ::
  Ord key =>
  MaxJobs ->
  (task -> IO RequestResult) ->
  Schedule key task ->
  Set key ->
  (SchedulerEnv key task, SchedulerState key task)
initScheduler maxJobs dispatch tasks completed =
  (env, state)
  where
    env = SchedulerEnv {maxJobs, dispatch}

    state = SchedulerState {
      schedule = tasks,
      completed,
      activeRequests = [],
      failures = []
    }

outputFailure :: RequestResult -> Maybe RequestFailure
outputFailure = \case
  RequestSuccess -> Nothing
  RequestFailure failure -> Just failure

-- | Indicate whether the dependencies of the given task are all completed, i.e. a subset of the contents of the
-- scheduler state's 'completed' field.
taskReady :: Ord key => Task key task -> Scheduler key task Bool
taskReady task =
  uses #completed (isSubsetOf task.deps)

-- | Pops the head of the schedule if its deps are satisfied.
-- Returns 'Blocked' without consuming the task when deps are pending, allowing it to be retried after more results
-- arrive.
dequeue :: Ord key => Scheduler key task (Status key task)
dequeue =
  use #schedule >>= \case
    Schedule [] -> pure Exhausted
    Schedule (task : rest) ->
      taskReady task >>= \case
        True -> Ready task <$ (#schedule .= Schedule rest)
        False -> pure (Blocked task)

-- | Execute a request in a new thread, tracking its handle in the state.
-- Decides whether the loop should continue with the next task based on the concurrency limit configured by the test
-- harness.
startRequest :: Task key task -> Scheduler key task Capacity
startRequest task = do
  SchedulerEnv {dispatch, maxJobs} <- ask
  handle <- liftIO $ async $ executeRequest dispatch task
  #activeRequests . contains handle .= True
  uses #activeRequests \ reqs ->
    if length reqs >= coerce maxJobs then Full else Available

-- | Write a task's result to the state, remove it from the active set and add it to the completed set in the state.
storeResult :: Ord key => Async (RequestOutput key) -> RequestOutput key -> Scheduler key task ()
storeResult handle RequestOutput {key, result} = do
  #activeRequests . contains handle .= False
  #completed . contains key .= True
  #failures . at key .= outputFailure result

handleResult ::
  Ord key =>
  Exception e =>
  Async (RequestOutput key) ->
  Either e (RequestOutput key) ->
  Scheduler key task ()
handleResult handle =
  either (liftIO . throwIO) (storeResult handle)

-- | Block on the set of active 'Async' handles until one completes.
awaitOneRequest :: Ord key => Scheduler key task ()
awaitOneRequest = do
  active <- use #activeRequests
  (handle, result) <- liftIO $ waitAnyCatch (toList active)
  handleResult handle result

-- | Block on each active 'Async' handle until they've all completed.
awaitAllRequests :: Ord key => Scheduler key task ()
awaitAllRequests = do
  use #activeRequests >>= traverse_ \ handle ->
    handleResult handle =<< liftIO (waitCatch handle)

-- | Indicate whether the current build should continue, which is when no task has failed so far.
buildSuccess :: Scheduler key task Bool
buildSuccess =
  uses #failures null

-- | Wait for all active requests, not adding their results to the state.
terminateBuild :: Scheduler key task ()
terminateBuild =
  traverse_ (liftIO . waitCatch) =<< use #activeRequests

-- | Wait for one result and abort if a failure occurred.
waitForRequestSlot :: Ord key => Scheduler key task ()
waitForRequestSlot = do
  awaitOneRequest
  ifM buildSuccess loopSchedule terminateBuild

-- | Indicate whether no requests are currently being processed, which is treated as a fatal deadlock if the next task
-- has unsatisfied dependencies.
noActiveRequests :: Scheduler key task Bool
noActiveRequests =
  uses #activeRequests null

recordDeadlock :: Ord key => Task key task -> Scheduler key task ()
recordDeadlock task =
  #failures %= Map.insert task.key (RequestFatal "Deadlock: no active requests and next task blocked")

-- | Repeatedly pull the next task from the schedule, wait for its dependencies to complete processing, and start
-- processing it.
--
-- Record a deadlock failure and return if blocked on dependencies with no active requests.
--
-- If the schedule is empty, wait for the remaining active tasks to complete.
loopSchedule :: Ord key => Scheduler key task ()
loopSchedule =
  dequeue >>= \case
    Ready task ->
      startRequest task >>= \case
        Available -> loopSchedule
        Full -> waitForRequestSlot
    Blocked task ->
      ifM noActiveRequests (recordDeadlock task) waitForRequestSlot
    Exhausted ->
      awaitAllRequests

runScheduler ::
  Ord key =>
  SchedulerEnv key task ->
  SchedulerState key task ->
  IO (SchedulerState key task)
runScheduler env =
  execStateT (runReaderT loopSchedule env)
