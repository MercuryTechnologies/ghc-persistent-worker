-- | Generic concurrent scheduler with inbox-based architecture.
--
-- External clients submit work via 'submitRequest'.  The scheduler loop
-- classifies requests into tasks (via 'classify'), respects
-- inter-task dependency ordering, and dispatches ready tasks up to a
-- configurable concurrency limit.
--
-- Tasks exist in two pools:
--
-- * /Active/ (unsatisfied or ready): participate in dependency
--   tracking and dispatch.
-- * /Pending/: invisible to 'awaitIdle'.  They are activated via 'addResolutions'
--   enabled pending tasks that now have resolution entries) or immediately
--   at insertion time if a resolution already exists.
--
-- Resolution entries (mapping pending keys to resolved keys, values, and
-- dependency sets) are stored in 'SchedulerState' and populated by the
-- build layer through the 'propagate' callback via 'addResolutions'.
--
-- On task completion, the 'propagate' callback lets the build layer apply
-- domain-specific effects (e.g. computing resolution maps from metadata
-- results and promoting compile tasks).
--
-- The key type parameter @key :: 'Phase' -> 'Type'@ is phase-indexed:
-- @key \''Pending@ identifies tasks in the pending pool (e.g. by source path),
-- while @key \''Resolved@ identifies active\/completed tasks (e.g. by module name).
-- Dependencies are always expressed in terms of @key \''Resolved@ in the
-- active pools.  Resolution entries carry dependencies as @key \''Pending@,
-- which are converted to @key \''Resolved@ during promotion.
-- The task value parameter @task@ is a plain type shared across both pools.
module GhcServer.Scheduler where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (
  STM,
  TQueue,
  TVar,
  atomically,
  check,
  isEmptyTQueue,
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVar,
  readTVarIO,
  stateTVar,
  writeTQueue,
  writeTVar,
  )
import Control.Concurrent.STM.TQueue (peekTQueue)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forever, void)
import Data.Foldable (foldr', traverse_)
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import System.Timeout (timeout)

-- | Phase of a task in the scheduler lifecycle.
--
-- * @'Pending@: task is in the pending pool, awaiting resolution.
-- * @'Resolved@: task has been resolved and is eligible for dispatch.
data Phase = Pending | Resolved

-- | Constraint alias requiring 'Ord' for both phases of a key.
type OrdKey :: (Phase -> Type) -> Constraint
type OrdKey key = (Ord (key 'Pending), Ord (key 'Resolved))

-- | Result of executing a build task.
data TaskResult =
  TaskSuccess
  |
  TaskFailed String
  deriving stock (Eq, Show)

-- | A build task with a key, dependencies, and a dispatchable value.
--
-- The key and value are indexed by the same phase @p@, but dependencies are
-- always expressed in @key \''Resolved@ — pending tasks depend on resolved
-- (active) tasks, never on other pending tasks.
data Task (key :: Phase -> Type) (p :: Phase) a =
  Task {
    key :: key p,
    -- | Dependencies, always expressed as resolved keys.
    deps :: Set (key 'Resolved),
    -- | Whether the task is eligible for promotion from the pending pool.
    -- 'insertPending' merges this flag with OR when a duplicate key is inserted.
    -- 'promoteEnabled' only promotes tasks where this is 'True'.
    -- For active (non-pending) tasks, this field is ignored.
    enabled :: Bool,
    value :: a
  }

deriving stock instance (Show (key p), Show (key 'Resolved), Show a) => Show (Task key p a)
deriving stock instance (Eq (key p), Eq (key 'Resolved), Eq a) => Eq (Task key p a)

-- | Events processed by the scheduler loop.
data SchedulerEvent request (key :: Phase -> Type) =
  -- | An external request to be classified into tasks.
  RequestEvent request
  |
  -- | A task completed with its result.
  CompletionEvent (key 'Resolved) TaskResult

-- | Immutable configuration for the scheduler.
data SchedulerEnv request (key :: Phase -> Type) task ext =
  SchedulerEnv {
    maxJobs :: Int,
    -- | Domain-specific handler callbacks.
    handlers :: Handlers request key task ext,
    -- | Timeout for each task in seconds.
    taskTimeout :: Int
  }

-- | Domain-specific handler callbacks for the scheduler.
--
-- These callbacks bridge the generic scheduler with the build system:
-- dispatching tasks to workers, classifying requests into tasks, and
-- applying domain-specific effects on task completion.
data Handlers request (key :: Phase -> Type) task ext =
  Handlers {
    -- | Dispatch a resolved task to a worker.
    -- Receives the scheduler's domain-specific extension state and the full 'Task'
    -- so the handler can inspect metadata like 'enabled'.
    dispatch :: ext -> Task key 'Resolved task -> IO TaskResult,
    -- | Convert a request into active and pending task lists.
    classify :: request -> IO ([Task key 'Resolved task], [Task key 'Pending task]),
    -- | Apply domain-specific effects after a task completes.
    --
    -- Runs in the scheduler loop thread, so IO is safe but should not block
    -- for extended periods.
    propagate ::
      key 'Resolved ->
      TaskResult ->
      SchedulerState key task ext ->
      IO (SchedulerState key task ext)
  }

-- | All mutable scheduler state.
--
-- The @ext@ parameter allows the build layer to store domain-specific data
-- (such as accumulated resolution maps) alongside the scheduler's own state.
--
-- The @key@ parameter is phase-indexed: @key \''Pending@ for the pending pool,
-- @key \''Resolved@ for active\/completed tasks.  Dependencies are always
-- expressed in @key \''Resolved@.
data SchedulerState (key :: Phase -> Type) task ext =
  SchedulerState {
    -- | Tasks waiting on unmet dependencies.
    unsatisfied :: Map (key 'Resolved) (Task key 'Resolved task, Set (key 'Resolved)),
    -- | Tasks ready for dispatch.
    ready :: [Task key 'Resolved task],
    -- | Pre-resolution tasks awaiting promotion. Excluded from 'awaitIdle'.
    pending :: Map (key 'Pending) (Task key 'Pending task),
    completed :: Set (key 'Resolved),
    -- | All resolved keys that have been activated (i.e. moved to unsatisfied\/ready).
    -- Used for idempotent enqueue.
    accepted :: Set (key 'Resolved),
    activeCount :: Int,
    failures :: Map (key 'Resolved) String,
    -- | Resolution map: converts pending keys to resolved keys, values, and
    -- pending dep sets.  Populated by the build layer via 'addResolutions'
    -- after metadata completes.
    resolutions :: Map (key 'Pending) (key 'Resolved, task, Set (key 'Pending)),
    -- | Domain-specific state threaded through 'propagate'.
    ext :: ext
  }

-- | Mutable scheduler state, shared across worker threads and external callers.
data SchedulerResources request (key :: Phase -> Type) task ext =
  SchedulerResources {
    -- | Event queue for inbox requests and task completions.
    events :: TQueue (SchedulerEvent request key),
    state :: TVar (SchedulerState key task ext)
  }

-- | Move all unsatisfied tasks with empty dep sets to the ready list.
promoteReady ::
  SchedulerState key task ext ->
  SchedulerState key task ext
promoteReady state =
  state {unsatisfied, ready = state.ready ++ (fst <$> Map.elems readyNow)}
  where
    (readyNow, unsatisfied) = Map.partition (null . snd) state.unsatisfied

-- | Classify a single active task: skip if already enqueued, otherwise insert into 'unsatisfied'
-- and promote if ready.
classifyTask ::
  Ord (key 'Resolved) =>
  Task key 'Resolved task ->
  SchedulerState key task ext ->
  SchedulerState key task ext
classifyTask task state =
  if Set.member task.key state.accepted
  then state
  else promoteReady state {
    unsatisfied = Map.insert task.key (task, unmet) state.unsatisfied,
    accepted = Set.insert task.key state.accepted
  }
  where
    unmet = Set.difference task.deps state.completed

-- | Record a task result: update completed set, decrement active count,
-- remove key from dep sets, promote newly ready tasks.
recordResult ::
  Ord (key 'Resolved) =>
  key 'Resolved ->
  TaskResult ->
  SchedulerState key task ext ->
  SchedulerState key task ext
recordResult key result =
  promoteReady . record
  where
    record state =
      state {
        completed = Set.insert key state.completed,
        failures = case result of
          TaskSuccess -> state.failures
          TaskFailed msg -> Map.insert key msg state.failures,
        unsatisfied = Map.map (fmap (Set.delete key)) state.unsatisfied,
        activeCount = state.activeCount - 1
      }

-- | Insert a task into the pending pool, or resolve it immediately.
--
-- * If the resolution map already contains an entry for this key and the task
--   is enabled, resolve immediately into unsatisfied (via 'resolveTask').
-- * If already pending, merge the @enabled@ flag with OR.
-- * Otherwise, insert as a new pending task.
insertPending ::
  OrdKey key =>
  Task key 'Pending task ->
  SchedulerState key task ext ->
  SchedulerState key task ext
insertPending task state
  | task.enabled
  , Map.member task.key state.resolutions =
    let inserted = state {pending = Map.insertWith mergeTask task.key task state.pending}
    in case resolveTask task.key inserted of
      Nothing -> inserted
      Just (s', pendingDeps) -> promoteReady (go pendingDeps s')
  | otherwise = state {pending = Map.insertWith mergeTask task.key task state.pending}
  where
    mergeTask new old = old {enabled = old.enabled || new.enabled}
    go [] s = s
    go (k : ks) s =
      case resolveTask k s of
        Nothing -> go ks s
        Just (s', more) -> go (more ++ ks) s'

-- | Promote keys from the pending pool to unsatisfied, transitively through dependencies.
--
-- For each pending key, if the pending pool contains the task and 'resolutions' provides
-- a resolved key, value, and pending dependency set, the task is moved to unsatisfied with
-- the resolved identity.  Pending deps are converted to resolved keys via 'resolutions'
-- and placed in the unsatisfied dep set.  Any pending deps that are still in the pending pool
-- are added to the work list, ensuring transitive activation.
promote ::
  OrdKey key =>
  Set (key 'Pending) ->
  SchedulerState key task ext ->
  SchedulerState key task ext
promote keys =
  promoteReady . go (Set.toList keys)
  where
    go [] s = s
    go (k : ks) s =
      case resolveTask k s of
        Nothing -> go ks s
        Just (s', pendingDeps) -> go (pendingDeps ++ ks) s'

-- | Try to resolve a single pending task using 'resolutions' from state.
--
-- Returns 'Nothing' if the key is not pending or has no resolution entry.
-- On success, returns the updated state and a list of the task's own
-- dependencies that are still in the pending pool (for transitive promotion).
resolveTask ::
  OrdKey key =>
  key 'Pending ->
  SchedulerState key task ext ->
  Maybe (SchedulerState key task ext, [key 'Pending])
resolveTask k s = do
  task <- Map.lookup k s.pending
  (resolvedKey, resolved, pendingDeps) <- Map.lookup k s.resolutions
  let
    -- Convert pending deps to resolved keys via 'resolutions'.
    -- Deps whose resolution is not yet available are silently dropped;
    -- in practice this doesn't happen because metadata completes in
    -- dependency order.
    resolvedDeps = Set.fromList
      [rk | pk <- Set.toList pendingDeps, Just (rk, _, _) <- [Map.lookup pk s.resolutions]]
    allDeps = Set.union task.deps resolvedDeps
    resolvedTask = Task {key = resolvedKey, deps = allDeps, enabled = task.enabled, value = resolved}
    s' = s {
      pending = Map.delete k s.pending,
      unsatisfied = Map.insert resolvedKey (resolvedTask, Set.difference allDeps s.completed) s.unsatisfied,
      accepted = Set.insert resolvedKey s.accepted
    }
  pure (s', [pk | pk <- Set.toList pendingDeps, Map.member pk s'.pending])

-- | Promote all pending tasks that have @enabled = True@ and have an entry
-- in 'resolutions'.
--
-- Only tasks where @enabled = True@ and a matching resolution exists are promoted.
-- Promotion is transitive through dependencies, so tasks that are depended upon
-- by promoted ones also get promoted (if they have resolutions).
promoteEnabled ::
  OrdKey key =>
  SchedulerState key task ext ->
  SchedulerState key task ext
promoteEnabled state =
  promote enabledKeys state
  where
    enabledKeys = Set.fromList
      [
        k
        | (k, task) <- Map.toList state.pending
        , task.enabled
        , Map.member k state.resolutions
      ]

-- | Merge new resolution entries into state and promote eligible pending tasks.
--
-- This is the primary interface for the build layer to supply resolution data
-- after metadata completes.  After merging, all enabled pending tasks that
-- now have resolutions are promoted (transitively through deps).
addResolutions ::
  OrdKey key =>
  Map (key 'Pending) (key 'Resolved, task, Set (key 'Pending)) ->
  SchedulerState key task ext ->
  SchedulerState key task ext
addResolutions newResolutions state =
  promoteEnabled state {resolutions = Map.union newResolutions state.resolutions}

-- | Execute the task's dispatch function with timeout and exception handling.
runTask ::
  SchedulerEnv request key task ext ->
  ext ->
  Task key 'Resolved task ->
  IO TaskResult
runTask env ext task =
  try (timeout (env.taskTimeout * 1_000_000) (env.handlers.dispatch ext task)) >>= \case
    Right (Just r) -> pure r
    Right Nothing -> pure (TaskFailed ("Task timed out after " ++ show env.taskTimeout ++ "s"))
    Left (exc :: SomeException) ->
      case fromException exc of
        Just (e :: SomeAsyncException) -> throwIO e
        Nothing -> pure (TaskFailed (show exc))

-- | Run a task and signal completion via the event queue.
executeTask ::
  SchedulerEnv request key task ext ->
  SchedulerResources request key task ext ->
  ext ->
  Task key 'Resolved task ->
  IO ()
executeTask env resources ext task = do
  result <- runTask env ext task
  atomically (writeTQueue resources.events (CompletionEvent task.key result))

-- | Fork a task into an async worker.
--
-- The caller is responsible for incrementing 'activeCount' before calling
-- this function (see 'fillSlots').
startTask ::
  SchedulerEnv request key task ext ->
  SchedulerResources request key task ext ->
  ext ->
  Task key 'Resolved task ->
  IO ()
startTask env resources ext task =
  void (async (executeTask env resources ext task))

-- | Enqueue pre-built active tasks directly. Skips tasks whose keys are already known.
--
-- This is intended for use by dispatch callbacks that generate follow-up tasks
-- (e.g. compile tasks after metadata completion). For external callers, prefer
-- 'submitRequest'.
enqueueTasks ::
  Ord (key 'Resolved) =>
  [Task key 'Resolved task] ->
  SchedulerState key task ext ->
  SchedulerState key task ext
enqueueTasks =
  flip (foldr' classifyTask)

-- | Insert tasks into the pending pool, merging the @enabled@ flag for duplicate keys.
enqueuePending ::
  OrdKey key =>
  [Task key 'Pending task] ->
  SchedulerState key task ext ->
  SchedulerState key task ext
enqueuePending =
  flip (foldr' insertPending)

-- | Handle a single event: classify inbox tasks or record completions.
processEvent ::
  OrdKey key =>
  SchedulerEnv request key task ext ->
  SchedulerResources request key task ext ->
  SchedulerEvent request key ->
  IO ()
processEvent env resources = \case
  RequestEvent req -> do
    (activeTasks, pendingTasks) <- env.handlers.classify req
    atomically do
      modifyTVar' resources.state (enqueuePending pendingTasks . enqueueTasks activeTasks)
  CompletionEvent key result -> do
    propagated <- env.handlers.propagate key result =<< readTVarIO resources.state
    atomically do
      writeTVar resources.state (recordResult key result propagated)

-- | Take ready tasks from the pool up to the job limit and start them.
--
-- Atomically moves tasks from ready to active (incrementing 'activeCount')
-- so that 'awaitIdle' cannot observe a transient state where ready is
-- empty but 'activeCount' has not yet been bumped.
fillSlots ::
  SchedulerEnv request key task ext ->
  SchedulerResources request key task ext ->
  IO ()
fillSlots env resources =
  traverse_ (uncurry (startTask env resources)) =<< atomically (stateTVar resources.state takeReady)
  where
    takeReady state =
      let
        available = env.maxJobs - state.activeCount
        (toDispatch, keep) = splitAt available state.ready
      in (map (state.ext,) toDispatch, state {ready = keep, activeCount = state.activeCount + length toDispatch})

-- | Main scheduler loop. Reads one event at a time, processes it, dispatches ready tasks,
-- repeats. Runs indefinitely, blocking on the event queue when idle.
--
-- Leaves the processed task in the queue while processing to avoid race conditions.
schedulerLoop ::
  OrdKey key =>
  SchedulerEnv request key task ext ->
  SchedulerResources request key task ext ->
  IO Void
schedulerLoop env resources =
  forever do
    fillSlots env resources
    event <- atomically do
      peekTQueue resources.events
    processEvent env resources event
    atomically do
      void $ readTQueue resources.events

-- API -------------------------------------------------------------------

-- | Start the scheduler loop in a background thread.
--
-- The loop runs indefinitely, blocking on the event queue when idle.
runScheduler ::
  OrdKey key =>
  SchedulerEnv request key task ext ->
  SchedulerResources request key task ext ->
  IO (Async Void)
runScheduler env resources =
  async (schedulerLoop env resources)

-- | Create a fresh scheduler state.
--
-- Use this when the 'SchedulerEnv' callbacks need access to the scheduler state
-- (e.g. dispatch callbacks that submit follow-up requests to the inbox).
-- After creating the env with references to this state, call 'runScheduler'.
newSchedulerState :: ext -> IO (SchedulerResources request key task ext)
newSchedulerState initialExt = do
  events <- newTQueueIO
  state <- newTVarIO SchedulerState {
    unsatisfied = Map.empty,
    ready = [],
    pending = Map.empty,
    completed = Set.empty,
    accepted = Set.empty,
    activeCount = 0,
    failures = Map.empty,
    resolutions = Map.empty,
    ext = initialExt
  }
  pure SchedulerResources {events, state}

-- | Submit a request to the scheduler's event queue.
submitRequest :: SchedulerResources request key task ext -> request -> IO ()
submitRequest resources request =
  atomically (writeTQueue resources.events (RequestEvent request))

-- | Block until the scheduler is idle: no active tasks, no ready tasks, no unsatisfied tasks,
-- and no pending events in the queue.
--
-- Pending tasks are excluded — they are not considered active work.
--
-- This is the primary termination criterion for tests and single-shot builds.
awaitIdle :: SchedulerResources request key task ext -> STM ()
awaitIdle resources = do
  empty <- isEmptyTQueue resources.events
  check empty
  state <- readTVar resources.state
  check (state.activeCount == 0 && null state.ready && Map.null state.unsatisfied)

