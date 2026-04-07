-- | Unit tests for 'GhcServer.Scheduler' pure functions.
module Test.SchedulerTest where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GhcServer.Scheduler (
  Phase (..),
  SchedulerState (..),
  Task (..),
  addResolutions,
  insertPending,
  promote,
  promoteEnabled,
  )
import Hedgehog (TestT, property, test, withTests, (===))
import Test.Tasty (DependencyType (..), TestName, TestTree, dependentTestGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ---------------------------------------------------------------------------
-- Test key and task types using Int-based keys
-- ---------------------------------------------------------------------------

-- | Phase-indexed key for testing.
-- Both phases use 'Int', which mirrors the case where pending and resolved
-- keys have the same underlying type.
data TestKey (p :: Phase) where
  TK :: Int -> TestKey p

deriving stock instance Show (TestKey p)
deriving stock instance Eq (TestKey p)
deriving stock instance Ord (TestKey p)

-- | Simple task value type for testing.
data TestTask =
  PendingVal Int
  |
  ResolvedVal Int
  deriving stock (Show, Eq)

type Key = Int
type State = SchedulerState TestKey TestTask ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

unitTest :: TestName -> TestT IO () -> TestTree
unitTest desc t =
  testProperty desc (withTests 1 (property (test t)))

emptyState :: State
emptyState =
  SchedulerState {
    unsatisfied = Map.empty,
    ready = [],
    pending = Map.empty,
    completed = Set.empty,
    accepted = Set.empty,
    activeCount = 0,
    failures = Map.empty,
    resolutions = Map.empty,
    ext = ()
  }

pendingTask :: Key -> Set Key -> Bool -> Int -> Task TestKey 'Pending TestTask
pendingTask k deps isEnabled val =
  Task {key = TK k, deps = Set.map TK deps, enabled = isEnabled, value = PendingVal val}

pendingKeys :: State -> Set Key
pendingKeys = Set.map (\(TK k) -> k) . Map.keysSet . (.pending)

unsatisfiedKeys :: State -> Set Key
unsatisfiedKeys = Set.map (\(TK k) -> k) . Map.keysSet . (.unsatisfied)

readyKeys :: State -> Set Key
readyKeys = Set.fromList . map (\(TK k) -> k) . map (.key) . (.ready)

-- | Build resolution map from raw (key, (resolvedValue, pendingDeps)) entries.
mkResolutions :: [(Key, (TestTask, Set Key))] -> Map.Map (TestKey 'Pending) (TestKey 'Resolved, TestTask, Set (TestKey 'Pending))
mkResolutions =
  Map.fromList . map \(k, (v, deps)) -> (TK k, (TK k, v, Set.map TK deps))


-- ---------------------------------------------------------------------------
-- Promote test spec
-- ---------------------------------------------------------------------------

-- | Specification for a @promote@ or @promoteEnabled@ test.
data PromoteSpec =
  PromoteSpec {
    pending_ :: [(Key, Set Key, Bool, Int)]
    ,
    completed_ :: Set Key
    ,
    accepted_ :: Set Key
    ,
    resolutions_ :: [(Key, (TestTask, Set Key))]
    ,
    -- | Keys to promote (used only by @promote@, ignored by @promoteEnabled@).
    promoteKeys :: Set Key
    ,
    expectPending :: Set Key
    ,
    expectUnsatisfied :: Set Key
    ,
    expectReady :: Set Key
  }

defaultPromoteSpec :: PromoteSpec
defaultPromoteSpec =
  PromoteSpec {
    pending_ = [],
    completed_ = Set.empty,
    accepted_ = Set.empty,
    resolutions_ = [],
    promoteKeys = Set.empty,
    expectPending = Set.empty,
    expectUnsatisfied = Set.empty,
    expectReady = Set.empty
  }

specState :: PromoteSpec -> State
specState spec =
  emptyState {
    pending = Map.fromList [(TK k, pendingTask k deps en val) | (k, deps, en, val) <- spec.pending_],
    completed = Set.map TK spec.completed_,
    accepted = Set.map TK spec.accepted_,
    resolutions = mkResolutions spec.resolutions_
  }

runPromote :: PromoteSpec -> TestT IO ()
runPromote spec = do
  let result = promote (Set.map TK spec.promoteKeys) (specState spec)
  spec.expectPending === pendingKeys result
  spec.expectUnsatisfied === unsatisfiedKeys result
  spec.expectReady === readyKeys result

runPromoteEnabled :: PromoteSpec -> TestT IO ()
runPromoteEnabled spec = do
  let result = promoteEnabled (specState spec)
  spec.expectPending === pendingKeys result
  spec.expectUnsatisfied === unsatisfiedKeys result
  spec.expectReady === readyKeys result

-- ---------------------------------------------------------------------------
-- Tests for 'promote'
-- ---------------------------------------------------------------------------

test_promoteSingleNoDeps :: TestTree
test_promoteSingleNoDeps =
  unitTest "single task with no deps becomes ready" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.empty, False, 10)],
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1,
      expectReady = Set.singleton 1
    }

test_promoteWithUnmetDep :: TestTree
test_promoteWithUnmetDep =
  unitTest "task with unmet dep goes to unsatisfied" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.singleton 99, False, 10)],
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1,
      expectUnsatisfied = Set.singleton 1
    }

test_promoteDepAlreadyCompleted :: TestTree
test_promoteDepAlreadyCompleted =
  unitTest "task whose dep is completed becomes ready" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.singleton 2, False, 10)],
      completed_ = Set.singleton 2,
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1,
      expectReady = Set.singleton 1
    }

test_promoteExtraDeps :: TestTree
test_promoteExtraDeps =
  unitTest "extra deps from resolution map are added" do
    -- Task 1 has an existing dep on resolved key 99 (from task.deps)
    -- and an extra pending dep on key 2 (from the resolution map).
    -- Both deps contribute to the unsatisfied set.
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.singleton 99, False, 10), (2, Set.empty, False, 20)],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.singleton 2))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      promoteKeys = Set.singleton 1,
      expectUnsatisfied = Set.singleton 1,
      expectReady = Set.singleton 2
    }

test_promoteTransitive :: TestTree
test_promoteTransitive =
  unitTest "transitive promotion through pending deps" do
    runPromote defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, False, 10)
        , (2, Set.empty, False, 20)
        ],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.singleton 2))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      promoteKeys = Set.singleton 1,
      expectReady = Set.singleton 2,
      expectUnsatisfied = Set.singleton 1
    }

test_promoteNotInPending :: TestTree
test_promoteNotInPending =
  unitTest "promoting key not in pending is a no-op" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.empty, False, 10)],
      resolutions_ = [(99, (ResolvedVal 99, Set.empty))],
      promoteKeys = Set.singleton 99,
      expectPending = Set.singleton 1
    }

test_promoteNoResolution :: TestTree
test_promoteNoResolution =
  unitTest "pending task without resolution stays pending" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.empty, False, 10)],
      promoteKeys = Set.singleton 1,
      expectPending = Set.singleton 1
    }

test_promoteAlreadyAccepted :: TestTree
test_promoteAlreadyAccepted =
  unitTest "already-accepted key is not in pending, skipped" do
    runPromote defaultPromoteSpec {
      accepted_ = Set.singleton 1,
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1
    }

test_promoteUpdatesAccepted :: TestTree
test_promoteUpdatesAccepted =
  unitTest "promoted tasks are added to accepted set" do
    let
      spec = defaultPromoteSpec {
        pending_ = [(1, Set.empty, False, 10)],
        resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
        promoteKeys = Set.singleton 1
      }
      result = promote (Set.map TK spec.promoteKeys) (specState spec)
    Set.member (TK 1) result.accepted === True

-- ---------------------------------------------------------------------------
-- Tests for 'promoteEnabled'
-- ---------------------------------------------------------------------------

test_promoteEnabledSkipsDisabled :: TestTree
test_promoteEnabledSkipsDisabled =
  unitTest "promoteEnabled skips disabled tasks" do
    runPromoteEnabled defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, True, 10)
        , (2, Set.empty, False, 20)
        ],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.empty))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      expectReady = Set.singleton 1,
      expectPending = Set.singleton 2
    }

test_promoteEnabledNoResolution :: TestTree
test_promoteEnabledNoResolution =
  unitTest "promoteEnabled skips enabled tasks without resolution" do
    runPromoteEnabled defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, True, 10)
        , (2, Set.empty, True, 20)
        ],
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      expectReady = Set.singleton 1,
      expectPending = Set.singleton 2
    }

test_promoteEnabledTransitive :: TestTree
test_promoteEnabledTransitive =
  unitTest "promoteEnabled transitively promotes disabled deps" do
    runPromoteEnabled defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, True, 10)
        , (2, Set.empty, False, 20)
        ],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.singleton 2))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      expectReady = Set.singleton 2,
      expectUnsatisfied = Set.singleton 1
    }

-- ---------------------------------------------------------------------------
-- Tests for 'insertPending'
-- ---------------------------------------------------------------------------

test_insertPendingMergesEnabled :: TestTree
test_insertPendingMergesEnabled =
  unitTest "insertPending merges enabled with OR" do
    let
      state = emptyState {
        pending = Map.singleton (TK 1) (pendingTask 1 Set.empty False 10)
      }
      result = insertPending (pendingTask 1 Set.empty True 10) state
    case Map.lookup (TK 1) result.pending of
      Just t -> t.enabled === True
      Nothing -> fail "task should be in pending"

test_insertPendingResolvesImmediately :: TestTree
test_insertPendingResolvesImmediately =
  unitTest "insertPending resolves immediately when resolution exists and enabled" do
    let
      state = emptyState {
        resolutions = mkResolutions [(1, (ResolvedVal 10, Set.empty))]
      }
      result = insertPending (pendingTask 1 Set.empty True 10) state
    Map.null result.pending === True
    readyKeys result === Set.singleton 1

-- ---------------------------------------------------------------------------
-- Tests for 'addResolutions'
-- ---------------------------------------------------------------------------

test_addResolutionsPromotesPending :: TestTree
test_addResolutionsPromotesPending =
  unitTest "addResolutions promotes enabled pending tasks" do
    let
      state = emptyState {
        pending = Map.fromList
          [ (TK 1, pendingTask 1 Set.empty True 10)
          , (TK 2, pendingTask 2 Set.empty False 20)
          ]
      }
      result = addResolutions (mkResolutions [(1, (ResolvedVal 10, Set.empty)), (2, (ResolvedVal 20, Set.empty))]) state
    pendingKeys result === Set.singleton 2
    readyKeys result === Set.singleton 1

-- ---------------------------------------------------------------------------
-- Test tree
-- ---------------------------------------------------------------------------

test_scheduler :: TestTree
test_scheduler =
  dependentTestGroup "GhcServer.Scheduler" AllFinish
    [ dependentTestGroup "promote" AllFinish
        [ test_promoteSingleNoDeps
        , test_promoteWithUnmetDep
        , test_promoteDepAlreadyCompleted
        , test_promoteExtraDeps
        , test_promoteTransitive
        , test_promoteNotInPending
        , test_promoteNoResolution
        , test_promoteAlreadyAccepted
        , test_promoteUpdatesAccepted
        ]
    , dependentTestGroup "promoteEnabled" AllFinish
        [ test_promoteEnabledSkipsDisabled
        , test_promoteEnabledNoResolution
        , test_promoteEnabledTransitive
        ]
    , dependentTestGroup "addResolutions" AllFinish
        [ test_addResolutionsPromotesPending
        ]
    , dependentTestGroup "insertPending" AllFinish
        [ test_insertPendingMergesEnabled
        , test_insertPendingResolvesImmediately
        ]
    ]
