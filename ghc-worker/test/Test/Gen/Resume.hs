module Test.Gen.Resume where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified GHC.Data.Graph.Directed as Graph
import GHC.Data.Graph.Directed (reachablesG, transposeG)
import qualified GHC.Data.Graph.Directed as G (Node (..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Test.Data.Project (
  Component (..),
  GenModule (..),
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  TaskKey (..),
  )
import Test.Data.ProjectBuild (RebuildSet (..), ResumePlan (..))
import Test.Data.Scheduler (Task (..))

type DepGraph = Graph.Graph (Graph.Node TaskKey (Task TaskKey Component))

-- | Compute the set of build tasks that need to be executed during a resume build.
-- They consist of the modified modules and all modules that depend on them.
--
-- This uses a reachability query on the transposition of the graph computed in "Test.Gen.ProjectBuild" to obtain the
-- set of dependents.
--
-- Note that this is the graph that already includes the resume dependencies.
-- I think this would only be relevant if we also removed dependencies for the resume build, because the modules with
-- added deps are already in the mutation set.
computeRebuildSet :: DepGraph -> Set ModuleKey -> RebuildSet
computeRebuildSet g mutations =
  RebuildSet {
    moduleKeys,
    allAffectedKeys = rebuildKeys <> affectedMetaKeys,
    hasChanges = not (null allChangedKeys)
  }
  where
    rebuildKeys = Set.map TaskCompile moduleKeys

    affectedMetaKeys = Set.map (TaskMeta . (.unit)) moduleKeys

    moduleKeys = Set.fromList [key | G.DigraphNode {G.node_key = TaskCompile key} <- closure]

    closure = reachablesG (transposeG g) roots

    roots =
      [
        G.DigraphNode {
          node_payload = Task {key, deps = [], value = dummy},
          node_key = key,
          node_dependencies = []
        }
        | key <- Set.toList allChangedKeys
      ]

    allChangedKeys = Set.map TaskCompile mutations

    -- We're not interested in any payloads, just the new schedule, but the graph doesn't allow us to use only keys
    -- for the query.
    dummy = ComponentUnit GenUnit {key = 0, depUnits = Set.empty, modules = []}

-- | Generate the data required for the resume build.
--
-- There are three kinds of mutations:
--
-- - @depMutations@ change imports in source files and therefore affect the dependency graph.
--   They add the resume deps drawn from the pool of eligible modules in the generator for 'GenModule'.
--
-- - @moduleMutations@ make trivial changes to an expression that only requires the module to be rebuilt.
--   They can only affect modules without errors, because error modules are rebuilt irrespective of changes.
--
-- Mutations can affect the dependency graph when imports are changed (@depMutations@) or only require the module to
-- be recompiled (@moduleMutations@).
--
-- TODO pretty sure I forgot to add the modules affected by @fixErrors@ to the rebuild set, unless they're already in
-- @moduleMutations@ because we know that errors _always_ have to be rebuilt, or are added otherwise.
-- Investigate.
genResumePlan ::
  InitialProject ->
  [GenUnit GenModule] ->
  DepGraph ->
  Gen ResumePlan
genResumePlan InitialProject {modulesSuccess} units graph = do
  fixErrors <- Gen.bool
  moduleMutations <- Map.fromList <$> Gen.subsequence (Map.toList modulesSuccess)
  pure ResumePlan {
    fixErrors,
    moduleMutations,
    depMutations,
    rebuild = computeRebuildSet graph (Map.keysSet moduleMutations <> Map.keysSet depMutations)
  }
  where
    depMutations =
      Map.fromList [
        (key, (added, toList (added <> deps)))
        |
        unit <- units,
        GenModule {..} <- unit.modules,
        Just added <- [resumeDeps]
      ]
