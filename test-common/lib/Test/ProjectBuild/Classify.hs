-- | Description: Create informational output for various statistics.
module Test.ProjectBuild.Classify where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Hedgehog (PropertyT, classify, label)
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Project (InitialProject (..))
import Test.Data.ProjectBuild (ProjectBuild (..), ResumePlan (..))

classifyFirstBuild :: BuildResult -> PropertyT IO ()
classifyFirstBuild result = do
  label "── first build ──"
  classify "  success" (not result.hasErrors)
  classify "  error" result.hasErrors

classifyProject :: ProjectBuild -> PropertyT IO ()
classifyProject ProjectBuild {initial = InitialProject {unitCount, moduleCount}} = do
  label "── project size ──"
  classify "  1 unit" (unitCount == 1)
  classify "  2-3 units" (unitCount >= 2 && unitCount <= 3)
  classify "  4-5 units" (unitCount >= 4)
  classify "  1-3 modules" (moduleCount <= 3)
  classify "  4-10 modules" (moduleCount >= 4 && moduleCount <= 10)
  classify "  >10 modules" (moduleCount > 10)

classifyResume :: ProjectBuild -> BuildResult -> PropertyT IO ()
classifyResume ProjectBuild {resumePlan, initial = InitialProject {moduleCount}} BuildResult {hasErrors} = do
  label "── resume build ──"
  classify "  error build" hasErrors
  classify "  success build" (not hasErrors)
  when hasErrors do
    classify "  fix errors" fixErrors
    classify "  keep errors" (not fixErrors)
  classify "  with modifications" hasModifications
  classify "  with added deps" hasAddedDeps
  -- TODO this is badly named. also should we avoid this case entirely, since nothing significant happens, or does it
  -- have interesting properties?
  classify "  full resume" (not hasModifications && not hasAddedDeps)
  when hasModifications do
    classify "  rebuild all modules" (Map.size moduleMutations == moduleCount)
    classify "  rebuild subset" (Map.size moduleMutations < moduleCount)
  where
    hasModifications = not (Map.null moduleMutations)

    hasAddedDeps = not (Map.null depMutations)

    ResumePlan {fixErrors, moduleMutations, depMutations} = resumePlan
