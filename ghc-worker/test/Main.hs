{-# LANGUAGE CPP #-}

module Main where

import qualified BuildPlanTest.Test1 (test_buildPlan)
import qualified BuildPlanTest.Test2 (test_buildPlan)
import FlagParserTest (test_parseBuckArgs)
import Incremental.BuildTest (test_incrementalBuild)
import Incremental.FlowTest (test_incrementalFlow)
import InterfacePathTest (test_interfacePath)
import ProjectBuildTest (test_projectBuild)
import ResourceTest (test_resources)
import ScheduleTest (test_sortScheduleOrder)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Data.Env (testConfigOptions)
import Test.Tasty (
  DependencyType (AllFinish),
  TestTree,
  after,
  defaultIngredients,
  defaultMainWithIngredients,
  includingOptions,
  testGroup,
  )

-- | Some tests require our GHC patches.
fullTest :: Bool

#if defined(MWB)

fullTest = True

#else

fullTest = False

#endif

testsGeneral :: [TestTree]
testsGeneral =
  [
    test_parseBuckArgs,
    test_interfacePath,
    test_sortScheduleOrder,
    test_projectBuild,
    testGroup "incremental metadata" [
      test_incrementalBuild,
      test_incrementalFlow
    ]
  ] <> if fullTest then [
    BuildPlanTest.Test1.test_buildPlan,
    BuildPlanTest.Test2.test_buildPlan
  ] else []

tests :: TestTree
tests =
  testGroup "all" [
    test_resources,
    afterResources (testGroup "general" testsGeneral)
  ]
  where
    -- tasty 1.5 has @sequentialTestGroup@, but the current Nix env has 1.4, so we'll make do with this for now.
    afterResources = after AllFinish "resources"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMainWithIngredients (includingOptions testConfigOptions : defaultIngredients) tests
