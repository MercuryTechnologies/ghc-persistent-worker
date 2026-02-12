module Test.Run where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (TestT, property, test, withTests)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))
