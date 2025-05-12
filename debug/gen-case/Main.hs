{-# options_ghc -O0 #-}
module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Data (TestA (TestA), mkA, mkB1, mkB2)
import Data.Foldable (traverse_)
import GHC.Debug.Stub (withGhcDebug)

setup :: IO (MVar [TestA])
setup = do
  b1 <- mkB1
  b2 <- mkB2
  a1 <- mkA
  newMVar [a1, TestA (Just b1) b2]
{-# noinline setup #-}

main :: IO ()
main = do
  withGhcDebug do
    mv <- setup
    b2 <- mkB2
    modifyMVar_ mv \ as -> pure (TestA Nothing b2 : as)
    threadDelay 10_000_000_000
    readMVar mv >>= traverse_ \ a -> print a
