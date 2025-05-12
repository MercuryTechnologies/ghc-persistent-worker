{-# options_ghc -O0 #-}
module Data where

import Control.Concurrent (MVar, newMVar)

data TestB =
  TestB (MVar Int) (Maybe String)

instance Show TestB where
  show (TestB _ s) = show s

data TestA =
  TestA (Maybe TestB) TestB
  deriving stock (Show)

mkB1 :: IO TestB
mkB1 = do
  mv <- newMVar 1
  pure (TestB mv (Just "one"))

mkB2 :: IO TestB
mkB2 = do
  mv <- newMVar 2
  pure (TestB mv (Just "two"))

mkA :: IO TestA
mkA = do
  b1 <- mkB1
  b2 <- mkB2
  pure (TestA (Just b1) b2)
