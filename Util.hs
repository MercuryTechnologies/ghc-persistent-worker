{-# LANGUAGE NumericUnderscores #-}

module Util
  ( fileOpenAfterCheck,
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.IO (Handle)
import System.Posix.Files (fileAccess)

fileOpenAfterCheck :: FilePath -> (Bool, Bool) -> (FilePath -> IO Handle) -> IO Handle
fileOpenAfterCheck fp (doesRead, doesWrite) openAction = do
  let blockUntilPass = do
        b <- fileAccess fp doesRead doesWrite False
        when (not b) $ do
          threadDelay 1_000_000
          blockUntilPass
  openAction fp

