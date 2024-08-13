{-# LANGUAGE NumericUnderscores #-}

module Util
  ( whenM,
    openFileAfterCheck,
    openPipeRead,
    openPipeWrite,
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (when)
import System.IO (Handle)
import System.Posix.Files (fileAccess)
import System.Posix.IO
  ( OpenFileFlags (nonBlock),
    OpenMode (ReadOnly, WriteOnly),
    defaultFileFlags,
    fdToHandle,
    openFd,
  )

-- Not wanting to depend on extra. Later, as we can depend on the ghc lib, we can outsource this.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM check action = do
  b <- check
  if b then action else pure ()

openFileAfterCheck :: FilePath -> (Bool, Bool) -> (FilePath -> IO Handle) -> IO Handle
openFileAfterCheck fp (doesRead, doesWrite) openAction = do
  let blockUntilPass = do
        b <- fileAccess fp doesRead doesWrite False
        when (not b) $ do
          threadDelay 1_000_000
          blockUntilPass
  openAction fp

openPipeRead :: FilePath -> IO Handle
openPipeRead fp = do
  fd <- openFd fp ReadOnly defaultFileFlags {nonBlock = True}
  h <- fdToHandle fd
  pure h

openPipeWrite :: FilePath -> IO Handle
openPipeWrite fp = do
  let blockUntilOpen = do
        efd <- try (openFd fp WriteOnly defaultFileFlags {nonBlock = True})
        case efd of
          Right fd -> pure fd
          Left (_ :: IOException) -> do
            threadDelay 1_000_000
            blockUntilOpen
  fd <- blockUntilOpen
  h <- fdToHandle fd
  pure h
