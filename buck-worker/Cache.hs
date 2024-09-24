{-# language BlockArguments #-}

module Cache where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, readIORef, writeIORef)
import GHC (Ghc)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad (modifySession, withSession)
import GHC.Runtime.Interpreter (Interp)

data Cache =
  Cache {
    interp :: Interp
  }

type CacheRef = IORef (Maybe Cache)

withCache :: CacheRef -> Ghc a -> Ghc a
withCache cache prog = do
  restoreCache
  prog <* updateCache
  where
    restoreCache =
      liftIO (readIORef cache) >>= traverse_ \ Cache {interp} ->
        modifySession \ env -> env {hsc_interp = Just interp}

    updateCache =
      withSession \ HscEnv {hsc_interp} ->
        for_ hsc_interp \ interp ->
          liftIO $ writeIORef cache (Just Cache {interp})
