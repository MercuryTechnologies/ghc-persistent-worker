module BuckProxy.Util where

import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO (hPutStrLn, stderr)

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

