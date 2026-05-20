module Internal.State.Stats where

import Control.Monad.IO.Class (liftIO)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Utils.Outputable (doublePrec, text, (<+>))
import Types.Log (Logger (..))

logMemStats :: String -> Logger -> IO ()
logMemStats step logger = do
  s <- liftIO getRTSStats
  let logMem desc value = logger.debugD (text (desc ++ ":") <+> doublePrec 2 (fromIntegral value / 1_000_000) <+> text "MB")
  logger.debugD (text ("-------------- " ++ step))
  logMem "Mem in use" s.gc.gcdetails_mem_in_use_bytes
  logMem "Max mem in use" s.max_mem_in_use_bytes
  logMem "Max live bytes" s.max_live_bytes
