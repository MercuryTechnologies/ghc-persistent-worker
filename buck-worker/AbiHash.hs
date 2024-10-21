module AbiHash where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (targetProfile)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Unit.Module.ModIface (mi_final_exts, mi_mod_hash)
import GHC.Utils.Logger (LogFlags (..), log_default_dump_context)
import GHC.Utils.Outputable (ppr, renderWithContext)
import System.FilePath (dropExtension)

data AbiHash =
  AbiHash {
    path :: String,
    hash :: String
  }
  deriving stock (Eq, Show)

readAbiHash ::
  MonadIO m =>
  HscEnv ->
  Maybe String ->
  m (Maybe AbiHash)
readAbiHash HscEnv {hsc_dflags, hsc_NC} (Just path) = do
  let hi_file = dropExtension path
  iface <- liftIO $ readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace hi_file
  pure (Just (AbiHash {path, hash = dump hsc_dflags (mi_mod_hash (mi_final_exts iface))}))
  where
    dump dflags = renderWithContext (log_default_dump_context (initLogFlags dflags)) . ppr

readAbiHash _ _ = pure Nothing
