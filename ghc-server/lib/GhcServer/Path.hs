-- | Path utilities shared across GHC server modules.
module GhcServer.Path where

import System.OsPath.Extra (OsPath, decodeUtf, osp, toOsPath, unsafeEncodeUtf, (</>))

-- TODO remove
fp :: OsPath -> FilePath
fp p =
  either (error . msg) id (decodeUtf p)
  where
    msg err = "Decoding path " <> show p <> " failed: " <> show err

-- TODO remove
osPath :: String -> OsPath
osPath = unsafeEncodeUtf

-- | Directory names under the project root for server artifacts.
outputDirName, tmpDirName, socketDirName :: OsPath
outputDirName = toOsPath "output"
tmpDirName = toOsPath "tmp"
socketDirName = toOsPath "socket"

-- | The Unix socket path for the server, placed under the project root.
socketPath :: OsPath -> OsPath
socketPath projectRoot = projectRoot </> socketDirName </> [osp|server.sock|]
