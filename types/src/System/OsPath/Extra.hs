module System.OsPath.Extra where

import Control.Exception (Exception, SomeException, throw)
import System.OsPath (OsPath, decodeUtf, encodeUtf)

data OsPathDecodingException = OsPathDecodingException OsPath SomeException
  deriving stock (Show)

instance Exception OsPathDecodingException where

data OsPathEncodingException = OsPathEncodingException String SomeException
  deriving stock (Show)

instance Exception OsPathEncodingException where

-- | Like 'decodeUtf' but throws an exception instead of returning an Either
-- and the exception provides the filepath as context.
fromOsPath :: OsPath -> String
fromOsPath p = either (throw . OsPathDecodingException p) id (decodeUtf p)

-- | Like 'encodeUtf' but throws an exception instead of returning an Either
-- and the exception provides the filepath as context.
toOsPath :: String -> OsPath
toOsPath p = either (throw . OsPathEncodingException p) id (encodeUtf p)
