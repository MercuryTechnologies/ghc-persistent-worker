{-# OPTIONS_GHC -Wno-orphans #-}
module System.OsPath.Extra
  ( OsPathDecodingException (..)
  , OsPathEncodingException (..)
  , fromOsPath
  , toOsPath
  , encodeUtf
  , decodeUtf
  , module OsPathReexport
  ) where

import Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Text as T
import Control.Exception (Exception, SomeException, throw)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified System.OsPath as OsPath (decodeUtf, encodeUtf)

import System.OsPath as OsPathReexport hiding (decodeUtf, encodeUtf)

data OsPathDecodingException = OsPathDecodingException OsPath SomeException
  deriving stock (Show)

instance Exception OsPathDecodingException where

data OsPathEncodingException = OsPathEncodingException String SomeException
  deriving stock (Show)

instance Exception OsPathEncodingException where

-- | Like 'decodeUtf' but throws an exception instead of returning an Either
-- and the exception provides the filepath as context.
fromOsPath :: OsPath -> String
fromOsPath = either throw id . decodeUtf

-- | Like 'encodeUtf' but throws an exception instead of returning an Either
-- and the exception provides the filepath as context.
toOsPath :: String -> OsPath
toOsPath = either throw id . encodeUtf

-- | Like 'encodeUtf' but provides the filepath in exceptions
encodeUtf :: MonadThrow m => String -> m OsPath
encodeUtf p = either (throwM . OsPathEncodingException p) pure (OsPath.encodeUtf p)

-- | Like 'decodeUtf' but provides the filepath in exceptions
decodeUtf :: MonadThrow m => OsPath -> m String
decodeUtf p = either (throwM . OsPathDecodingException p) pure (OsPath.decodeUtf p)

instance ToJSON OsPath where
  toEncoding = toEncoding . fromOsPath
  toJSON = String . T.pack . fromOsPath
