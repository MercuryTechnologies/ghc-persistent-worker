module Internal.Json where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import GHC (GhcMonad)
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Outputable (SDoc, parens, text, (<+>))
import Internal.Error (workerError)
import System.Directory.OsPath (doesFileExist)
import System.OsPath.Extra (OsPath, fromOsPath)

-- | Throw a driver error with a file path appended to the message.
jsonFileError ::
  GhcMonad m =>
  OsPath ->
  SDoc ->
  m a
jsonFileError path message =
  workerError (message <+> parens (text (fromOsPath path)))

-- | Read and decode a JSON file, throwing an error when decoding fails, but returning 'Nothing' when the file doesn't
-- exist.
optionalJsonFile ::
  forall a m .
  GhcMonad m =>
  FromJSON a =>
  String ->
  OsPath ->
  m (Maybe a)
optionalJsonFile desc path =
  liftIO (doesFileExist path) >>= \case
    False -> pure Nothing
    True ->
      either parseError (pure . Just) =<< liftIO (eitherDecodeFileStrict' @a (fromOsPath path))
  where
    parseError err =
      jsonFileError path ("Parse error in JSON file for" <+> text desc Outputable.<> ":" <+> text err)

-- | Read and decode a JSON file, throwing an error when decoding fails or the file doesn't exist.
requiredJsonFile ::
  GhcMonad m =>
  FromJSON a =>
  String ->
  OsPath ->
  m a
requiredJsonFile desc path =
  maybe missing pure =<< optionalJsonFile desc path
  where
    missing = jsonFileError path ("Missing JSON file for" <+> text desc)
