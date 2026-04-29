{-# LANGUAGE TemplateHaskell #-}

module Internal.DynFlags.Parse where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Function ((&))
import FlatParse.Basic (
  Parser,
  Result (..),
  branch,
  char,
  eof,
  many,
  optional_,
  runParser,
  utf8ToStr,
  withByteString,
  withError,
  )
import FlatParse.Basic.Switch (switch)
import GHC.Driver.DynFlags (DynFlags (..), GhcLink (..))
import GHC.Driver.Errors.Types (DriverMessages)
import GHC.Utils.Outputable (text, (<+>))
import Internal.DynFlags.Options (
  Opt (..),
  ParseError (..),
  UpdateFlags (..),
  UpdateWithArg (..),
  eol,
  parseOptionSpec,
  restOfLine,
  )
import Internal.Error (listToMessages, unknownError, unknownErrors)

-- | The result of parsing a CLI argument.
data Flag =
  -- | An option that updates 'DynFlags'.
  FlagOption UpdateFlags
  |
  -- | A positional argument, usually a source file.
  FlagPositional ByteString
  |
  -- | An unknown option, i.e. an argument starting with @-@.
  FlagUnknown ByteString
  |
  -- | An arg passed to @-X@ that isn't present in the extension list.
  FlagUnknownExtension ByteString
  |
  -- | An option that requires an argument was followed by EOF.
  FlagMissingArg ByteString

-- | Parse the argument to an option, either @-flag\nvalue@, @-flag=value@, or @-fvalue@.
parseArg ::
  Bool ->
  UpdateWithArg a a ->
  ByteString ->
  Parser ParseError Flag
parseArg separate UpdateWithArg {parser, update} name = do
  ensureFlagEnd
  branch eof (pure (FlagMissingArg name)) do
    arg <- parser <* eol
    pure (FlagOption (update arg))
  where
    ensureFlagEnd =
      if separate
      then $(char '\n') <|> $(char '=')
      else pure ()

-- | An optional argument is only allowed for options that don't use a separator, like @-i@ vs @-i/path@.
parseArgOptional ::
  UpdateWithArg a (Maybe a) ->
  Parser ParseError Flag
parseArgOptional UpdateWithArg {parser, update} = do
  arg <- branch eol (pure Nothing) (Just <$> parser <* eol)
  pure (FlagOption (update arg))

-- | An option without an argument.
parseSwitch :: UpdateFlags -> Parser ParseError Flag
parseSwitch update = pure (FlagOption update)

parseOption :: Parser ParseError Flag
parseOption =
  withByteString parseOptionSpec \case
    OptArg {..} -> parseArg separate handler
    OptArgOptional handler -> const (parseArgOptional handler)
    OptSwitch update -> const (pure (FlagOption update))
    OptUnknown -> const (FlagUnknown <$> restOfLine)

parseFlag :: Parser ParseError Flag
parseFlag =
  $(switch [|case _ of
    "-" -> withError parseOption handleError
    _ -> FlagPositional <$> restOfLine
  |])
  where
    handleError = \case
      UnknownExtension name -> pure (FlagUnknownExtension name)

parseFlagLine :: Parser ParseError Flag
parseFlagLine =
  parseFlag <* optional_ $(char '\n')

applyFlags :: DynFlags -> [Flag] -> Either DriverMessages (DynFlags, [ByteString])
applyFlags dflags flags =
  if null errors
  then Right (foldl' (&) dflags options, positional)
  else Left (listToMessages errors)
  where
    (errors, positional) = partitionEithers nonOptions
    (nonOptions, options) = partitionEithers (classify <$> flags)

    classify = \case
      FlagOption (UpdateFlags f) -> Right f
      FlagPositional src -> Left (Right src)
      FlagUnknown name -> flagError "Unrecognised flag:" name
      FlagUnknownExtension name -> flagError "Unknown extension:" name
      FlagMissingArg name -> flagError "Missing argument for" name

    flagError desc name = Left (Left (unknownError (Just "worker") dflags (desc <+> text (utf8ToStr name))))

unconsumedError :: DynFlags -> ByteString -> DriverMessages
unconsumedError dflags unconsumed =
  unknownErrors (Just "worker") dflags $
  text "FlagParser: Internal error, unconsumed input:" <+> text (utf8ToStr unconsumed)

parseDynFlags :: DynFlags -> ByteString -> Either DriverMessages (DynFlags, [ByteString])
parseDynFlags dflags0 input =
  case runParser (many parseFlagLine) input of
    OK flags "" -> applyFlags dflags1 flags
    OK _ unconsumed -> Left (unconsumedError dflags0 unconsumed)
    Fail -> unexpected
    Err _ -> unexpected
  where
    dflags1 = dflags0 {ghcLink = LinkBinary, verbosity = 0}

    unexpected = Left (unknownErrors (Just "worker") dflags0 "FlagParser: unexpected failure")
