{-# language BlockArguments #-}

module Internal.Error where

import Control.Concurrent.MVar (MVar)
import Control.Exception (AsyncException (..), Exception (..), IOException, throwIO)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC (Ghc, GhcException (..), printException)
import GHC.Types.SourceError (SourceError, throwErrors)
import Internal.Log (Log, logOther)
import System.Environment (getProgName)
import System.Exit (ExitCode)
import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Types.Error (Messages)

handleExceptions :: MVar Log -> a -> Ghc a -> Ghc a
handleExceptions logVar errResult =
  MC.handle \ e -> do
    handler e
    pure errResult
  where
    handler exception
      | Just (se :: SourceError) <- fromException exception
      = printException se

      | Just (ioe :: IOException) <- fromException exception
      = fm (show ioe)

      | Just UserInterrupt <- fromException exception
      = liftIO $ throwIO UserInterrupt

      | Just StackOverflow <- fromException exception
      = fm "stack overflow: use +RTS -K<size> to increase it"

      | Just (ex :: ExitCode) <- fromException exception
      = liftIO $ throwIO ex

      | Just ge <- fromException exception
      = case ge of
        Signal _ -> pure ()
        ProgramError _ -> fm (show ge)
        CmdLineError _ -> fm ("<command line>: " ++ show ge)
        _ -> do
          progName <- liftIO getProgName
          fm (progName ++ ": " ++ show ge)

      | otherwise
      = fm (show (Panic (show exception)))

    fm = logOther logVar

eitherMessages ::
  MonadIO m =>
  (a -> GhcMessage) ->
  Either (Messages a) b ->
  m b
eitherMessages toMessage = \case
  Right b -> pure b
  Left errs -> throwErrors (toMessage <$> errs)
