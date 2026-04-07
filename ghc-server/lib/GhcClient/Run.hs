{-# LANGUAGE ApplicativeDo #-}

module GhcClient.Run where

import BuckWorkerProto (ExecuteCommand)
import Common.Grpc (sendRequest, waitPoll)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bifunctor (first)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GhcServer.Data.Config (ClientConfig (..))
import GhcServer.Path (socketPath)
import Internal.Log (dbg)
import Network.GRPC.Client (Server (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  eitherReader,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  many,
  metavar,
  progDesc,
  short,
  strArgument,
  switch,
  (<**>),
  )
import Proto.Worker_Fields qualified as Fields
import System.Exit (die)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.OsPath (encodeUtf)
import System.OsPath.Extra (fromOsPath)

-- | CLI argument parser for the client.
clientConfigParser :: Parser ClientConfig
clientConfigParser = do
  projectRoot <- argument readOsPath (metavar "PROJECT_ROOT" <> help "Path to the project root directory")
  wait <- switch (long "wait" <> short 'w' <> help "Wait for the build to complete before returning")
  recompile <- switch (long "recompile" <> help "Recompile modules even when cached artifacts exist")
  rebuild <- switch (long "rebuild" <> help "Recompute metadata and recompile even when cached")
  targets <- many (strArgument (metavar "TARGETS..." <> help "Schedule targets (e.g. unit1 unit2:metadata unit2:Module)"))
  pure ClientConfig {..}
  where
    readOsPath =
      eitherReader (first show <$> encodeUtf)

clientParserInfo :: ParserInfo ClientConfig
clientParserInfo =
  info (clientConfigParser <**> helper) (fullDesc <> progDesc desc <> header "CLI for ghc-server")
  where
    desc = "Send build commands to ghc-server"

-- | Wait for the server to come online, then send a gRPC request to schedule jobs.
client :: ClientConfig -> ExceptT String IO ()
client config = do
  liftIO do
    hPutStrLn stderr ("Connecting to ghc-server at " ++ fromOsPath socket)
    waitPoll socket
  response <- liftIO $ sendRequest (ServerUnix (fromOsPath socket)) request
  dbg (Text.unpack response.stderr)
  if response.exitCode == 0
  then dbg "Build succeeded."
  else throwE "Build failed."
  where
    request :: Proto ExecuteCommand
    request =
      defMessage
        & Fields.argv .~ ("schedule" : flagArgs ++ [encodeUtf8 (Text.pack arg) | arg <- config.targets])

    flagArgs =
      ["--wait" | config.wait]
      ++ ["--recompile" | config.recompile]
      ++ ["--rebuild" | config.rebuild]

    socket = socketPath config.projectRoot

-- | Parse CLI args and run the client command.
runClient :: IO ()
runClient = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  config <- execParser clientParserInfo
  either die pure =<< runExceptT (client config)
