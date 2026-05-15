module BuckProxy.Run where

import BuckProxy.Orchestration (GhcWorkerCommand (..), WorkerExe (..), WorkerResource (..), proxyServer)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  many,
  metavar,
  optional,
  progDesc,
  strArgument,
  strOption,
  switch,
  (<**>),
  )
import System.OsPath.Extra (toOsPath)
import System.Process (terminateProcess)
import Types.Orchestration (PrimarySocketName (..), ServerSocketPath (..))


-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | The @ghc-worker@ executable and arguments.
    -- Used to spawn the GHC server, provided by Buck.
    command :: Maybe GhcWorkerCommand,

    -- | If 'True', don't kill the @ghc-worker@ process after the build has concluded (i.e. the @buck-proxy@ process is
    -- terminated by Buck).
    remain :: Bool,

    -- | Override the name of the worker socket instead of using @$BUCK_BUILD_ID@.
    -- This can be used with 'remain' to reuse a worker across builds.
    workerSocket :: Maybe PrimarySocketName
  }
  deriving stock (Eq, Show)

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  build
    <$> optional (strOption (long "exe" <> metavar "EXE" <> help "Path to the ghc-worker executable"))
    <*> switch (long "remain" <> help "Don't kill the ghc-worker process after the build")
    <*> optional (strOption (long "socket-name" <> metavar "NAME" <> help "Override the worker socket name"))
    <*> many (strArgument (metavar "ARGS..."))
  where
    build exe remain workerSocket args =
      CliOptions {
        command = exe <&> \ e -> GhcWorkerCommand {exe = WorkerExe e, args},
        remain,
        workerSocket = PrimarySocketName . toOsPath <$> workerSocket
      }

    (<&>) = flip fmap

cliOptionsParserInfo :: ParserInfo CliOptions
cliOptionsParserInfo =
  info (cliOptionsParser <**> helper)
    (fullDesc <> progDesc "Buck2 GHC worker proxy" <> header "buck-proxy")

-- | Main function for starting buck proxy using the provided server socket path and CLI options.
run ::
  -- | This is WORKER_SOCKET
  ServerSocketPath ->
  CliOptions ->
  MVar (IO ()) ->
  IO ()
run socket CliOptions {command, remain, workerSocket} refHandler
  | Nothing <- command
  = throwIO (userError "No ghc-worker executable specified on the command line")
  | Just cmd <- command
  = do
    refWorkerMap <- newMVar (Map.empty)
    -- SIGTERM Handler
    modifyMVar_ refHandler \_ -> pure do
      unless remain do
        wmap <- readMVar refWorkerMap
        for_ wmap \resource ->
          terminateProcess resource.processHandle
    proxyServer refWorkerMap cmd socket workerSocket

parseCliArgs :: IO CliOptions
parseCliArgs = execParser cliOptionsParserInfo
