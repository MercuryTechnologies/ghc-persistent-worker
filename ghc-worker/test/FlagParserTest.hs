module FlagParserTest where

import Control.Arrow ((>>>))
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Coerce (coerce)
import Data.List (intercalate)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Extra (fst3)
import GHC (GeneralFlag (..), mkGeneralLocated)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Driver.DynFlags (DynFlags (..), OnOff (..), Option (..), PackageDBFlag (..), PkgDbRef (..), WarningFlag (..))
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Errors.Types (DriverMessage)
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.LanguageExtensions (Extension (..))
import GHC.Paths (libdir)
import GHC.Settings (ToolSettings (..))
import GHC.Types.Error (DiagnosticOpts, defaultDiagnosticOpts)
import GHC.Unit (stringToUnitId)
import GHC.Utils.Error (pprMessages)
import GHC.Utils.Outputable (Outputable (..), showPprUnsafe)
import Hedgehog (TestT, assert, evalEither, failure, (===))
import Internal.DynFlags.Parse (parseDynFlags)
import System.OsPath.Extra (toOsPath)
import Test.Run (assertJust, unitTest)
import Test.Tasty (TestTree, testGroup)

newtype DbRef =
  DbRef PkgDbRef
  deriving stock (Eq)

instance Show DbRef where
  show = coerce >>> \case
    GlobalPkgDb -> "global"
    UserPkgDb -> "user"
    PkgDbPath path -> show path

dbRef :: String -> DbRef
dbRef path = DbRef (PkgDbPath (toOsPath path))

newtype DbFlag =
  DbFlag PackageDBFlag
  deriving stock (Eq)

instance Show DbFlag where
  show = coerce >>> \case
    PackageDB ref -> show (DbRef ref)
    NoUserPackageDB -> "NoUserPackageDB"
    NoGlobalPackageDB -> "NoGlobalPackageDB"
    ClearPackageDBs -> "ClearPackageDBs"

newtype ShowPpr a =
  ShowPpr a
  deriving stock (Eq)

instance Outputable a => Show (ShowPpr a) where
  show (ShowPpr a) = showPprUnsafe a

dbFlags :: DynFlags -> [DbFlag]
dbFlags dflags = DbFlag <$> dflags.packageDBFlags

diagOpts :: DiagnosticOpts DriverMessage
diagOpts = defaultDiagnosticOpts @DriverMessage

parseVanilla :: DynFlags -> [ByteString] -> IO (Either String DynFlags)
parseVanilla dflags args = do
  bimap show fst3 <$> try @SomeException do
    parseDynamicFlagsCmdLine dflags (mkGeneralLocated "test" . Text.unpack . decodeUtf8 <$> args)

parseFast :: DynFlags -> [ByteString] -> Either String (DynFlags, [ByteString])
parseFast dflags0 args =
  first (showPprUnsafe . pprMessages diagOpts) (parseDynFlags dflags0 (ByteString.unlines args))

parseTest :: [ByteString] -> TestT IO (DynFlags, Either String DynFlags, Either String (DynFlags, [ByteString]))
parseTest args =
  liftIO do
    dflags <- (.hsc_dflags) <$> initHscEnv (Just libdir)
    dflagsVanilla <- parseVanilla dflags args
    pure (dflags, dflagsVanilla, (parseFast dflags args))

parseTestSuccess :: [ByteString] -> TestT IO (DynFlags, DynFlags, DynFlags, [ByteString])
parseTestSuccess args = do
  (dflags0, vanillaResult, parseResult) <- parseTest args
  (dflags, positional) <- evalEither parseResult
  dflagsVanilla <- evalEither vanillaResult
  pure (dflags0, dflagsVanilla, dflags, positional)

argsSuccess :: [ByteString]
argsSuccess =
  [
    "-hide-all-packages",
    "-include-pkg-deps",
    "-no-link",
    "-dynamic",
    "-fbyte-code-and-object-code",
    "-fprefer-byte-code",
    "-fPIC",
    "-fwrite-ide-info",
    "-fexternal-dynamic-refs",
    "-fpackage-db-byte-code",
    "-prof",
    "-haddock",
    "-idiscarded1:discarded2",
    "-i",
    "-ione::two:three:",
    "-fdefer-diagnostics",
    "-osuf", "o_test",
    "-j",
    "-package-db", "/path1",
    "-package-db=/path2",
    "-O2",
    "-XViewPatterns",
    "-XNoViewPatterns",
    "-XPatternSynonyms",
    "-XNoFlexibleContexts",
    "-XScopedTypeVariables",
    "-XGHC2021",
    "-odir", "/o",
    "-hidir", "/hi",
    "-stubdir", "/stub",
    "-hiedir", "/hie",
    "-dumpdir", "/dump",
    "-this-unit-id", "unit-test",
    "-package-env", "/package-env",
    "-dep-json", "/dep-json",
    "-llink",
    "-L/linkdir",
    "-DFEATURE=no",
    "-UUNDEF",
    "-Werror",
    "-Wall",
    "-Werror=unused-binds",
    "-Wno-missing-exported-signatures",
    "-Wno-missing-export-lists",
    "-Wno-missing-import-lists",
    "-Wno-missed-specialisations",
    "-Wno-all-missed-specialisations",
    "-Wno-unsafe",
    "-Wno-missing-local-signatures",
    "-Wno-monomorphism-restriction",
    "-Wno-missing-safe-haskell-mode",
    "-Wno-unused-packages",
    "-Wno-operator-whitespace",
    "-Wno-missing-kind-signatures",
    "-Wno-implicit-lift",
    "-Wno-missing-role-annotations",
    "-Wno-term-variable-capture",
    "-Wno-missing-poly-kind-signatures",
    "-Wno-x-partial",
    "-Wno-defaulted-exception-context",
    "-fno-warn-ambiguous-fields",
    "-fwarn-tabs",
    "-fdefer-diagnostics",
    "-fdiagnostics-color=always",
    "-fno-defer-type-errors",
    "-fobject-determinism",
    "Mod1.hs",
    "Mod2.hs"
  ]

test_flagParser_success :: TestT IO ()
test_flagParser_success = do
  (dflags0, dflagsVanilla, dflags, positional) <- parseTestSuccess argsSuccess
  ["Mod1.hs", "Mod2.hs"] === positional
  EnumSet.toList dflagsVanilla.generalFlags === EnumSet.toList dflags.generalFlags
  -- This optimization flag is only enabled with @-O2@
  assert (not (Opt_DictsStrict `elem` EnumSet.toList dflags0.generalFlags))
  assert (Opt_DictsStrict `elem` EnumSet.toList dflags.generalFlags)
  ["one", "two", "three"] === dflags.importPaths
  "o_test" === dflags.objectSuf_
  [On ExplicitForAll, On ScopedTypeVariables, Off FlexibleContexts, On PatternSynonyms, Off ViewPatterns, On ViewPatterns] === dflags.extensions
  case dflags.packageDBFlags of
    [PackageDB ref1, PackageDB ref2] -> do
      dbRef "/path2" === DbRef ref1
      dbRef "/path1" === DbRef ref2
    _ -> failure
  dbFlags dflagsVanilla === dbFlags dflags
  dflagsVanilla.extensions === dflags.extensions
  assertJust "/o" dflags.objectDir
  assertJust "/hi" dflags.hiDir
  assertJust "/stub" dflags.stubDir
  assertJust "/hie" dflags.hieDir
  assertJust "/dump" dflags.dumpDir
  ShowPpr (stringToUnitId "unit-test") === ShowPpr dflags.homeUnitId_
  [
    Opt_WarnUnusedTopBinds,
    Opt_WarnUnusedLocalBinds,
    Opt_WarnUnusedPatternBinds,
    Opt_WarnAmbiguousFields
    ] === EnumSet.toList dflags.fatalWarningFlags
  ["-llink"] === [o | Option o <- dflags.ldInputs]
  ["/linkdir"] === dflags.libraryPaths
  ["-UUNDEF", "-DFEATURE=no"] === dflags.toolSettings.toolSettings_opt_P

argsMissingArg :: [ByteString]
argsMissingArg =
  [
    "-osuf", "o",
    "-j",
    "-package-db"
  ]

targetMissingArg :: String
targetMissingArg =
  "worker: error: Missing argument for package-db"

test_flagParser_missingArg :: TestT IO ()
test_flagParser_missingArg =
  parseTest argsMissingArg >>= \case
    (_, _, Left errs) -> targetMissingArg === errs
    _ -> failure

argsInvalidExtension :: [ByteString]
argsInvalidExtension =
  [
    "-XInvalid",
    "-XNoUnknownFeature"
  ]

targetInvalidExtension :: String
targetInvalidExtension =
  intercalate "\n" [
    "worker: error: Unknown extension: Invalid",
    "worker: error: Unknown extension: UnknownFeature"
  ]

test_flagParser_invalidExtension :: TestT IO ()
test_flagParser_invalidExtension =
  parseTest argsInvalidExtension >>= \case
    (_, _, Left errs) -> targetInvalidExtension === errs
    _ -> failure

argsUnknown :: [ByteString]
argsUnknown =
  [
    "-osuf", "o",
    "-j",
    "-package-db", "/path",
    "-unknown1",
    "-O2",
    -- TODO this should not result in unconsumed input.
    -- The problem is that in parseOptionSpec, when parsing succeeds for the branching case but
    -- fails afterwards (due to the toEol thing), some failure logic is skipped, idk.
    -- "-hide-all-packagess",
    "-unknown2"
  ]

targetUnknown :: String
targetUnknown =
  intercalate "\n" [
    "worker: error: Unrecognised flag: unknown1",
    "worker: error: Unrecognised flag: unknown2"
  ]

test_flagParser_unknown :: TestT IO ()
test_flagParser_unknown =
  parseTest argsUnknown >>= \case
    (_, _, Left errs) -> targetUnknown === errs
    _ -> failure

argsUnknownPartial :: [ByteString]
argsUnknownPartial =
  [
    "-Wall",
    "-Wno-xxx",
    "-Wall"
  ]

targetUnknownPartial :: String
targetUnknownPartial =
  intercalate "\n" [
    "worker: error: Unknown warning: xxx"
  ]

test_flagParser_unknownPartial :: TestT IO ()
test_flagParser_unknownPartial =
  parseTest argsUnknownPartial >>= \case
    (_, _, Left errs) -> targetUnknownPartial === errs
    _ -> failure

test_parseBuckArgs :: TestTree
test_parseBuckArgs =
  testGroup "flag parser" [
    unitTest "successful" test_flagParser_success,
    unitTest "missing argument" test_flagParser_missingArg,
    unitTest "invalid extension" test_flagParser_invalidExtension,
    unitTest "unknown options" test_flagParser_unknown,
    unitTest "unknown flag with partial match" test_flagParser_unknownPartial
  ]
