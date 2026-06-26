{-# LANGUAGE CPP #-}

-- | Functions used for setting flags when parsing CLI args that aren't exported from "GHC.Driver.Session".
--
-- Most of these are very similar to the originals.
--
-- The warning flag updaters have been copied verbatim; only the monad was changed to allow us to use them.
-- We could optimize this code, but we really only want to use the custom parser for benchmarking.
module Internal.DynFlags.Update where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (State, modify')
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import FlatParse.Basic (utf8ToStr)
import GHC (DynFlags (..))
import GHC.Driver.DynFlags (
  Language,
  ModRenaming (..),
  PackageArg,
  PackageDBFlag (..),
  PackageFlag (..),
  PkgDbRef (..),
  gopt_set,
  gopt_unset,
  lang_set,
  xopt_set,
  xopt_unset,
  )
import GHC.Driver.Flags (WarningFlag, WarningGroup, warningGroupFlags, warningGroupIncludesExtendedWarnings)
import GHC.Driver.Session (
  impliedXFlags,
  wopt_set,
  wopt_set_all_custom,
  wopt_set_all_fatal_custom,
  wopt_set_custom,
  wopt_set_fatal,
  wopt_set_fatal_custom,
  wopt_unset,
  wopt_unset_all_custom,
  wopt_unset_all_fatal_custom,
  wopt_unset_custom,
  wopt_unset_fatal,
  wopt_unset_fatal_custom,
  )
import GHC.Fingerprint (fingerprintFingerprints, fingerprintString)
import GHC.LanguageExtensions (Extension)
import GHC.Platform.Ways (Way (..), addWay, wayGeneralFlags, wayUnsetGeneralFlags)
import GHC.Settings (ToolSettings (..))
import GHC.Unit.Module.Warnings (WarningCategory)

#if defined(MWB)

import System.OsPath (unsafeEncodeUtf)

#endif

splitPaths :: ByteString -> [String]
splitPaths spec = filter (not . null) (utf8ToStr <$> ByteString.split ':' spec)

updateImportPaths :: Maybe ByteString -> DynFlags -> DynFlags
updateImportPaths arg dflags =
  dflags {importPaths = maybe [] addPaths arg}
  where
    addPaths spec = dflags.importPaths ++ splitPaths spec

updateLibraryPaths :: ByteString -> DynFlags -> DynFlags
updateLibraryPaths arg dflags =
  dflags {libraryPaths = dflags.libraryPaths ++ splitPaths arg}

#if defined(MWB)

addPackageDB :: String -> DynFlags -> DynFlags
addPackageDB path dflags =
  dflags {packageDBFlags = PackageDB (PkgDbPath (unsafeEncodeUtf path)) : packageDBFlags dflags}

#else

addPackageDB :: String -> DynFlags -> DynFlags
addPackageDB path dflags =
  dflags {packageDBFlags = PackageDB (PkgDbPath path) : packageDBFlags dflags}

#endif

addCppFlag :: String -> String -> DynFlags -> DynFlags
addCppFlag flag value dflags =
  dflags {
    toolSettings = dflags.toolSettings {
      toolSettings_opt_P = newOpts,
      toolSettings_opt_P_fingerprint = fingerprintFingerprints (map fingerprintString newOpts)
    }
  }
  where
    newOpts = ("-" ++ flag ++ value) : dflags.toolSettings.toolSettings_opt_P

addWayDyn :: DynFlags -> DynFlags
addWayDyn dflags =
  let platform = targetPlatform dflags
      dflags1 = dflags {targetWays_ = addWay WayDyn (targetWays_ dflags)}
      dflags2 = foldl' gopt_set dflags1 (wayGeneralFlags platform WayDyn)
      dflags3 = foldl' gopt_unset dflags2 (wayUnsetGeneralFlags platform WayDyn)
  in dflags3

addWayProf :: DynFlags -> DynFlags
addWayProf dflags =
  let platform = targetPlatform dflags
      dflags1 = dflags {targetWays_ = addWay WayProf (targetWays_ dflags)}
      dflags2 = foldl' gopt_set dflags1 (wayGeneralFlags platform WayProf)
      dflags3 = foldl' gopt_unset dflags2 (wayUnsetGeneralFlags platform WayProf)
  in dflags3

extensionMap :: Map ByteString Extension
extensionMap =
  Map.fromList [(ByteString.pack (show ext), ext) | ext <- [minBound .. maxBound]]

languageMap :: Map ByteString Language
languageMap =
  Map.fromList [(ByteString.pack (show lang), lang) | lang <- [minBound .. maxBound]]

data ExtensionUpdate =
  SetLanguage Language
  |
  SetExtension Bool Extension
  deriving stock (Eq, Show)

resolveExtension :: ByteString -> Bool -> Maybe ExtensionUpdate
resolveExtension name disable =
  (SetLanguage <$> languageMap !? name)
  <|>
  (SetExtension disable <$> extensionMap !? name)

setExtensionFlag', unSetExtensionFlag' :: Extension -> DynFlags -> DynFlags
setExtensionFlag' f dflags = foldr ($) (xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedXFlags, f' == f ]

unSetExtensionFlag' f dflags = xopt_unset dflags f

updateExtension :: ExtensionUpdate -> DynFlags -> DynFlags
updateExtension = \case
  SetLanguage language ->
    flip lang_set (Just language)
  SetExtension disable extension ->
    (if disable then flip xopt_unset else setExtensionFlag') extension

-- TODO This does not parse renamings
addExpose :: DynFlags -> String -> PackageArg -> DynFlags
addExpose dflags doc pkgArg =
  dflags {
    packageFlags = ExposePackage doc pkgArg (ModRenaming True []) : packageFlags dflags
  }

upd :: (DynFlags -> DynFlags) -> State DynFlags ()
upd = modify'

setWarningGroup :: WarningGroup -> State DynFlags ()
setWarningGroup g = do
    mapM_ setWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_set_all_custom

unSetWarningGroup :: WarningGroup -> State DynFlags ()
unSetWarningGroup g = do
    mapM_ unSetWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_unset_all_custom

setWErrorWarningGroup :: WarningGroup -> State DynFlags ()
setWErrorWarningGroup g =
  do { setWarningGroup g
     ; setFatalWarningGroup g }

setFatalWarningGroup :: WarningGroup -> State DynFlags ()
setFatalWarningGroup g = do
    mapM_ setFatalWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_set_all_fatal_custom

unSetFatalWarningGroup :: WarningGroup -> State DynFlags ()
unSetFatalWarningGroup g = do
    mapM_ unSetFatalWarningFlag (warningGroupFlags g)
    when (warningGroupIncludesExtendedWarnings g) $ upd wopt_unset_all_fatal_custom


setWarningFlag, unSetWarningFlag :: WarningFlag -> State DynFlags ()
setWarningFlag   f = upd (\dfs -> wopt_set dfs f)
unSetWarningFlag f = upd (\dfs -> wopt_unset dfs f)

setFatalWarningFlag, unSetFatalWarningFlag :: WarningFlag -> State DynFlags ()
setFatalWarningFlag   f = upd (\dfs -> wopt_set_fatal dfs f)
unSetFatalWarningFlag f = upd (\dfs -> wopt_unset_fatal dfs f)

setWErrorFlag :: WarningFlag -> State DynFlags ()
setWErrorFlag flag =
  do { setWarningFlag flag
     ; setFatalWarningFlag flag }


setCustomWarningFlag, unSetCustomWarningFlag :: WarningCategory -> State DynFlags ()
setCustomWarningFlag   f = upd (\dfs -> wopt_set_custom dfs f)
unSetCustomWarningFlag f = upd (\dfs -> wopt_unset_custom dfs f)

setCustomFatalWarningFlag, unSetCustomFatalWarningFlag :: WarningCategory -> State DynFlags ()
setCustomFatalWarningFlag   f = upd (\dfs -> wopt_set_fatal_custom dfs f)
unSetCustomFatalWarningFlag f = upd (\dfs -> wopt_unset_fatal_custom dfs f)

setCustomWErrorFlag :: WarningCategory -> State DynFlags ()
setCustomWErrorFlag flag =
  do { setCustomWarningFlag flag
     ; setCustomFatalWarningFlag flag }
