module Internal.ValidateNames (
  validateModuleNames,
) where

import Control.Monad (unless)
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import GHC (ModuleGraph, moduleNameString)
import GHC.Driver.Env (HscEnv, hscActiveUnitId)
import GHC.Unit.Module (IsBootInterface (..))
import GHC.Unit.Module.Graph (mgModSummaries)
import GHC.Unit.Module.ModSummary (ModSummary, isBootSummary, msHsFilePath, ms_mod_name, ms_unitid)
import GHC.Utils.Panic (GhcException (..), throwGhcExceptionIO)

inferModuleName :: String -> String
inferModuleName =
  T.unpack
    . T.intercalate "."
    . reverse
    . takeWhile firstIsUpper
    . reverse
    . T.splitOn "."
    . T.replace "/" "."
    . stripSuffix_ ".hs"
    . T.pack
  where
    stripSuffix_ suf t = fromMaybe t (T.stripSuffix suf t)
    -- For now, we infer the expected module name by stopping when we find a parent directory which
    -- does not start with an uppercase letter.
    firstIsUpper t =
      case T.uncons t of
        Just (h, _) -> isUpper h
        Nothing -> False

checkSummary :: HscEnv -> ModSummary -> Maybe String
checkSummary hsc_env summary
  | ms_unitid summary /= hscActiveUnitId hsc_env = Nothing
  | isBootSummary summary == IsBoot = Nothing
  | modname /= inferred, modname /= "Main"
  = Just $ mconcat
    [
      path, ":\n",
      "  Module name does not match file name:\n",
      "  Expected: ", inferred, "\n",
      "    Actual: ", modname, "\n"
    ]
  | otherwise = Nothing
  where
    path = msHsFilePath summary
    modname = moduleNameString (ms_mod_name summary)
    inferred = inferModuleName path

-- | Check that each module's declared name matches the name inferred from
-- its source path.
-- Doing this validation helps avoid confusing "Could not find module X" downstream errors.
validateModuleNames :: HscEnv -> ModuleGraph -> IO ()
validateModuleNames hsc_env graph = do
  let errors = mapMaybe (checkSummary hsc_env) (mgModSummaries graph)
  unless (null errors) do
    throwGhcExceptionIO (ProgramError (intercalate "\n" errors))
