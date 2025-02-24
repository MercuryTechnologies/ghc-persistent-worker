{-# language ViewPatterns, CPP, OverloadedStrings #-}

module Internal.CompileHpt where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import GHC (DynFlags (..), Ghc, GhcMonad (..), ModLocation (..), ModSummary (..))
import GHC.Driver.Env (HscEnv (..), hscUpdateHUG)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Pipeline (compileOne)
import GHC.Runtime.Loader (initializePlugins)
import GHC.Unit.Env (addHomeModInfoToHug, ue_unsafeHomeUnit)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..))
import Internal.Cache (ModuleArtifacts (..), Target (..))
import Internal.CompileMake (step1)
import Internal.Error (eitherMessages)
import System.FilePath ((</>))

addDepsToHscEnv :: [HomeModInfo] -> HscEnv -> HscEnv
addDepsToHscEnv deps = hscUpdateHUG (\hug -> foldr addHomeModInfoToHug hug deps)

setHiLocation :: HscEnv -> ModSummary -> ModSummary
setHiLocation HscEnv {hsc_dflags = DynFlags {outputHi = Just path}} summ =
  summ {ms_location = summ.ms_location {ml_hi_file = path}}
setHiLocation _ summ = summ

compileHpt :: String -> NonEmpty (String, String, [String]) -> Target -> Ghc (Maybe ModuleArtifacts)
compileHpt tmp units (Target src) = do
  step1 unitArgs
  hsc_env <- liftIO . initializePlugins =<< getSession
  hmi@HomeModInfo {hm_iface = iface, hm_linkable} <- liftIO do
    summary <-
      fmap (setHiLocation hsc_env) .
      eitherMessages GhcDriverMessage =<<
      summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
    compileOne hsc_env summary 1 100000 Nothing (HomeModLinkable Nothing Nothing)
  modifySession (addDepsToHscEnv [hmi])
  pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hm_linkable})
  where
    unitArgs = units <&> \ (name, srcDir, deps) ->
      ["-i", "-i" ++ srcDir, "-hidir" ++ tmp </> "out", "-i" ++ tmp </> "out", "-this-unit-id", name] ++ concat [["-package-id", dep] | dep <- deps]
