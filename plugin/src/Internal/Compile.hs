{-# language ViewPatterns, CPP #-}

module Internal.Compile where

import Data.Maybe (isJust)
import GHC (DynFlags (..), Ghc, GhcException (..), GhcMonad (..), HscEnv, Phase)
import GHC.Driver.Backend (backendGeneratesCode)
import GHC.Driver.Env (HscEnv (..), hscUpdateFlags)
import GHC.Driver.Phases (Phase (..), StopPhase (..), startPhase)
import GHC.Driver.Pipeline (
  TPhase,
  TPipelineClass,
  asPipeline,
  cmmCppPipeline,
  cmmPipeline,
  fullPipeline,
  llvmLlcPipeline,
  llvmManglePipeline,
  llvmPipeline,
  mkPipeEnv,
  runPipeline,
  viaCPipeline,
  )
import GHC.Driver.Pipeline.Monad (PipeEnv (..), PipelineOutput (..))
import GHC.Driver.Session (augmentByWorkingDirectory, isNoLink, outputFile)
import GHC.Runtime.Loader (initializePlugins)
import GHC.Types.SourceFile (HscSource)
import GHC.Unit.Home.ModInfo (HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..), unlessM)
import GHC.Utils.Panic (panic, throwGhcExceptionIO)
import Internal.Cache (ModuleArtifacts (..), Target (..))
import System.Directory (doesFileExist)

type P m = TPipelineClass TPhase m

-- | This is inlined mostly verbatim (from @pipelineStart@) to allow us to return the iface from 'fullPipeline'.
-- While we'll eventually want to make this possible upstream, a proper solution will most likely involve much more
-- substantial refactoring.
pipelineOneshot :: P m => PipeEnv -> HscEnv -> FilePath -> m (Maybe ModuleArtifacts)
pipelineOneshot pipe_env hsc_env input_fn =
  fromPhase (startPhase $ src_suffix pipe_env)
  where
   frontend :: P m => HscSource -> m (Maybe ModuleArtifacts)
   frontend sf = do
     (iface, hml) <- fullPipeline pipe_env hsc_env input_fn sf
     pure (Just ModuleArtifacts {iface, bytecode = homeMod_bytecode hml})
   c :: P m => Phase -> m (Maybe x)
   c phase = Nothing <$ viaCPipeline phase pipe_env hsc_env Nothing input_fn
   as :: P m => Bool -> m (Maybe x)
   as use_cpp = Nothing <$ asPipeline use_cpp pipe_env hsc_env Nothing input_fn

   fromPhase :: P m => Phase -> m (Maybe ModuleArtifacts)
   fromPhase (Unlit p)  = frontend p
   fromPhase (Cpp p)    = frontend p
   fromPhase (HsPp p)   = frontend p
   fromPhase (Hsc p)    = frontend p
   fromPhase HCc        = c HCc
   fromPhase Cc         = c Cc
   fromPhase Ccxx       = c Ccxx
   fromPhase Cobjc      = c Cobjc
   fromPhase Cobjcxx    = c Cobjcxx
   fromPhase (As p)     = as p
   fromPhase LlvmOpt    = Nothing <$ llvmPipeline pipe_env hsc_env Nothing input_fn
   fromPhase LlvmLlc    = Nothing <$ llvmLlcPipeline pipe_env hsc_env Nothing input_fn
   fromPhase LlvmMangle = Nothing <$ llvmManglePipeline pipe_env hsc_env Nothing input_fn
   fromPhase StopLn     = return Nothing
   fromPhase CmmCpp     = Nothing <$ cmmCppPipeline pipe_env hsc_env input_fn
   fromPhase Cmm        = Nothing <$ cmmPipeline pipe_env hsc_env input_fn
   fromPhase Js         = undefined
   fromPhase MergeForeign = panic "fromPhase: MergeForeign"

setDumpPrefix :: PipeEnv -> HscEnv -> HscEnv
setDumpPrefix pipe_env hsc_env =
  hscUpdateFlags (\dflags -> dflags { dumpPrefix = src_basename pipe_env ++ "."}) hsc_env

compileFile :: HscEnv -> FilePath -> IO (Maybe ModuleArtifacts)
compileFile hsc_env src = do
   unlessM (doesFileExist offset_file) do
     throwGhcExceptionIO (CmdLineError ("does not exist: " ++ offset_file))
   runPipeline (hsc_hooks hsc_env) pipeline
   where
    offset_file = augmentByWorkingDirectory dflags src
    dflags = hsc_dflags hsc_env
    mb_o_file = outputFile dflags
    ghc_link = ghcLink dflags
    output
      | not (backendGeneratesCode (backend dflags)) = NoOutputFile
      | not (isNoLink ghc_link) = Persistent
      | isJust mb_o_file = SpecificFile
      | otherwise = Persistent
    pipe_env = mkPipeEnv NoStop offset_file Nothing output
    pipeline = pipelineOneshot pipe_env (setDumpPrefix pipe_env hsc_env) offset_file

compileEps :: Target -> Ghc (Maybe ModuleArtifacts)
compileEps (Target src) = do
  hsc_env <- liftIO . initializePlugins =<< getSession
  liftIO $ compileFile hsc_env src
