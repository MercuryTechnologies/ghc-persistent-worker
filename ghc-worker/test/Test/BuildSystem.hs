module Test.BuildSystem where

import Test.Build (initialStrategy, resumeStrategy, runSchedule)
import Test.Cache (writeResumeCache)
import Test.Data.BuildSystem (BuildSystem (..))
import Test.Data.Env (MaxJobs, SessionEnv (..))
import Test.Resume (cleanResumeArtifacts)

-- | Wire up the handlers that represent actions performed by Buck.
mkBuildSystem :: MaxJobs -> SessionEnv -> BuildSystem
mkBuildSystem maxJobs sessionEnv =
  BuildSystem {
    writeCache =
      writeResumeCache sessionEnv,
    runInitialBuild =
      runSchedule maxJobs (initialStrategy sessionEnv) [],
    runResumeBuild = \ resumeEnv doFixErrors ->
      runSchedule maxJobs (resumeStrategy resumeEnv doFixErrors),
    cleanArtifacts =
      cleanResumeArtifacts sessionEnv.tempDir
  }
