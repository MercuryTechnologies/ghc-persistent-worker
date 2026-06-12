module Test.BuildSystem where

import Test.Build (initialStrategy, resumeStrategy, runSchedule)
import Test.Cache (writeResumeCache)
import Test.Data.BuildSystem (BuildSystem (..))
import Test.Data.Env (MaxJobs, SessionEnv (..))
import Test.Resume (cleanResumeArtifacts)

-- | Wire up the handlers that represent actions performed by Buck.
mkBuildSystem :: MaxJobs -> Bool -> SessionEnv -> BuildSystem
mkBuildSystem maxJobs useIncremental sessionEnv =
  BuildSystem {
    writeCache =
      writeResumeCache sessionEnv,
    runInitialBuild =
      runSchedule maxJobs (initialStrategy sessionEnv useIncremental) [],
    runResumeBuild = \ resumeEnv doFixErrors ->
      runSchedule maxJobs (resumeStrategy resumeEnv useIncremental doFixErrors),
    cleanArtifacts =
      cleanResumeArtifacts sessionEnv.tempDir
  }
