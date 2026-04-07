{-# LANGUAGE CPP #-}
-- | Parse Cabal package descriptions to discover units for the standalone GHC server.
--
-- Each library component (main library and sub-libraries) becomes a 'Unit'.
-- Dependencies between local libraries are resolved by matching package names
-- against the set of known library names in the same package.
module GhcServer.Cabal where

import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Distribution.Compat.NonEmptySet (toList)
import Distribution.Package (packageName)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Dependency (Dependency, depLibraries, depPkgName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#endif
import Distribution.Verbosity (silent)
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..), mkUnitCache)
import GhcServer.Path (osPath)
import GhcServer.Project (isHaskellSource, unitDepNode)
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.OsPath (OsPath, decodeUtf, (</>))
import Types.Log (Logger (..))

-- | Find the first @.cabal@ file in a directory.
findCabalFile :: OsPath -> IO (Maybe FilePath)
findCabalFile dir = do
  entries <- listDirectory dir
  paths <- catMaybes <$> traverse decode entries
  pure (case filter (".cabal" `isSuffixOf`) paths of
    [f] -> Just f
    _ -> Nothing)
  where
    decode e = case decodeUtf (dir </> e) of
      Right p -> pure (Just p)
      Left _ -> pure Nothing

-- | The name of a library component.
libraryUnitName :: String -> LibraryName -> UnitName
libraryUnitName pkgName = \case
  LMainLibName -> UnitName pkgName
  LSubLibName c -> UnitName (unUnqualComponentName c)

-- | Extract the set of all library component names in a package.
localLibNames :: String -> GenericPackageDescription -> Set.Set UnitName
localLibNames pkgName gpd =
  Set.fromList $
    [libraryUnitName pkgName LMainLibName | Just _ <- [gpd.condLibrary]]
    ++
    [libraryUnitName pkgName (LSubLibName c) | (c, _) <- gpd.condSubLibraries]

-- | Extract local (home unit) dependency names from a dependency.
--
-- Handles two cases:
--
-- * @build-depends: test-project:lib-a@ — @depPkgName@ is the package name, the sub-library
--   name is in @depLibraries@.
-- * @build-depends: lib-a@ — @depPkgName@ is @lib-a@ directly (only if it matches a local name).
classifyDep :: String -> Set.Set UnitName -> Dependency -> [UnitName]
classifyDep pkgName locals dep
  | depName == pkgName =
    -- Qualified dep like test-project:lib-a — extract sub-library names
    [libraryUnitName pkgName ln | ln <- toList (depLibraries dep), ln /= LMainLibName]
  | UnitName depName `Set.member` locals =
    [UnitName depName]
  | otherwise =
    []
  where
    depName = unPackageName (depPkgName dep)

-- | Partition dependencies into local (home unit) and external names.
partitionDeps :: String -> Set.Set UnitName -> [Dependency] -> ([UnitName], [String])
partitionDeps pkgName locals deps =
  (concatMap (classifyDep pkgName locals) deps, externals)
  where
    externals =
      [ unPackageName (depPkgName d)
      | d <- deps
      , let dn = unPackageName (depPkgName d)
      , dn /= pkgName
      , UnitName dn `Set.notMember` locals
      ]

-- | Discover source files in a list of source directories.
discoverSources :: OsPath -> [FilePath] -> IO [OsPath]
discoverSources projectRoot srcDirs = do
  let dirs = if null srcDirs then [projectRoot] else map (\ s -> projectRoot </> osPath s) srcDirs
  concat <$> traverse listSourceDir dirs
  where
    listSourceDir dir = do
      exists <- doesFileExist dir >>= \case
        True -> pure False
        False -> pure True
      if exists
        then do
          entries <- listDirectory dir
          pure [dir </> e | e <- entries, isHaskellSource e]
        else pure []

-- | Build a 'Unit' from a library component.
buildUnit ::
  OsPath ->
  OsPath ->
  OsPath ->
  String ->
  Set.Set UnitName ->
  LibraryName ->
  Library ->
  IO Unit
buildUnit projectRoot outputDir tmpDir pkgName locals libName lib = do
  let name = libraryUnitName pkgName libName
      bi = lib.libBuildInfo
      srcDirPaths = map getSymbolicPath bi.hsSourceDirs
      (localDeps, extDeps) = partitionDeps pkgName locals bi.targetBuildDepends
      ghcArgs = concatMap (\ d -> ["-package", d]) extDeps
  sources <- discoverSources projectRoot srcDirPaths
  createDirectoryIfMissing True (outputDir </> osPath name.string)
  createDirectoryIfMissing True (tmpDir </> osPath name.string)
  pure Unit {
    name,
    dir = case srcDirPaths of
      (d : _) -> projectRoot </> osPath d
      [] -> projectRoot,
    ghcArgs,
    sources,
    depUnits = localDeps,
    cache = mkUnitCache projectRoot name
  }

-- | Discover a project from a @.cabal@ file.
--
-- Each library component becomes a unit. Sub-libraries that depend on each other
-- are linked via 'depUnits'. External dependencies become @-package@ flags.
discoverCabalProject :: Logger -> OsPath -> OsPath -> OsPath -> IO Project
discoverCabalProject logger projectRoot outputDir tmpDir = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True tmpDir
  mCabalFile <- findCabalFile projectRoot
  cabalFile <- case mCabalFile of
    Nothing -> do
      root <- either (const "<decode error>") id <$> pure (decodeUtf projectRoot)
      fail ("No .cabal file found in: " ++ root)
    Just f -> pure f
  logger.info ("Loading project configuration from " ++ cabalFile)
#if MIN_VERSION_Cabal(3,14,0)
  gpd <- readGenericPackageDescription silent Nothing (makeSymbolicPath cabalFile)
#else
  gpd <- readGenericPackageDescription silent cabalFile
#endif
  let pkgName = unPackageName (packageName gpd)
      locals = localLibNames pkgName gpd
  units <- sequence $
    [buildUnit projectRoot outputDir tmpDir pkgName locals LMainLibName lib
     | Just ct <- [gpd.condLibrary]
     , let lib = ct.condTreeData]
    ++
    [buildUnit projectRoot outputDir tmpDir pkgName locals (LSubLibName c) (ct.condTreeData)
     | (c, ct) <- gpd.condSubLibraries]
  let unitMap = Map.fromList [(u.name, u) | u <- units]
      depGraph = graphFromEdgedVerticesOrd (map unitDepNode units)
  pure Project {units = unitMap, depGraph}
