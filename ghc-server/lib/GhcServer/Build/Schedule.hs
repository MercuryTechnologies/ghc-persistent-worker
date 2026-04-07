-- | Compute build tasks for concurrent compilation.
--
-- Compile tasks are created as /pending/ from unit source file paths at batch time,
-- before metadata is known. After metadata completes for a unit,
-- 'resolveFromCachedUnit' produces a resolution map from the written cache that is
-- applied at promotion time by the scheduler.
-- 'promoteEnabled' then activates tasks that are both requested and resolvable,
-- transitively promoting cross-unit dependencies.
{-# LANGUAGE CPP #-}

module GhcServer.Build.Schedule where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (ModuleName, mkModuleName, moduleNameString)
import GHC.Unit.Module.Graph (ModuleGraphNode (..), nodeKeyModName, nodeKeyUnitId)
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
import GHC.Unit.Module.Graph (mgNodeDependencies)
#else
import GHC.Unit.Module.Graph (nodeDependencies)
#endif
import GHC.Unit.Types (UnitId, unitIdString)
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..), unitId)
import GhcServer.Path (osPath)
import GhcServer.Scheduler (Phase (..), Task (..))
import System.OsPath (OsPath, (</>))
import Types.CachedDeps (
  CachedDep (..),
  CachedDeps (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  )

-- | Key for a build task, indexed by 'Phase'.
--
-- @TaskKey \''Pending@: compile tasks are keyed by 'OsPath' (source file path,
-- known at project discovery time).
--
-- @TaskKey \''Resolved@: compile tasks are keyed by 'ModuleName' (determined
-- after metadata).  Dependencies are always expressed in terms of
-- @TaskKey \''Resolved@.
data TaskKey (p :: Phase) where
  -- | Metadata step for a unit.  Valid in any phase.
  MetaTask :: UnitName -> TaskKey p
  -- | Compile step for a source file within a unit.  Pending pool only.
  PendingSource :: UnitName -> OsPath -> TaskKey 'Pending
  -- | Compile step for a module within a unit.  Active\/completed tasks only.
  ResolvedModule :: UnitName -> ModuleName -> TaskKey 'Resolved

deriving stock instance Show (TaskKey p)
deriving stock instance Eq (TaskKey p)

instance Ord (TaskKey 'Pending) where
  compare (MetaTask a) (MetaTask b) = compare a b
  compare (MetaTask _) _ = LT
  compare _ (MetaTask _) = GT
  compare (PendingSource u1 p1) (PendingSource u2 p2) = compare (u1, p1) (u2, p2)

instance Ord (TaskKey 'Resolved) where
  compare (MetaTask a) (MetaTask b) = compare a b
  compare (MetaTask _) _ = LT
  compare _ (MetaTask _) = GT
  compare (ResolvedModule u1 m1) (ResolvedModule u2 m2) = compare (u1, m1) (u2, m2)

-- | Per-request flags carried by every build task.
--
-- Identity data (unit name, source path, module name) lives in 'TaskKey'.
-- The 'Task' record pairs the key with this status.
--
-- * @rebuild@: when 'True', skip cache and run the step unconditionally.
-- * @cached@: whether the unit has a cache directory from a prior build.
--   Only meaningful for metadata tasks — compile tasks ignore it.
data BuildStatus =
  BuildStatus {
    rebuild :: Bool,
    cached :: Bool
  }
  deriving stock (Eq, Show)

-- | Reverse mapping from GHC 'UnitId' to 'UnitName', precomputed from a 'Project'.
unitIdToName :: Project -> Map UnitId UnitName
unitIdToName project =
  Map.fromList [(unitId name, name) | name <- Map.keys project.units]

-- | Look up a 'UnitName' from a 'UnitId', falling back to the raw unit id string.
lookupUnitName :: Map UnitId UnitName -> UnitId -> UnitName
lookupUnitName nameMap uid =
  Map.findWithDefault (UnitName (unitIdString uid)) uid nameMap

-- | Build metadata tasks for the given units.
--
-- Each metadata task depends on the metadata tasks of its home-unit dependencies.
-- Metadata tasks are created as active (resolved), not pending.
-- The @cachedUnits@ set marks units that have cache from a prior build.
metadataTasks :: Project -> Set UnitName -> Bool -> [UnitName] -> [Task TaskKey 'Resolved BuildStatus]
metadataTasks project cachedUnits rebuild =
  map metaTask
  where
    metaTask name =
      Task {
        key = MetaTask name,
        deps = Set.fromList
          [MetaTask dep | dep <- depUnits],
        enabled = True,
        value = BuildStatus {rebuild, cached = Set.member name cachedUnits}
      }
      where
        depUnits = maybe [] (.depUnits) (Map.lookup name project.units)

-- | Create pending compile tasks from a unit's source files.
--
-- Each task depends only on its unit's metadata task. Foreign-unit and home-unit
-- module deps are injected later at promotion time using resolution data.
--
-- The @isEnabled@ flag controls whether the task is eligible for promotion.
-- Tasks for implicit dependency units should pass @False@; the scheduler's
-- 'insertPending' will upgrade the flag with OR if a later batch enables them.
--
-- The @rebuild@ flag is embedded in the task value for dispatch-time skip decisions.
compileTasksFromSources :: UnitName -> Bool -> Bool -> [OsPath] -> [Task TaskKey 'Pending BuildStatus]
compileTasksFromSources name rebuild isEnabled =
  map mkTask
  where
    mkTask src =
      Task {
        key = PendingSource name src,
        deps = Set.singleton (MetaTask name),
        enabled = isEnabled,
        value = BuildStatus {rebuild, cached = False}
      }


-- | Resolve dependencies of a module graph node to pending 'TaskKey's.
--
-- Only home-unit dependencies (those present in the module graph) are resolved;
-- external package dependencies are ignored.  The source map provides the
-- 'OsPath' for each home module, needed to construct 'PendingSource' keys.
nodeDepsToTaskKeys ::
  Map UnitId UnitName ->
  Map (UnitId, ModuleName) OsPath ->
  ModuleGraphNode ->
  Set (TaskKey 'Pending)
nodeDepsToTaskKeys nameMap srcMap node =
  Set.fromList
    [PendingSource depName depSrc
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
      | nk <- mgNodeDependencies True node
#else
      | nk <- nodeDependencies True node
#endif
      , Just depModName <- [nodeKeyModName nk]
      , let depUid = nodeKeyUnitId nk
      , Just depSrc <- [Map.lookup (depUid, depModName) srcMap]
      , let depName = lookupUnitName nameMap depUid
    ]

-- | Resolution map type.
--
-- Maps a pending 'TaskKey' to its resolved key, resolved 'BuildTask' value,
-- and pending module-level dependencies.  The pending deps are converted to
-- resolved keys during promotion by the scheduler.
type Resolutions = Map (TaskKey 'Pending) (TaskKey 'Resolved, BuildStatus, Set (TaskKey 'Pending))

-- | Key for a module in the build system's module map.
data ModuleKey =
  ModuleKey {
    unit :: UnitName,
    name :: ModuleName
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON ModuleKey where
  toJSON key =
    object ["unit" .= key.unit.string, "name" .= moduleNameString key.name]

instance FromJSON ModuleKey where
  parseJSON =
    withObject "ModuleKey" \ o -> do
      unitStr <- o .: "unit"
      nameStr <- o .: "name"
      pure ModuleKey {unit = UnitName unitStr, name = mkModuleName nameStr}

-- | Per-module build information.
--
-- Stores direct dependencies and the @.dyn_hi@ path.  The scheduler uses @deps@
-- for ordering; 'buildModuleCachedDeps' assembles the transitive 'CachedDeps'
-- on demand at compile time from these direct deps.
data ModuleInfo =
  ModuleInfo {
    task :: TaskKey 'Pending,
    deps :: Set ModuleKey,
    hiPath :: OsPath
  }

-- | Build a module map from a 'CachedUnit'.
--
-- Given a 'CachedUnit' (read from @cached_unit.json@), produces a 'Map'
-- 'ModuleKey' 'ModuleInfo' with direct dependencies and @.dyn_hi@ paths.
-- No transitive closure is computed here — that is deferred to
-- 'buildModuleCachedDeps' at compile time.
resolveFromCachedUnit ::
  UnitName ->
  OsPath ->
  CachedUnit ->
  Map ModuleKey ModuleInfo
resolveFromCachedUnit name outputDir cu =
  Map.fromList
    [ (key, ModuleInfo {task = pendingKey, deps = directDeps cm, hiPath = modHiPath key.name})
    | (JsonFs mn, cm) <- Map.toList moduleMap
    , let modName = mkModuleName (moduleNameString mn)
    , let key = ModuleKey {unit = name, name = modName}
    , let pendingKey = PendingSource name cm.source
    ]
  where
    moduleMap = fromMaybe Map.empty (cu.cache <|> cu.build_plan)

    directDeps :: CachedModule -> Set ModuleKey
    directDeps cm =
      Set.fromList (homeDeps ++ packageDeps')
      where
        homeDeps =
          [ ModuleKey {unit = name, name = mkModuleName (moduleNameString (jsonFsVal depMod))}
          | depMod <- cm.modules
          ]

        packageDeps' =
          [ ModuleKey {unit = UnitName (unitIdString (jsonFsVal pkgId)), name = mkModuleName (moduleNameString (jsonFsVal depMod))}
          | CachedPackageDep {id = pkgId, modules = depMods} <- cm.packages
          , depMod <- depMods
          ]

    modHiPath modName =
      outputDir </> osPath (unitIdString (unitId name)) </> osPath (moduleNameString modName ++ ".dyn_hi")

    jsonFsVal :: JsonFs a -> a
    jsonFsVal (JsonFs a) = a

-- | Assemble deduplicated, topologically sorted 'CachedDeps' for a module
-- from the full module map.
--
-- Performs a DFS with post-order emission (leaves first) and a visited set
-- for deduplication.  This gives the correct load order for HPT pre-population:
-- a module's dependencies are listed before the module itself.
--
-- The result excludes the target module itself — only its transitive deps.
buildModuleCachedDeps :: Map ModuleKey ModuleInfo -> ModuleKey -> CachedDeps
buildModuleCachedDeps allModules target =
  CachedDeps (snd (go Set.empty roots))
  where
    roots =
      maybe [] (Set.toList . (.deps)) (Map.lookup target allModules)

    go :: Set ModuleKey -> [ModuleKey] -> (Set ModuleKey, [CachedDep])
    go visited [] = (visited, [])
    go visited (k : ks)
      | Set.member k visited = go visited ks
      | otherwise =
        case Map.lookup k allModules of
          Nothing -> go visited' ks
          Just info ->
            let
              (visited'', childDeps) = go visited' (Set.toList info.deps)
              (visited''', siblingDeps) = go visited'' ks
            in
              (visited''', childDeps ++ [mkDep k] ++ siblingDeps)
      where
        visited' = Set.insert k visited

    mkDep key =
      CachedDep {
        name = JsonFs key.name,
        package = JsonFs (unitId key.unit)
      }

-- | Derive scheduler 'Resolutions' from a module map.
--
-- Maps each pending key to a resolved key with direct module-level
-- dependencies.  Dependencies are resolved by looking up each dep's
-- 'ModuleKey' in the combined (prior + new) module map.
resolutionsFromModuleMap ::
  Map ModuleKey ModuleInfo ->
  Map ModuleKey ModuleInfo ->
  Resolutions
resolutionsFromModuleMap priorModules newModules =
  Map.fromList
    [ (info.task, (ResolvedModule key.unit key.name, BuildStatus {rebuild = False, cached = False}, depTasks info))
    | (key, info) <- Map.toList newModules
    ]
  where
    allModules = Map.union newModules priorModules

    depTasks :: ModuleInfo -> Set (TaskKey 'Pending)
    depTasks info =
      Set.fromList
        [ depInfo.task
        | depKey <- Set.toList info.deps
        , Just depInfo <- [Map.lookup depKey allModules]
        ]
