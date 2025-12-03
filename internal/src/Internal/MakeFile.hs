{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# language NoImplicitPrelude, FieldSelectors, CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#define FIXED_NODES defined(MWB_2025_10)

-----------------------------------------------------------------------------
--
-- Makefile Dependency Generation
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Internal.MakeFile where

#if FIXED_NODES

import Data.Either (partitionEithers)
import GHC.Data.OsPath (unsafeDecodeUtf, unsafeEncodeUtf)
import GHC.Types.Error (mkUnknownDiagnostic)

#else

import GHC.Driver.Ppr

#endif

import Control.Monad (unless, when)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import GHC.Data.Graph.Directed (SCC (..))
import GHC.Data.Maybe
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Make (cyclicModuleErr, downsweep)
import GHC.Driver.Monad
import GHC.Driver.Phases (Phase (Unlit), StopPhase (StopPreprocess), startPhase)
import GHC.Driver.Pipeline (TPhase (T_FileArgs, T_Unlit), mkPipeEnv, runPipeline, use)
import GHC.Driver.Pipeline.Monad (PipelineOutput (NoOutputFile))
import GHC.Driver.Session (pgm_F)
import GHC.Prelude
import qualified GHC.SysTools as SysTools
import GHC.Types.Error (unionManyMessages)
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.State (lookupUnitId)
import GHC.Utils.Error
import GHC.Utils.Exception
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.TmpFs
import Internal.MakeFile.JSON
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (isEOFError)

#if !MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
import GHC.Utils.Panic.Plain
#endif

#if !defined(MWB)
depJSON :: DynFlags -> Maybe FilePath
depJSON _ = Nothing

ms_opts :: ModSummary -> [String]
ms_opts _ = []

#endif

doMkDependHS :: GhcMonad m => [FilePath] -> m ModuleGraph
doMkDependHS srcs = do
    -- Initialisation
    dflags0 <- GHC.getSessionDynFlags

    -- If no suffix is provided, use the default -- the empty one
    let dflags = if null (depSuffixes dflags0)
                 then dflags0 { depSuffixes = [""] }
                 else dflags0

    -- Do the downsweep to find all the modules
    targets <- mapM (\s -> GHC.guessTarget s Nothing Nothing) srcs
    GHC.setTargets targets
    let excl_mods = depExcludeMods dflags
    (errs, module_graph) <- withSession \ hsc_env -> liftIO $ downsweepCompat hsc_env [] excl_mods True
    let msgs = unionManyMessages errs
    unless (isEmptyMessages msgs) $ throwErrors (fmap GhcDriverMessage msgs)
    doMkDependModuleGraph dflags module_graph
    pure module_graph
    where
#if FIXED_NODES
      downsweepCompat hsc_env = downsweep hsc_env mkUnknownDiagnostic Nothing
#else
      downsweepCompat hsc_env old_summaries excl_mods allow_dup_roots =
        fmap mkModuleGraph <$> downsweep hsc_env old_summaries excl_mods allow_dup_roots
#endif

-----------------------------------------------------------------
--
--              The main function
--
-----------------------------------------------------------------

doMkDependModuleGraph :: GhcMonad m => DynFlags -> ModuleGraph -> m ()
doMkDependModuleGraph dflags module_graph = do
    logger <- getLogger
    tmpfs <- hsc_tmpfs <$> getSession
    let excl_mods = depExcludeMods dflags

    files <- liftIO $ beginMkDependHS logger tmpfs dflags
    let sorted = GHC.topSortModuleGraph False module_graph Nothing

    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg logger 2 (text "Module dependencies" $$ ppr sorted)

    hsc_env <- getSession
    let node_dep_map = buildNodeDepMap hsc_env sorted

    -- Process them one by one, dumping results into makefile
    -- and complaining about cycles
    root <- liftIO getCurrentDirectory
    mapM_ (liftIO . processDeps dflags hsc_env excl_mods root (mkd_tmp_hdl files) (mkd_dep_json files) node_dep_map) sorted

    -- If -ddump-mod-cycles, show cycles in the module graph
    liftIO $ dumpModCycles logger module_graph

    -- Tidy up
    liftIO $ endMkDependHS logger files

-----------------------------------------------------------------
--
--              beginMkDependHs
--      Create a temporary file,
--      find the Makefile,
--      slurp through it, etc
--
-----------------------------------------------------------------

data MkDepFiles
  = MkDep { mkd_make_file :: FilePath,          -- Name of the makefile
            mkd_make_hdl  :: Maybe Handle,      -- Handle for the open makefile
             -- | Output interface for the -dep-json file
            mkd_dep_json  :: !(Maybe (JsonOutput DepJSON)),
            mkd_tmp_file  :: FilePath,          -- Name of the temporary file
            mkd_tmp_hdl   :: Handle }           -- Handle of the open temporary file

beginMkDependHS :: Logger -> TmpFs -> DynFlags -> IO MkDepFiles
beginMkDependHS logger tmpfs dflags = do
        -- open a new temp file in which to stuff the dependency info
        -- as we go along.
  tmp_file <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "dep"
  tmp_hdl <- openFile tmp_file WriteMode

        -- open the makefile
  let makefile = depMakefile dflags
  exists <- doesFileExist makefile
  mb_make_hdl <-
        if not exists
        then return Nothing
        else do
           makefile_hdl <- openFile makefile ReadMode

                -- slurp through until we get the magic start string,
                -- copying the contents into dep_makefile
           let slurp = do
                l <- hGetLine makefile_hdl
                if (l == depStartMarker)
                        then return ()
                        else do hPutStrLn tmp_hdl l; slurp

                -- slurp through until we get the magic end marker,
                -- throwing away the contents
           let chuck = do
                l <- hGetLine makefile_hdl
                if (l == depEndMarker)
                        then return ()
                        else chuck

           catchIO slurp
                (\e -> if isEOFError e then return () else ioError e)
           catchIO chuck
                (\e -> if isEOFError e then return () else ioError e)

           return (Just makefile_hdl)

  dep_json_ref <- mkJsonOutput initDepJSON (depJSON dflags)

        -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  return (MkDep { mkd_make_file = makefile, mkd_make_hdl = mb_make_hdl,
                  mkd_dep_json = dep_json_ref,
                  mkd_tmp_file  = tmp_file, mkd_tmp_hdl  = tmp_hdl})

-----------------------------------------------------------------
--
--              processDeps
--
-----------------------------------------------------------------

type NodeDepMap = Map.Map NodeKey Dep

buildNodeDepMap :: HscEnv -> [SCC ModuleGraphNode] -> NodeDepMap
buildNodeDepMap hsc_env =
  foldl' insertScc Map.empty
  where
    insertScc acc = foldl' insertNode acc . flatten

    flatten (AcyclicSCC node) = [node]
    flatten (CyclicSCC nodes) = nodes

    insertNode acc node =
      case node of
#if FIXED_NODES
        ModuleNode _ (ModuleNodeCompile info) ->
          Map.insert (mkNodeKey node) (mkDep info) acc
#else
        ModuleNode _ info ->
          Map.insert (mkNodeKey node) (mkDep info) acc
#endif
        _ -> acc

    mkDep :: ModSummary -> Dep
    mkDep info =
      let loc = ms_location info
          dep_unit_id = ms_unitid info
          dep_mod = ms_mod info
          dep_local = isJust (ml_hs_file loc) && dep_unit_id == hscActiveUnitId hsc_env
      in DepHi
        { dep_mod
        , dep_unit_id
        , dep_path = ml_hi_file loc
        , dep_unit = lookupUnitId (hsc_units hsc_env) dep_unit_id
        , dep_local
        , dep_boot = isBootSummary info
        }

processDeps :: DynFlags
            -> HscEnv
            -> [ModuleName]
            -> FilePath
            -> Handle           -- Write dependencies to here
            -> Maybe (JsonOutput DepJSON)
            -> NodeDepMap
            -> SCC ModuleGraphNode
            -> IO ()
-- Write suitable dependencies to handle
-- Always:
--                      this.o : this.hs
--
-- If the dependency is on something other than a .hi file:
--                      this.o this.p_o ... : dep
-- otherwise
--                      this.o ...   : dep.hi
--                      this.p_o ... : dep.p_hi
--                      ...
-- (where .o is $osuf, and the other suffixes come from
-- the cmdline -s options).
--
-- For {-# SOURCE #-} imports the "hi" will be "hi-boot".

processDeps _dflags_ _ _ _ _ _ _ (AcyclicSCC (LinkNode {})) = return ()

#if FIXED_NODES

processDeps _ _ _ _ _ _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwOneError $ cyclicModuleErr nodes
processDeps _ _ _ _ _ _ _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwOneError $
      mkPlainErrorMsgEnvelope noSrcSpan $
      GhcDriverMessage $ DriverInstantiationNodeInDependencyGeneration node
processDeps _ _ _ _ _ _ _ (AcyclicSCC (UnitNode {})) = return ()
processDeps _ _ _ _ _ _ _ (AcyclicSCC (ModuleNode _ (ModuleNodeFixed {})))
  -- No dependencies needed for fixed modules (already compiled)
  = return ()

processDeps dflags hsc_env excl_mods root hdl m_dep_json node_dep_map (AcyclicSCC (ModuleNode node_deps (ModuleNodeCompile node)))

#else

processDeps dflags _ _ _ _ _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwGhcExceptionIO $ ProgramError $
      showSDoc dflags $ cyclicModuleErr nodes

processDeps dflags _ _ _ _ _ _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwGhcExceptionIO $ ProgramError $
      showSDoc dflags $
        vcat [ text "Unexpected backpack instantiation in dependency graph while constructing Makefile:"
             , nest 2 $ ppr node ]

processDeps dflags hsc_env excl_mods root hdl m_dep_json node_dep_map (AcyclicSCC (ModuleNode node_deps node))

#endif

  | hscActiveUnitId hsc_env /= ms_unitid node
  = pure ()
  | otherwise
  = do
  pp <- preprocessor
  deps <- fmap concat $ sequence $
    [cpp_deps | depIncludeCppDeps dflags] ++ [
      pure graph_deps
    ]
  updateJson m_dep_json (updateDepJSON include_pkg_deps pp dep_node deps)
  writeDependencies include_pkg_deps root hdl extra_suffixes dep_node deps
  where
    extra_suffixes = depSuffixes dflags
    include_pkg_deps = depIncludePkgDeps dflags
    src_file = msHsFilePath node
    dep_node =
      DepNode {
        dn_mod = ms_mod node,
        dn_src = src_file,
        dn_obj = msObjFilePath node,
        dn_hi = msHiFilePath node,
        dn_boot = isBootSummary node,
        dn_options = Set.fromList (ms_opts node)
      }
    graph_deps =
      [ dep
      | edge <- node_deps
      , dep <- maybeToList (Map.lookup edge node_dep_map)
      , moduleName (dep.dep_mod) `notElem` excl_mods
      ]

    preprocessor
      | Just src <- ml_hs_file (ms_location node)
      = runPipeline (hsc_hooks hsc_env) $ do
        let (_, suffix) = splitExtension src
            lit | Unlit _ <- startPhase suffix = True
                | otherwise = False
            pipe_env = mkPipeEnv StopPreprocess src Nothing NoOutputFile
        unlit_fn <- if lit then use (T_Unlit pipe_env hsc_env src) else pure src
        (dflags1, _, _) <- use (T_FileArgs hsc_env unlit_fn)
        let pp = pgm_F dflags1
        pure (if null pp then global_preprocessor else Just pp)
      | otherwise
      = pure global_preprocessor

    global_preprocessor
      | let pp = pgm_F dflags
      , not (null pp)
      = Just pp
      | otherwise
      = Nothing

    -- Emit a dependency for each CPP import
    -- CPP deps are discovered in the module parsing phase by parsing
    -- comment lines left by the preprocessor.
    -- Note that GHC.parseModule may throw an exception if the module
    -- fails to parse, which may not be desirable (see #16616).
    cpp_deps = do
      session <- Session <$> newIORef hsc_env
      parsedMod <- reflectGhc (GHC.parseModule node) session
      pure (DepCpp <$> GHC.pm_extra_src_files parsedMod)

writeDependencies ::
  Bool ->
  FilePath ->
  Handle ->
  [FilePath] ->
  DepNode ->
  [Dep] ->
  IO ()
writeDependencies include_pkgs root hdl suffixes node deps =
  traverse_ write tasks
  where
    tasks = source_dep : boot_dep ++ concatMap import_dep deps

    -- Emit std dependency of the object(s) on the source file
    -- Something like       A.o : A.hs
    source_dep = (obj_files, dn_src)

    -- add dependency between objects and their corresponding .hi-boot
    -- files if the module has a corresponding .hs-boot file (#14482)
    boot_dep
      | IsBoot <- dn_boot
      = [([obj], hi) | (obj, hi) <- zip (suffixed (viaOsPath removeBootSuffix dn_obj)) (suffixed dn_hi)]
      | otherwise
      = []

    -- Add one dependency for each suffix;
    -- e.g.         A.o   : B.hi
    --              A.x_o : B.x_hi
    import_dep = \case
      DepHi {dep_path, dep_boot, dep_unit}
        | isNothing dep_unit || include_pkgs
        , let path = if dep_boot == IsBoot then viaOsPath addBootSuffix dep_path else dep_path
        -> [([obj], hi) | (obj, hi) <- zip obj_files (suffixed path)]

        | otherwise
        -> []

      DepCpp {dep_path} -> [(obj_files, dep_path)]

    write (from, to) = writeDependency root hdl from to

    obj_files = suffixed dn_obj

    suffixed f = insertSuffixes f suffixes

    DepNode {dn_src, dn_obj, dn_hi, dn_boot} = node

#if FIXED_NODES

    viaOsPath f a = unsafeDecodeUtf (f (unsafeEncodeUtf a))

#else

    viaOsPath f a = f a

#endif

-----------------------------
writeDependency :: FilePath -> Handle -> [FilePath] -> FilePath -> IO ()
-- (writeDependency r h [t1,t2] dep) writes to handle h the dependency
--      t1 t2 : dep
writeDependency _root hdl targets dep
  = do let -- We need to avoid making deps on
           --     c:/foo/...
           -- on cygwin as make gets confused by the :
           -- Making relative deps avoids some instances of this.
           -- dep' = makeRelative root dep
           forOutput = escapeSpaces . reslash Forwards . normalise
           output = unwords (map forOutput targets) ++ " : " ++ dep -- forOutput dep'
       hPutStrLn hdl output

-----------------------------
insertSuffixes
        :: FilePath     -- Original filename;   e.g. "foo.o"
        -> [String]     -- Suffix prefixes      e.g. ["x_", "y_"]
        -> [FilePath]   -- Zapped filenames     e.g. ["foo.x_o", "foo.y_o"]
        -- Note that the extra bit gets inserted *before* the old suffix
        -- We assume the old suffix contains no dots, so we know where to
        -- split it
insertSuffixes file_name extras
  = [ basename <.> (extra ++ suffix) | extra <- extras ]
  where
    (basename, suffix) = case splitExtension file_name of
                         -- Drop the "." from the extension
                         (b, s) -> (b, drop 1 s)


-----------------------------------------------------------------
--
--              endMkDependHs
--      Complete the makefile, close the tmp file etc
--
-----------------------------------------------------------------

endMkDependHS :: Logger -> MkDepFiles -> IO ()

endMkDependHS logger
   (MkDep { mkd_make_file = makefile, mkd_make_hdl = makefile_hdl,
            mkd_dep_json,
            mkd_tmp_file = tmp_file, mkd_tmp_hdl = tmp_hdl })
  = do
  -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do
        -- slurp the rest of the original makefile and copy it into the output
        SysTools.copyHandle hdl tmp_hdl
        hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

        -- Create a backup of the original makefile
  when (isJust makefile_hdl) $ do
    showPass logger ("Backing up " ++ makefile)
    SysTools.copyFile makefile (makefile++".bak")

        -- Copy the new makefile in place
  showPass logger "Installing new makefile"
  SysTools.copyFile tmp_file makefile

  -- Write the dependency and option data to a json file if the corresponding
  -- flags were specified.
  writeJsonOutput mkd_dep_json


-----------------------------------------------------------------
--              Module cycles
-----------------------------------------------------------------

dumpModCycles :: Logger -> ModuleGraph -> IO ()
dumpModCycles logger module_graph
  | not (logHasDumpFlag logger Opt_D_dump_mod_cycles)
  = return ()

  | null cycles
  = putMsg logger (text "No module cycles")

  | otherwise
  = putMsg logger (hang (text "Module cycles found:") 2 pp_cycles)
  where
    topoSort = GHC.topSortModuleGraph True module_graph Nothing

    cycles :: [[ModuleGraphNode]]
    cycles =
      [ c | CyclicSCC c <- topoSort ]

    pp_cycles = vcat [ (text "---------- Cycle" <+> int n <+> text "----------")
                        $$ pprCycle c $$ blankLine
                     | (n,c) <- [1..] `zip` cycles ]

#if FIXED_NODES

pprCycle :: [ModuleGraphNode] -> SDoc
-- Print a cycle, but show only the imports within the cycle
pprCycle summaries = pp_group (CyclicSCC summaries)
  where
    cycle_keys :: [NodeKey]  -- The modules in this cycle
    cycle_keys = map mkNodeKey summaries

    pp_group :: SCC ModuleGraphNode -> SDoc
    pp_group (AcyclicSCC (ModuleNode deps m)) = pp_mod deps m
    pp_group (AcyclicSCC _) = empty
    pp_group (CyclicSCC mss)
        = assert (not (null boot_only)) $
                -- The boot-only list must be non-empty, else there would
                -- be an infinite chain of non-boot imports, and we've
                -- already checked for that in processModDeps
          pp_mod loop_deps loop_breaker $$ vcat (map pp_group groups)
        where
          (boot_only, others) = partitionEithers (map is_boot_only mss)
          is_boot_key (NodeKey_Module (ModNodeKeyWithUid (GWIB _ IsBoot) _)) = True
          is_boot_key _ = False
          is_boot_only n@(ModuleNode deps ms) =
            let non_boot_deps = filter (not . is_boot_key) deps
            in if not (any in_group non_boot_deps)
                then Left (deps, ms)
                else Right n
          is_boot_only n = Right n
          in_group m = m `elem` group_mods
          group_mods = map mkNodeKey mss

          (loop_deps, loop_breaker) =  head boot_only
          all_others   = tail (map (uncurry ModuleNode) boot_only) ++ others
          groups =
            GHC.topSortModuleGraph True (mkModuleGraph all_others) Nothing

    pp_mod :: [NodeKey] -> ModuleNodeInfo -> SDoc
    pp_mod deps mn =
      text mod_str <> text (take (20 - length mod_str) (repeat ' ')) <> ppr_deps deps
      where
        mod_str = moduleNameString (moduleNodeInfoModuleName mn)

    ppr_deps :: [NodeKey] -> SDoc
    ppr_deps [] = empty
    ppr_deps deps =
      let is_mod_dep (NodeKey_Module {}) = True
          is_mod_dep _ = False

          is_boot_dep (NodeKey_Module (ModNodeKeyWithUid (GWIB _ IsBoot) _)) = True
          is_boot_dep _ = False

          cycle_deps = filter (`elem` cycle_keys) deps
          (mod_deps, other_deps) = partition is_mod_dep cycle_deps
          (boot_deps, normal_deps) = partition is_boot_dep mod_deps
      in vcat [
           if null normal_deps then empty
           else text "imports" <+> pprWithCommas ppr normal_deps,
           if null boot_deps then empty
           else text "{-# SOURCE #-} imports" <+> pprWithCommas ppr boot_deps,
           if null other_deps then empty
           else text "depends on" <+> pprWithCommas ppr other_deps
         ]

#else

pprCycle :: [ModuleGraphNode] -> SDoc
-- Print a cycle, but show only the imports within the cycle
pprCycle summaries = pp_group (CyclicSCC summaries)
  where
    cycle_mods :: [ModuleName]  -- The modules in this cycle
    cycle_mods = map (moduleName . ms_mod) [ms | ModuleNode _ ms <- summaries]

    pp_group :: SCC ModuleGraphNode -> SDoc
    pp_group (AcyclicSCC (ModuleNode _ ms)) = pp_ms ms
    pp_group (AcyclicSCC _) = empty
    pp_group (CyclicSCC mss)
        = assert (not (null boot_only)) $
                -- The boot-only list must be non-empty, else there would
                -- be an infinite chain of non-boot imports, and we've
                -- already checked for that in processModDeps
          pp_ms loop_breaker $$ vcat (map pp_group groups)
        where
          (boot_only, others) = partition is_boot_only mss
          is_boot_only (ModuleNode _ ms) = not (any in_group (map snd (ms_imps ms)))
          is_boot_only  _ = False
          in_group (L _ m) = m `elem` group_mods
          group_mods = map (moduleName . ms_mod) [ms | ModuleNode _ ms <- mss]

          loop_breaker = head ([ms | ModuleNode _ ms  <- boot_only])
          all_others   = tail boot_only ++ others
          groups =
            GHC.topSortModuleGraph True (mkModuleGraph all_others) Nothing

    pp_ms summary = text mod_str <> text (take (20 - length mod_str) (repeat ' '))
                       <+> (pp_imps empty (map snd (ms_imps summary)) $$
                            pp_imps (text "{-# SOURCE #-}") (map snd (ms_srcimps summary)))
        where
          mod_str = moduleNameString (moduleName (ms_mod summary))

    pp_imps :: SDoc -> [Located ModuleName] -> SDoc
    pp_imps _    [] = empty
    pp_imps what lms
        = case [m | L _ m <- lms, m `elem` cycle_mods] of
            [] -> empty
            ms -> what <+> text "imports" <+>
                                pprWithCommas ppr ms

#endif

-----------------------------------------------------------------
--
--              Flags
--
-----------------------------------------------------------------

depStartMarker, depEndMarker :: String
depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"
