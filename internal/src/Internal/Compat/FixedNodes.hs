{-# LANGUAGE CPP, PatternSynonyms, ViewPatterns, FieldSelectors #-}

module Internal.Compat.FixedNodes where

import GHC (HscEnv, ModSummary, ModuleGraph, ModuleName)
import GHC.Driver.Errors.Types (DriverMessages)
import GHC.Driver.Make (downsweep)
import GHC.Unit.Module.Graph (ModNodeKeyWithUid, ModuleGraphNode (..))
import GHC.Unit.Module.Location (ModLocation)

#if defined(MWB)

import GHC.Unit.Module.Graph (mkModuleGraph)

#elif MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Types.Error (mkUnknownDiagnostic)

#endif

#if defined(FIXED_NODES)

import GHC.Unit.Module.Graph (ModuleNodeInfo (..))

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.Graph (ModuleNodeEdge)

type Edge = ModuleNodeEdge

#else

import GHC.Unit.Module.Graph (NodeKey)

type Edge = NodeKey

#endif

pattern CompileNode :: [Edge] -> ModSummary -> ModuleGraphNode
pattern CompileNode {deps, summary} <- ModuleNode !deps (ModuleNodeCompile !summary)

pattern FixedNode :: [Edge] -> ModNodeKeyWithUid -> ModLocation -> ModuleGraphNode
pattern FixedNode {deps, key, location} <- ModuleNode !deps (ModuleNodeFixed !key !location)

#else

import GHC.Unit.Module.Graph (NodeKey)

pattern CompileNode :: [NodeKey] -> ModSummary -> ModuleGraphNode
pattern CompileNode {deps, summary} <- ModuleNode !deps !summary

pattern FixedNode :: [NodeKey] -> ModNodeKeyWithUid -> ModLocation -> ModuleGraphNode
pattern FixedNode {deps, key, location} <- (const Nothing -> Just (deps, key, location))

#endif

downsweepCompat ::
  HscEnv ->
  [ModSummary] ->
  Maybe ModuleGraph ->
  [ModuleName] ->
  Bool ->
  IO ([DriverMessages], ModuleGraph)

#if defined(MWB)

downsweepCompat hsc_env summaries cache excl dup =
  fmap mkModuleGraph <$> downsweep hsc_env summaries cache excl dup

#elif MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

downsweepCompat hsc_env summaries _ =
  downsweep hsc_env mkUnknownDiagnostic Nothing summaries

#endif
