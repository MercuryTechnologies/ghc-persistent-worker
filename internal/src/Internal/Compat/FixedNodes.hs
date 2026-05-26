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

support_FixedNodes :: Bool
support_FixedNodes = True

pattern CompileNode :: [Edge] -> ModSummary -> ModuleGraphNode
pattern CompileNode {depsCompile, summary} <- ModuleNode !depsCompile (ModuleNodeCompile !summary)

pattern FixedNode :: [Edge] -> ModNodeKeyWithUid -> ModLocation -> ModuleGraphNode
pattern FixedNode {depsFixed, key, location} <- ModuleNode !depsFixed (ModuleNodeFixed !key !location)

#else

import GHC.Unit.Module.Graph (NodeKey)

support_FixedNodes :: Bool
support_FixedNodes = False

pattern CompileNode :: [NodeKey] -> ModSummary -> ModuleGraphNode
pattern CompileNode {depsCompile, summary} <- ModuleNode !depsCompile !summary

pattern FixedNode :: [NodeKey] -> ModNodeKeyWithUid -> ModLocation -> ModuleGraphNode
pattern FixedNode {depsFixed, key, location} <- (const Nothing -> Just (depsFixed, key, location))

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
