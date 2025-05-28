{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Internal.MakeFile.JSON
  ( writeJSONFile,
    JsonOutput (..),
    mkJsonOutput,
    updateJson,
    writeJsonOutput,
    DepJSON,
    DepNode (..),
    Dep (..),
    initDepJSON,
    updateDepJSON,
  )
where

import Data.Foldable (traverse_)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import GHC.Data.FastString (unpackFS)
import GHC.Generics (Generic, Generically (Generically))
import GHC.Prelude
import GHC.Unit
import GHC.Utils.Json
import GHC.Utils.Misc
import GHC.Utils.Outputable
import System.FilePath (normalise)

#if !defined(MWB) && !defined(TCR)
import GHC.Data.FastString (lexicalCompareFS)

instance Ord PackageId where
    PackageId p1 `compare` PackageId p2 = p1 `lexicalCompareFS` p2

#endif

--------------------------------------------------------------------------------
-- Output helpers
--------------------------------------------------------------------------------

writeJSONFile :: ToJson a => a -> FilePath -> IO ()
writeJSONFile doc p = do
  withAtomicRename p
    $ \tmp -> writeFile tmp $ showSDocUnsafe $ renderJSON $ json doc

--------------------------------------------------------------------------------
-- Output interface for json dumps
--------------------------------------------------------------------------------

-- | Resources for a json dump option, used in "GHC.Driver.MakeFile".
-- The flag @-dep-json@ add an additional output target for dependency
-- diagnostics.
data JsonOutput a =
  JsonOutput {
    -- | This ref is updated in @processDeps@ incrementally, using a
    -- flag-specific type.
    json_ref :: IORef a,

    -- | The output file path specified as argument to the flag.
    json_path :: FilePath
  }

-- | Allocate an 'IORef' with the given function if the 'FilePath' is 'Just',
-- indicating that the userspecified @-*-json@.
mkJsonOutput ::
  IO (IORef a) ->
  Maybe FilePath ->
  IO (Maybe (JsonOutput a))
mkJsonOutput mk_ref =
  traverse $ \ json_path -> do
    json_ref <- mk_ref
    pure JsonOutput {json_ref, json_path}

-- | Update the dump data in 'json_ref' if the output target is present.
updateJson :: Maybe (JsonOutput a) -> (a -> a) -> IO ()
updateJson out f = traverse_ (\ JsonOutput {json_ref} -> modifyIORef' json_ref f) out

-- | Write a json object to the flag-dependent file if the output target is
-- present.
writeJsonOutput ::
  ToJson a =>
  Maybe (JsonOutput a) ->
  IO ()
writeJsonOutput =
  traverse_ $ \ JsonOutput {json_ref, json_path} -> do
    payload <- readIORef json_ref
    writeJSONFile payload json_path

--------------------------------------------------------------------------------
-- Types abstracting over json and Makefile
--------------------------------------------------------------------------------

data DepNode =
  DepNode {
    dn_mod :: Module,
    dn_src :: FilePath,
    dn_obj :: FilePath,
    dn_hi :: FilePath,
    dn_boot :: IsBootInterface,
    dn_options :: Set.Set String
  }

data Dep =
  DepHi {
    dep_mod :: Module,
    dep_path :: FilePath,
    dep_unit :: Maybe UnitInfo,
    dep_local :: Bool,
    dep_boot :: IsBootInterface
  }
  |
  DepCpp {
    dep_path :: FilePath
  }

--------------------------------------------------------------------------------
-- Payload for -dep-json
--------------------------------------------------------------------------------

newtype PackageDeps =
  PackageDeps (Map.Map (String, UnitId, PackageId) (Set.Set ModuleName))
  deriving newtype (Monoid)

instance Semigroup PackageDeps where
  PackageDeps l <> PackageDeps r = PackageDeps (Map.unionWith (Semigroup.<>) l r)

data Deps =
  Deps {
    sources :: Set.Set FilePath,
    modules :: (Set.Set ModuleName, Set.Set ModuleName),
    packages :: PackageDeps,
    cpp :: Set.Set FilePath,
    options :: Set.Set String,
    preprocessor :: Maybe FilePath
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically Deps)

data ModuleDeps =
  ModuleDeps {
    source :: Deps,
    boot :: Maybe Deps
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically ModuleDeps)

newtype DepJSON = DepJSON (Map.Map ModuleName ModuleDeps)

instance ToJson DepJSON where
  json (DepJSON m) =
    JSObject [
      (moduleNameString target, module_deps md)
      | (target, md) <- Map.toList m
    ]
    where
      module_deps ModuleDeps {source, boot} =
        JSObject (("boot", maybe JSNull (JSObject . deps) boot) : deps source)

      deps Deps {packages = PackageDeps packages, ..} =
        [
          ("sources", array sources normalise),
          ("modules", array (fst modules) moduleNameString),
          ("modules-boot", array (snd modules) moduleNameString),
          ("packages",
            JSArray [
              package name unit_id package_id mods |
              ((name, unit_id, package_id), mods) <- Map.toList packages
            ]
          ),
          ("cpp", array cpp id),
          ("options", array options id),
          ("preprocessor", maybe JSNull JSString preprocessor)
        ]

      package name unit_id (PackageId package_id) mods =
        JSObject [
          ("id", JSString (unitIdString unit_id)),
          ("name", JSString name),
          ("package-id", JSString (unpackFS package_id)),
          ("modules", array mods moduleNameString)
        ]

      array values render = JSArray (fmap (JSString . render) (Set.toList values))

initDepJSON :: IO (IORef DepJSON)
initDepJSON = newIORef $ DepJSON Map.empty

insertDepJSON :: ModuleName -> IsBootInterface -> Deps -> DepJSON -> DepJSON
insertDepJSON target is_boot dep (DepJSON m0) =
  DepJSON $ Map.insertWith (Semigroup.<>) target new m0
  where
    new
      | IsBoot <- is_boot = mempty {boot = Just dep}
      | otherwise = mempty {source = dep}

updateDepJSON :: Bool -> Maybe FilePath -> DepNode -> [Dep] -> DepJSON -> DepJSON
updateDepJSON include_pkgs preprocessor DepNode {..} deps =
  insertDepJSON (moduleName dn_mod) dn_boot payload
  where
    payload = node_data Semigroup.<> foldMap dep deps

    node_data =
      mempty {
        sources = Set.singleton dn_src,
        preprocessor,
        options = dn_options
      }

    dep = \case
      DepHi {dep_mod, dep_local, dep_unit, dep_boot}
        | dep_local
        , let set = Set.singleton (moduleName dep_mod)
              value | IsBoot <- dep_boot = (Set.empty, set)
                    | otherwise = (set, Set.empty)
        -> mempty {modules = value}

        | include_pkgs
        , Just unit <- dep_unit
        , let PackageName nameFS = unitPackageName unit
              name = unpackFS nameFS
              withLibName (PackageName c) = name ++ ":" ++ unpackFS c
              lname = maybe name withLibName (unitComponentName unit)
              key = (lname, unitId unit, unitPackageId unit)
        -> mempty {packages = PackageDeps (Map.singleton key (Set.singleton (moduleName dep_mod)))}

        | otherwise
        -> mempty

      DepCpp {dep_path} ->
        mempty {cpp = Set.singleton dep_path}
