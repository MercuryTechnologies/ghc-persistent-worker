{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.List (isPrefixOf, sortOn, uncons)
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith, nonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (traceId)
import GHC.Debug.Client
import GHC.Debug.Profile
import GHC.Debug.Retainers
import GHC.Debug.Types.Graph (heapGraphSize, ppClosure)

findThunks :: [ClosurePtr] -> DebugM [[ClosurePtr]]
findThunks roots =
  findRetainers (Just 10000) flt roots
  where
    flt = InfoSourceFilter \ SourceInformation {infoClosureType, infoType} ->
      not (null infoType)
      &&
      elem infoClosureType types

    types =
      [
        THUNK,
        THUNK_1_0,
        THUNK_0_1,
        THUNK_2_0,
        THUNK_1_1,
        THUNK_0_2,
        THUNK_STATIC,
        THUNK_SELECTOR
      ]

type Grouped = [NonEmpty (String, [(SizedClosureP, Maybe SourceInformation)])]

ignoreModules :: [String]
ignoreModules =
  [
  ]

ignoreModulesPrefix :: [String]
ignoreModulesPrefix =
  [
    "GHC.Internal",
    "Cmm"
  ]

sanitize :: (SizedClosureP, Maybe SourceInformation) -> Maybe (SizedClosureP, SourceInformation)
sanitize (scp@(DCS _ clp), (Just si@SourceInformation {infoModule}))
  | ThunkClosure {} <- clp = Just frame
  | elem infoModule ignoreModules
  = Nothing
  | any (flip isPrefixOf infoModule) ignoreModulesPrefix
  = Nothing
  | otherwise
  = case clp of
    ConstrClosure {} -> Just frame
    FunClosure {} -> Just frame
    TSOClosure {} -> Nothing
    MutVarClosure {} -> Nothing
    UnsupportedClosure {} -> Nothing
    APClosure {} -> Nothing
    APStackClosure {} -> Nothing
    -- _ -> Just frame
    _ -> Nothing
  where
    frame = (scp, si)
sanitize _ = Nothing

showClosure :: Show c => DebugClosure ccs (GenSrtPayload c) p ConstrDesc s c -> String
showClosure = \case
  ConstrClosure {..} -> name constrDesc
  c -> ppClosure (const show) 0 c

displayStack :: [(String, [(SizedClosureP, SourceInformation)])] -> IO ()
displayStack =
  traverse_ \ (_, stack) -> for_ stack \ (DCS size d, l) ->
    putStrLn $ tdisplay d l ++ " | " ++ show (getSize size)
  where
    tdisplay d SourceInformation {..} = case d of
      FunClosure {} -> showClosure d ++ " " ++ infoName
      ConstrClosure {} -> infoName ++ " :: " ++ infoType
      ThunkClosure {} -> showClosure d ++ " " ++ infoName ++ " :: " ++ infoType
      _ ->
        showClosure d ++  " <" ++ infoName ++ ":" ++ infoType ++ ":" ++ infoModule ++ ":" ++ infoPosition ++ "> "

displayGrouped :: Grouped -> IO ()
displayGrouped =
  traverse_ \case
    stacks@(stack@(_, (_, Just SourceInformation {infoType}) : _) :| _)
      | null infoType
      -> pure ()
      | otherwise
      -> do
        putStrLn "----------------------------------------"
        putStrLn ("Retainers of thunks of type '" ++ infoType ++ "' : " ++ show (length stacks))
        displayStack [mapMaybe sanitize <$> stack]
    stacks ->
      putStrLn ("ERROR: No info for stack of size " ++ show (length stacks))

thunks ::
  (
    [ClosurePtr] ->
    DebugM Grouped,
    (Grouped -> IO ())
  )
thunks =
  (prep, process)
  where
    prep roots = do
      rs <- findThunks roots
      rs' <- traverse (\c -> (maybe "unknown" (show . fst) (uncons c),) <$> addLocationToStack c) rs
      pure (dropWhile (\ a -> length a < 5) (sortOn length (groupAllWith byName rs')))

    byName (_, (_, Just SourceInformation {infoType}) : _) = infoType
    byName _ = "unnamed"

    process = displayGrouped

targets :: [ClosurePtr] -> DebugM [[ClosurePtr]]
targets roots =
  findRetainersOfConstructor (Just 2) roots "Bin"

retainers ::
  (
    [ClosurePtr] ->
    DebugM [(String, [(SizedClosureP, Maybe SourceInformation)])],
    ([(String, [(SizedClosureP, Maybe SourceInformation)])] -> IO ())
  )
retainers =
  (prep, process)
  where
    prep roots = do
      rs <- targets roots
      traverse (\c -> (maybe "unknown" (show . fst) (uncons c),) <$> (addLocationToStack c)) rs

    process = displayRetainerStack

ctypes :: ([ClosurePtr] -> DebugM CensusByClosureType, CensusByClosureType -> IO ())
ctypes =
  (prep, process)
  where
    prep = census2LevelClosureType
    process = writeCensusByClosureType "profile.txt"

heapGraph ::
  (
    [ClosurePtr] ->
    DebugM (HeapGraph Size),
    (HeapGraph Size -> IO ())
  )
heapGraph =
  (prep, process)
  where
    prep roots = multiBuildHeapGraph (Just 3) (fromMaybe undefined (nonEmpty roots))
    process hg = putStrLn (ppHeapGraph show hg)

debug :: Debuggee -> IO ()
debug e = do
  pause e
  (payload, count) <- run e do
    _ <- precacheBlocks
    roots <- gcRoots
    res <- prep roots
    pure (res, length roots)
  liftIO $ putStrLn ("Number of roots: " ++ show count)
  process payload
  where
    (prep, process) = current
    current = thunks

main :: IO ()
main = snapshotRun "snapshot-mwb-1" debug
