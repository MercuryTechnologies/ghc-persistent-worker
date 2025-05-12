{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_, fold, toList)
import Data.List (isPrefixOf, sortOn, uncons)
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith, nonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (traceId)
import GHC.Debug.Client
import GHC.Debug.Profile
import GHC.Debug.Retainers
import GHC.Debug.Types.Graph (heapGraphSize, ppClosure)
import Data.Map.Strict ((!?))
import GHC.Debug.Client.Monad (unsafeLiftIO)
import Data.Functor ((<&>))

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
    ConstrClosure {constrDesc}
      | constrDesc.name == ":"
      -> Nothing
      | constrDesc.name == "[]"
      -> Nothing
      | otherwise
      -> Just frame
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
  traverse_ \ (_, stack) -> do
    putStrLn "---"
    for_ stack \ (DCS size d, l) ->
      putStrLn $ tdisplay d l ++ " | " ++ show (getSize size)
  where
    tdisplay d SourceInformation {..} = case d of
      FunClosure {} -> showClosure d ++ " " ++ infoName
      ConstrClosure {} -> infoName ++ " :: " ++ infoType
      ThunkClosure {} -> showClosure d ++ " " ++ infoName ++ " :: " ++ infoType
      _ ->
        showClosure d ++  " <" ++ infoName ++ ":" ++ infoType ++ ":" ++ infoModule ++ ":" ++ infoPosition ++ "> "

displayGrouped :: Maybe Int -> Grouped -> IO ()
displayGrouped limit =
  traverse_ \case
    stacks@((_, (_, Just SourceInformation {infoType}) : _) :| _)
      | null infoType
      -> pure ()
      | otherwise
      -> do
        putStrLn "----------------------------------------"
        putStrLn ("Retainers of thunks of type '" ++ infoType ++ "' : " ++ show (length stacks))
        displayStack (fmap (mapMaybe sanitize) <$> maybe id take limit (toList stacks))
    stacks ->
      putStrLn ("ERROR: No info for stack of size " ++ show (length stacks))

addLocs ::
  [[ClosurePtr]] ->
  DebugM [(String, [(SizedClosureP, Maybe SourceInformation)])]
addLocs =
  traverse \ c -> (maybe "unknown" (show . fst) (uncons c),) <$> addLocationToStack c

data FromTo =
  FromTo {
    mldf :: [(String, [(SizedClosureP, Maybe SourceInformation)])]
  }
  deriving stock (Eq, Show)

displayFromTo :: Maybe Int -> FromTo -> IO ()
displayFromTo limit (FromTo (nonEmpty -> Just stacks)) =
  displayGrouped limit [stacks]
displayFromTo _ _ =
  putStrLn "No results."

findFromTo :: String -> String -> [ClosurePtr] -> DebugM [[ClosurePtr]]
findFromTo from to roots = do
  fromResult <- closureCensusBy matchFrom roots
  let froms = fold (fromResult !? ())
  unsafeLiftIO $ putStrLn ("Found " ++ show (length froms) ++ " closures of " ++ from)
  findRetainers (Just 100) matchTo froms
  where
    matchFrom ptr = \case
      DCS _ c@ConstrClosure {} -> matchFromType ptr c
      DCS _ c@ThunkClosure {} -> matchFromType ptr c
      _ -> pure Nothing

    matchFromType ptr closure = do
      getSourceInfo (tableId (info closure)) <&> \ mi -> mi >>= \ SourceInformation {infoType} ->
        if infoType == from
        then Just ((), [ptr])
        else Nothing

    matchTo =
      InfoSourceFilter \ SourceInformation {infoType, infoName} ->
        infoType == to || infoName == to


fromTo ::
  Maybe Int ->
  String ->
  String ->
  (
    [ClosurePtr] ->
    DebugM FromTo,
    (FromTo -> IO ())
  )
fromTo showLimit from to =
  (prep, process)
  where
    prep roots = do
      rs <- findFromTo from to roots
      rs' <- addLocs rs
      pure (FromTo rs')

    process = displayFromTo showLimit

modLocationDynFlags ::
  (
    [ClosurePtr] ->
    DebugM FromTo,
    (FromTo -> IO ())
  )
modLocationDynFlags =
  fromTo (Just 1) "ModLocation" "DynFlags"

aToB ::
  (
    [ClosurePtr] ->
    DebugM FromTo,
    (FromTo -> IO ())
  )
aToB =
  fromTo Nothing "TestA" "TestB"

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
      rs' <- addLocs rs
      pure (dropWhile (\ a -> length a < 0) (sortOn length (groupAllWith byName rs')))

    byName (_, (_, Just SourceInformation {infoType}) : _) = infoType
    byName _ = "unnamed"

    process = displayGrouped (Just 1)

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
      addLocs rs

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
    current = aToB

main :: IO ()
main = snapshotRun "snapshot-synth-1" debug
