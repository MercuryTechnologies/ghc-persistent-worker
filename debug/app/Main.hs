{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold, for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, sortBy, sortOn, uncons)
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith, nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Debug.Trace (traceId)
import GHC.Debug.Client
import GHC.Debug.Client.Monad (unsafeLiftIO)
import GHC.Debug.Profile
import GHC.Debug.Retainers
import GHC.Debug.Strings (stringAnalysis)
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
      ] :: [ClosureType]

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
    APStackClosure {} -> Just frame
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
      putStrLn $ tdisplay d l ++ " | " ++ pos l ++ " | " ++ show (getSize size)
  where
    pos SourceInformation {..} = infoPosition
    tdisplay d SourceInformation {..} = case d of
      FunClosure {} -> showClosure d ++ " " ++ infoName
      ConstrClosure {} -> infoName ++ " :: " ++ infoType
      ThunkClosure {} -> showClosure d ++ " " ++ infoName ++ " :: " ++ infoType
      _ ->
        showClosure d ++  " <" ++ infoName ++ ":" ++ infoType ++ ":" ++ infoModule ++ ":" ++ infoPosition ++ "> "

nonNullStack :: (String, [(SizedClosureP, SourceInformation)]) -> Bool
nonNullStack (_, s) = not (null s)

displayGrouped :: Bool -> Maybe Int -> Grouped -> IO ()
displayGrouped sanitized limit =
  traverse_ display . fmap (filter nonNullStack . fmap (fmap sanitizeStack) . toList)
  where
    display stacks = do
      let tpe = fromMaybe "<unknown>" (listToMaybe (mapMaybe clInfoType stacks))
      putStrLn "----------------------------------------"
      putStrLn ("Retainers of thunks of type '" ++ tpe ++ "' : " ++ show (length stacks))
      displayStack (maybe id take limit stacks)

    clInfoType = \case
      (_, (_, SourceInformation {infoType}) : _) -> Just infoType
      _ -> Nothing

    sanitizeStack :: [(SizedClosureP, Maybe SourceInformation)] -> [(SizedClosureP, SourceInformation)]
    sanitizeStack
      | sanitized = \case
        [] -> []
        (scp, (Just si)) : t -> (scp, si) : mapMaybe sanitize t
        _ : t -> sanitizeStack t
      | otherwise = mapMaybe sequence

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
  displayGrouped True limit [stacks]
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

modLocationToDynFlags ::
  (
    [ClosurePtr] ->
    DebugM FromTo,
    (FromTo -> IO ())
  )
modLocationToDynFlags =
  fromTo (Just 5) "ModLocation" "DynFlags"

aToB ::
  (
    [ClosurePtr] ->
    DebugM FromTo,
    (FromTo -> IO ())
  )
aToB =
  fromTo Nothing "TestA" "TestB"

byName :: (a1, [(a2, Maybe SourceInformation)]) -> String
byName (_, (_, Just SourceInformation {infoType}) : _) = infoType
byName _ = "unnamed"

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
      pure (dropWhile (\ a -> length a <= 0) (sortOn length (groupAllWith byName rs')))

    process = displayGrouped True (Just 1)

retainers ::
  (
    [ClosurePtr] ->
    DebugM Grouped,
    (Grouped -> IO ())
  )
retainers =
  (prep, process)
  where
    prep roots = do
      rs <- targets roots
      rs' <- addLocs rs
      pure (sortOn length (groupAllWith byName rs'))

    targets :: [ClosurePtr] -> DebugM [[ClosurePtr]]
    targets roots =
      findRetainersOfConstructor (Just num) roots ":"

    process = displayGrouped True (Just num)

    num = 20

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

strings ::
  (
    [ClosurePtr] ->
    DebugM Grouped,
    (Grouped -> IO ())
  )
strings =
  (prep, process)
  where
    prep roots = do
      rs <- targets roots
      rs' <- addLocs rs
      pure (sortOn length (groupAllWith byName rs'))

    targets roots = do
      strs <- stringAnalysis roots
      when False do
        showFreq strs
      let clps = foldMap Set.toList (strs !? target)
      findRetainersOf (Just num) roots clps

    showFreq strs = do
      let byCount = reverse [(k, v) | (k, v) <- (sortBy (comparing snd) (fmap Set.size <$> Map.toList strs))]
      for_ (take 1000 byCount) \ (s, count) -> (unsafeLiftIO (putStrLn (s ++ ": " ++ show count)))

    target :: String
    target = "buck-out/v2/gen/toolchains/904931f735703749-8d78dcf3e0e36516/__haskell__/__action___0__/tf-random/out.link/lib/ghc-9.10.1/lib/package.conf.d"

    process x =
      displayGrouped True (Just num) x

    num = 20

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
    current = strings

main :: IO ()
main = snapshotRun "mwb-1" debug
