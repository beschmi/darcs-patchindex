-- Copyright (C) 2002-2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}


module Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , viewChanges
    , withSelectedPatchFromRepo
    , filterOutConflicts
    , runSelection
    , selectionContextPrim
    , selectionContext
    ) where

import Data.List ( intersperse )
import Data.Maybe ( isJust, isNothing )
import Data.Char ( toUpper )
import Control.Monad ( when, (>=>), unless )
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ReaderT, Reader, asks, runReader, runReaderT )
import Control.Monad.State ( StateT, modify, gets, runStateT, execStateT )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import Darcs.UI.External ( editText )
import Darcs.UI.PrintPatch
    ( showFriendly
    , printFriendly
    , printPatch
    , printPatchPager
    )
import English ( Noun(..), englishNum  )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, n2pia )
import Darcs.Repository ( Repository, readRepo, unrecordedChanges )
import Darcs.Repository.Flags( UseIndex(..), ScanKnown(..) )
import Darcs.Patch ( RepoPatch, Patchy, PrimPatch, summary,
                     invert, listTouchedFiles,
                     commuteFLorComplain, fromPrims, anonymous )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch.Apply( ApplyState )
import qualified Darcs.Patch ( thing, things )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.Split ( Splitter(..) )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), (:||:)(..),
                       (+>+), lengthFL, mapFL_FL,
                       spanFL, spanFL_M, reverseFL, (+<+), mapFL, filterFL )
import Darcs.Witnesses.WZipper( FZipper(..), left, right
                              , rightmost
                              , toEnd, toStart)
import Darcs.Patch.Choices ( PatchChoices, patchChoices,
                             patchChoicesTpsSub,
                             forceFirst, forceLast, makeUncertain, tag,
                      getChoices, refineChoices,
                      separateFirstFromMiddleLast,
                      patchSlot',
                      selectAllMiddles,
                      forceMatchingLast,
                      forceMatchingFirst, makeEverythingLater,
                      makeEverythingSooner,
                      TaggedPatch, tpPatch, Slot(..),
                      substitute
                    )
import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.CommuteFn ( commuterIdRL )
import Darcs.Patch.Permutations ( partitionConflictingFL )
import qualified Darcs.Patch.TouchesFiles as TouchesFiles
import Darcs.Patch.Match ( haveNonrangeMatch, matchAPatch, matchAPatchread )
import Darcs.UI.Flags ( DarcsFlag( Summary, DontGrabDeps, Verbose, DontPromptForDependencies
                              , SkipConflicts )
                   , isInteractive
    , toMatchFlags )
import Darcs.Witnesses.Sealed ( FlippedSeal(..), flipSeal, seal2, unseal2, Sealed(..), Sealed2(..) )
import Darcs.Utils ( askUser, promptChar, PromptConfig(..) )
import Printer ( prefix, putDocLn )
import Storage.Hashed.Tree( Tree )

-- | When asking about patches, we either ask about them in
-- oldest-first or newest first (with respect to the current ordering
-- of the repository), and we either want an initial segment or a
-- final segment of the poset of patches.
--
-- @First@: ask for an initial
-- segment, first patches first (default for all pull-like commands)
--
-- @FirstReversed@: ask for an initial segment, last patches first
-- (used to ask about dependencies in record, and for pull-like
-- commands with the @--reverse@ flag).
--
-- @LastReversed@: ask for a final segment, last patches first. (default
-- for unpull-like commands, except for selecting *primitive* patches in
-- rollback)
--
-- @Last@: ask for a final segment, first patches first. (used for selecting
-- primitive patches in rollback, and for unpull-like commands with the
-- @--reverse@ flag
data WhichChanges = Last | LastReversed | First | FirstReversed deriving (Eq, Show)

-- | A @WhichChanges@ is backwards if the order in which patches are presented
-- is the opposite of the order of dependencies for that operation.
backward :: WhichChanges -> Bool
backward w = w == Last || w == FirstReversed

-- | The type of the function we use to filter patches when @--match@ is
-- given.
type MatchCriterion p = WhichChanges -> [DarcsFlag] -> Sealed2 p -> Bool

-- | A @PatchSelectionContext@ contains all the static settings for selecting
-- patches. See "PatchSelectionM"
data PatchSelectionContext p = PSC { opts :: [DarcsFlag]
                                   , splitter :: Maybe (Splitter p)
                                   , files :: Maybe [FilePath]
                                   , matchCriterion :: MatchCriterion p
                                   , jobname :: String
                                   , pristine :: Maybe (Tree IO)}

-- | A 'PatchSelectionContext' for selecting 'Prim' patches.
selectionContextPrim ::  PrimPatch prim => String -> [DarcsFlag] -> Maybe (Splitter prim)
                 -> Maybe [FilePath] -> Maybe (Tree IO) -> PatchSelectionContext prim
selectionContextPrim jn o spl fs p =
 PSC { opts = o
     , splitter = spl
     , files = fs
     , matchCriterion = triv
     , jobname = jn
     , pristine = p }

-- | A 'PatchSelectionContext' for selecting full patches ('PatchInfoAnd' patches)
selectionContext :: (RepoPatch p) => String -> [DarcsFlag] -> Maybe (Splitter (PatchInfoAnd p))
                 -> Maybe [FilePath] -> PatchSelectionContext (PatchInfoAnd p)
selectionContext jn o spl fs =
 PSC { opts = o
     , splitter = spl
     , files = fs
     , matchCriterion = iswanted
     , jobname = jn
     , pristine = Nothing }

-- | The dynamic parameters for interactive selection of patches.
data InteractiveSelectionContext p wX wY = ISC { total :: Int
                                                  -- ^ total number of patches
                                                , current :: Int
                                                  -- ^ number of already-seen patches
                                                , tps :: FZipper (TaggedPatch p) wX wY
                                                  -- ^ the patches we offer
                                                , choices :: PatchChoices p wX wY
                                                  -- ^ the user's choices
                                                }

type PatchSelectionM p a = ReaderT (PatchSelectionContext p) a

type InteractiveSelectionM p wX wY a =
    StateT (InteractiveSelectionContext p wX wY)
           (PatchSelectionM p IO) a

type PatchSelection p wX wY =
        PatchSelectionM p IO ((FL p :> FL p) wX wY)

-- Common match criteria

-- | For commands without @--match@, 'triv' matches all patches
triv :: MatchCriterion p
triv = \ _ _ _ -> True

-- | 'iswanted' selects patches according to the @--match@ flag in
-- opts'
iswanted :: RepoPatch p => MatchCriterion (PatchInfoAnd p)
iswanted whch opts' =
    unseal2 (iw whch)
        where
          matchFlags = toMatchFlags opts'
          iw First = matchAPatch matchFlags . hopefully
          iw Last = matchAPatch matchFlags . hopefully
          iw LastReversed = matchAPatch matchFlags . hopefully . invert
          iw FirstReversed = matchAPatch matchFlags . hopefully . invert

liftR :: Monad m => Reader r a -> ReaderT r m a
liftR = asks . runReader

-- | runs a 'PatchSelection' action in the given 'PatchSelectionContext'.
runSelection :: (Patchy p) => PatchSelection p wX wY -> PatchSelectionContext p
             -> IO ((FL p :> FL p) wX wY)
runSelection = runReaderT

-- | Select patches from a @FL@.
selectChanges :: forall p wX wY . (Patchy p, PatchInspect p, ShowPatch p, ApplyState p ~ Tree) =>
                WhichChanges -> FL p wX wY
                             -> PatchSelection p wX wY
selectChanges First = sc1 First
selectChanges Last = sc1 Last
selectChanges FirstReversed = return . invert
                              >=> sc1 FirstReversed
                              >=> return . invertC

selectChanges LastReversed = return . invert
                             >=> sc1 LastReversed
                             >=> return . invertC

sc1 :: forall p wX wY . (Patchy p, PatchInspect p, ShowPatch p, ApplyState p ~ Tree) =>
                WhichChanges -> FL p wX wY
                             -> PatchSelection p wX wY
sc1 whch =
    ((liftR . patchesToConsider whch)
     >=> realSelectChanges whch
     >=> return . selectedPatches whch
     >=> (liftR . canonizeAfterSplitter))

-- | inverses the choices that have been made
invertC :: (Patchy p) => (FL p :> FL p) wX wY -> (FL p :> FL p) wY wX
invertC (a :> b) = (invert b) :> (invert a)

-- | Shows the patch that is actually being selected the way the user
-- should see it.
repr :: (Patchy p) => WhichChanges -> Sealed2 p -> Sealed2 p
repr First (Sealed2 p) = Sealed2 p
repr LastReversed (Sealed2 p) = Sealed2 (invert p)
repr Last (Sealed2 p) = Sealed2 p
repr FirstReversed (Sealed2 p) = Sealed2 (invert p)

-- | The equivalent of 'selectChanges' for the @darcs changes@ command
viewChanges :: (Patchy p, ShowPatch p, ApplyState p ~ Tree) => [DarcsFlag] -> [Sealed2 p] -> IO ()
viewChanges opts' ps = textView opts' Nothing 0 [] ps

-- | The type of the answers to a "shall I [wiggle] that [foo]?" question
-- They are found in a [[KeyPress]] bunch, each list representing a set of
-- answers which belong together
data KeyPress = KeyPress { kp     :: Char
                           , kpHelp :: String }

-- | Generates the help for a set of basic and advanced 'KeyPress' groups.
helpFor :: String -> [[KeyPress]] -> [[KeyPress]] -> String
helpFor jn basicKeypresses advancedKeyPresses =
  unlines $ [ "How to use "++jn++":" ]
            ++ (concat $ intersperse [""] $ map (map help) keypresses)
            ++ [ ""
               , "?: show this help"
               , ""
               , "<Space>: accept the current default (which is capitalized)"
               ]
  where help i = kp i:(": "++kpHelp i)
        keypresses = basicKeypresses ++ advancedKeyPresses

-- | The keys used by a list of 'keyPress' groups.
keysFor :: [[KeyPress]] -> [Char]
keysFor = concatMap (map kp)

-- | The function for selecting a patch to amend record. Read at your own risks.
withSelectedPatchFromRepo ::
       forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
    => String   -- name of calling command (always "amend" as of now)
    -> Repository p wR wU wT
    -> [DarcsFlag]
    -> (forall wA . (FL (PatchInfoAnd p) :> PatchInfoAnd p) wA wR -> IO ())
    -> IO ()
withSelectedPatchFromRepo jn repository o job = do
    patchSet <- readRepo repository
    sp <- wspfr jn (matchAPatchread $ toMatchFlags o) (newset2RL patchSet) NilFL
    case sp of
        Just (FlippedSeal (skipped :> selected')) -> job (skipped :> selected')
        Nothing ->
            putStrLn $ "Cancelling " ++ jn ++ " since no patch was selected."

-- | This ensures that the selected patch commutes freely with the skipped
-- patches, including pending and also that the skipped sequences has an
-- ending context that matches the recorded state, z, of the repository.
wspfr :: (RepoPatch p, ApplyState p ~ Tree)
      => String
      -> (forall wA wB . (PatchInfoAnd p) wA wB -> Bool)
      -> RL (PatchInfoAnd p) wX wY
      -> FL (PatchInfoAnd p) wY wU
      -> IO (Maybe (FlippedSeal (FL (PatchInfoAnd p) :> (PatchInfoAnd p)) wU))
wspfr _ _ NilRL _ = return Nothing
wspfr jn matches remaining@(p:<:pps) skipped
    | not $ matches p = wspfr jn matches pps (p:>:skipped)
    | otherwise =
    case commuteFLorComplain (p :> skipped) of
    Left _  -> do putStrLn "\nSkipping depended-upon patch:"
                  printFriendly Nothing [] p
                  wspfr jn matches pps (p:>:skipped)
    Right (skipped' :> p') -> do
        printFriendly Nothing [] p
        yorn <- promptChar $
                    PromptConfig { pPrompt = prompt'
                                 , pBasicCharacters = keysFor basicOptions
                                 , pAdvancedCharacters = keysFor advancedOptions
                                 , pDefault = Just 'n'
                                 , pHelp = "?h" }
        case yorn of
            'y' -> return $ Just $ flipSeal $ skipped' :> p'
            'n' -> nextPatch
            'j' -> nextPatch
            'k' -> case skipped of -- look at the previous skipped state
                     NilFL -> repeatThis
                     (prev :>: skipped'') -> wspfr jn matches (prev :<: remaining) skipped''
            'v' -> printPatch p >> repeatThis
            'p' -> printPatchPager p >> repeatThis
            'x' -> do putDocLn $ prefix "    " $ summary p
                      repeatThis
            'q' -> do putStrLn $ jnCapital ++ " cancelled."
                      exitWith $ ExitSuccess
            _   -> do putStrLn $ helpFor jn basicOptions advancedOptions
                      repeatThis
  where jnCapital = (toUpper $ head jn) : tail jn
        repeatThis = wspfr jn matches (p:<:pps) skipped
        prompt' = "Shall I " ++ jn ++ " this patch?"
        nextPatch = wspfr jn matches pps (p:>:skipped)
        basicOptions =
                    [[ KeyPress 'y' (jn ++ " this patch")
                     , KeyPress 'n' ("don't " ++ jn ++ " it")
                     , KeyPress 'j' "skip to next patch"
                     , KeyPress 'k' "back up to previous patch"
                    ]]
        advancedOptions =
                    [[ KeyPress 'v' "view this patch in full"
                     , KeyPress 'p' "view this patch in full with pager"
                     , KeyPress 'x' "view a summary of this patch"
                     , KeyPress 'q' ("cancel " ++ jn)
                    ]]

-- After selecting with a splitter, the results may not be canonical
canonizeAfterSplitter :: (FL p :> FL p) wX wY -> Reader (PatchSelectionContext p) ((FL p :> FL p) wX wY)
canonizeAfterSplitter (x :> y) =
    do mspl <- asks splitter
       let canonizeIfNeeded = maybe id canonizeSplit mspl
       return $ canonizeIfNeeded x :> canonizeIfNeeded y

realSelectChanges :: forall p wX wY. (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree)
                                => WhichChanges
                                -> PatchChoices p wX wY
                                -> PatchSelectionM p IO (PatchChoices p wX wY)
realSelectChanges whch autoChoices =
    do
      o <- asks opts
      if not $ isInteractive o
       then return $ promote autoChoices
       else refineChoices (textSelect whch) autoChoices
    where forward = not $ backward whch
          promote = if forward
                    then makeEverythingSooner
                    else makeEverythingLater

-- | When using @--match@, remove unmatched patches not depended upon by matched
-- patches.
deselectUnwanted :: forall p wX wY . Patchy p => WhichChanges ->
                     PatchChoices p wX wY ->
                     Reader (PatchSelectionContext p) (PatchChoices p wX wY)
deselectUnwanted whichch pc =
    do
      o <- asks opts
      c <- asks matchCriterion
      let iswanted_ = c whichch o . seal2 . tpPatch
          select = if forward
                   then forceMatchingFirst iswanted_
                   else forceMatchingLast iswanted_
          deselect = if forward
                     then forceMatchingLast (not . iswanted_)
                     else forceMatchingFirst (not . iswanted_)
      return $
        if haveNonrangeMatch $ toMatchFlags o
            then if DontGrabDeps `elem` o
                 then deselect pc
                 else demote $ select pc
            else pc
    where
      forward = not $ backward whichch
      demote = if forward
               then makeEverythingLater
               else makeEverythingSooner

-- | Selects the patches matching the match criterion, and puts them first or last
-- according to whch, while respecting any dependencies.
patchesToConsider :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                     => WhichChanges
                     -> FL p wX wY
                     -> Reader (PatchSelectionContext p) (PatchChoices p wX wY)
patchesToConsider whch ps =
    do
      fs <- asks files
      o <- asks opts
      let deselectNotTouching =
              case whch of
                First -> TouchesFiles.deselectNotTouching
                Last -> TouchesFiles.selectNotTouching
                FirstReversed -> TouchesFiles.selectNotTouching
                LastReversed -> TouchesFiles.deselectNotTouching
          everything = patchChoices ps
      if isNothing fs && not (haveNonrangeMatch $ toMatchFlags o)
         then return everything
         else do notUnwanted <- deselectUnwanted whch everything
                 return $ deselectNotTouching fs notUnwanted

-- | Returns the results of a patch selection user interaction
selectedPatches :: Patchy p => WhichChanges -> PatchChoices p wY wZ
                      -> (FL p :> FL p) wY wZ
selectedPatches Last pc =
  case getChoices pc of
   fc :> mc :> lc -> mapFL_FL tpPatch (fc +>+ mc) :> mapFL_FL tpPatch lc

selectedPatches First pc =
  case separateFirstFromMiddleLast pc of
  xs :> ys -> mapFL_FL tpPatch xs :> mapFL_FL tpPatch ys

selectedPatches LastReversed pc =
  case separateFirstFromMiddleLast pc of
  xs :> ys -> mapFL_FL tpPatch (xs) :> (mapFL_FL tpPatch (ys))

selectedPatches FirstReversed pc =
  case getChoices pc of
  fc :> mc :> lc -> (mapFL_FL tpPatch (fc +>+ mc)) :> (mapFL_FL tpPatch lc)

-- | Runs a function on the underlying @PatchChoices@ object
liftChoices :: forall p a wX wY . Patchy p =>
               StateT (PatchChoices p wX wY) Identity a
                   -> InteractiveSelectionM p wX wY a
liftChoices act = do
  ch <- gets choices
  let (result, _) = runIdentity $ runStateT act ch
  modify $ \isc -> isc {choices = ch} -- Should this be ch or the result of runState?
  return result

-- | @justDone n@ notes that @n@ patches have just been processed
justDone :: Patchy p => Int -> InteractiveSelectionM p wX wY ()
justDone n = modify $ \isc -> isc{ current = current isc + n}

-- | The actual interactive selection process.
textSelect :: forall p wX wY . (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree) => WhichChanges ->
             FL (TaggedPatch p) wX wY -> PatchChoices p wX wY
             -> PatchSelectionM p IO (PatchChoices p wX wY)
textSelect whch tps' pcs = do
    userSelection <- execStateT (skipMundane whch >>
                                 showCur whch >>
                                 textSelectIfAny) $
                     ISC { total = lengthFL tps'
                         , current = 0
                         , tps = FZipper NilRL tps'
                         , choices = pcs }
    return $ choices userSelection
    where textSelectIfAny = do
            z <- gets tps
            if rightmost z
              then return ()
              else textSelect' whch

textSelect' :: (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree) => WhichChanges ->
              InteractiveSelectionM p wX wY ()
textSelect' whch = do
  z <- gets tps
  done <- if (not $ rightmost z)
           then textSelectOne whch
           else lastQuestion whch
  unless done $ textSelect' whch

optionsBasic :: [Char] -> [Char] -> [KeyPress]
optionsBasic jn aThing =
    [ KeyPress 'y' (jn++" this "++aThing)
    , KeyPress 'n' ("don't "++jn++" it")
    , KeyPress 'w' ("wait and decide later, defaulting to no") ]

optionsFile :: [Char] -> [KeyPress]
optionsFile jn =
    [ KeyPress 's' ("don't "++jn++" the rest of the changes to this file")
    , KeyPress 'f' (jn++" the rest of the changes to this file") ]

optionsView :: String -> String -> [KeyPress]
optionsView aThing someThings =
    [ KeyPress 'v' ("view this "++aThing++" in full")
    , KeyPress 'p' ("view this "++aThing++" in full with pager")
    , KeyPress 'l' ("list all selected "++someThings) ]

optionsSummary :: String -> [KeyPress]
optionsSummary aThing =
    [ KeyPress 'x' ("view a summary of this "++aThing) ]

optionsQuit :: String -> String -> [KeyPress]
optionsQuit jn someThings =
    [ KeyPress 'd' (jn++" selected "++someThings++", skipping all the remaining "++someThings)
    , KeyPress 'a' (jn++" all the remaining "++someThings)
    , KeyPress 'q' ("cancel "++jn) ]

optionsNav :: String -> Bool -> [KeyPress]
optionsNav aThing isLast=
    (if not isLast
     then [ KeyPress 'j' ("skip to next "++ aThing)]
     else [])
    ++
    [ KeyPress 'k' ("back up to previous "++ aThing)
    , KeyPress 'g' ("start over from the first "++aThing)]

optionsSplit :: Maybe (Splitter a) -> String -> [KeyPress]
optionsSplit split aThing
    | Just _ <- split
             = [ KeyPress 'e' ("interactively edit this "++ aThing) ]
    | otherwise = []

optionsLast :: String -> String -> ([[KeyPress]], [[KeyPress]])
optionsLast jn aThing =
  (optionsNav aThing True:
   [[ KeyPress 'y' ("confirm this operation")
    , KeyPress 'q' ("cancel " ++ jn) ]
    , [ KeyPress 'l' ("list all selected") ]
   ]
  ,[[KeyPress 'a' "confirm this operation"
    , KeyPress 'd' "confirm this operation"]])

options :: forall p wX wY . (Patchy p, ShowPatch p) => Bool ->
           InteractiveSelectionM p wX wY ([[KeyPress]],[[KeyPress]])
options single = do
  split <- asks splitter
  jn <- asks jobname
  aThing <- thing
  someThings <- things
  o <- asks opts
  return $
             ([optionsBasic jn aThing]
             ,[optionsSplit split aThing]
             ++ (if single then
                     [optionsFile jn ] else [])
             ++ [optionsView aThing someThings ++
                 if Summary `elem` o then []
                 else optionsSummary aThing]
             ++ [optionsQuit jn someThings]
             ++ [optionsNav aThing False]
             )

-- | Returns a @Sealed2@ version of the patch we are asking the user
-- about.
currentPatch :: forall p wX wY . Patchy p =>
               InteractiveSelectionM p wX wY
                    (Maybe (Sealed2 (TaggedPatch p)))
currentPatch = do
  (FZipper _ tps_todo) :: FZipper (TaggedPatch p) wX wY <- gets tps
  case tps_todo of
    NilFL -> return Nothing
    (tp:>:_) -> return $ Just (Sealed2 tp)

-- | Returns the patches we have yet to ask the user about.
todo :: forall p wX wY . Patchy p
        => InteractiveSelectionM p wX wY
              (FlippedSeal (FL (TaggedPatch p)) wY)
todo = do
    (FZipper _ tps_todo) <- gets tps
    return (FlippedSeal tps_todo)

-- | Modify the underlying @PatchChoices@ by some function
modChoices :: forall p wX wY . Patchy p =>
              (PatchChoices p wX wY -> PatchChoices p wX wY)
              -> InteractiveSelectionM p wX wY ()
modChoices f = modify $ \isc -> isc{choices = f $ choices isc}

-- | returns @Just f@ if the 'currentPatch' only modifies @f@,
-- @Nothing@ otherwise.
currentFile :: forall p wX wY . (Patchy p, PatchInspect p)
               => InteractiveSelectionM p wX wY (Maybe FilePath)
currentFile = do
  c <- currentPatch
  return $ case c of
             Nothing -> Nothing
             Just (Sealed2 tp) ->
                 case listTouchedFiles tp of
                   [f] -> Just f
                   _ -> Nothing

-- | @decide True@ selects the current patch, and @decide False@ deselects
-- it.
decide :: forall p wX wY wT wU . Patchy p => WhichChanges -> Bool
         -> TaggedPatch p wT wU
         -> InteractiveSelectionM p wX wY ()
decide whch takeOrDrop tp =
    if backward whch == takeOrDrop -- we go backward xor we are dropping
    then modChoices $ forceLast (tag tp)
    else modChoices $ forceFirst (tag tp)

-- | like 'decide', but for all patches touching @file@
decideWholeFile :: forall p wX wY. (Patchy p, PatchInspect p) => WhichChanges ->
                  FilePath -> Bool -> InteractiveSelectionM p wX wY ()
decideWholeFile whch file takeOrDrop =
    do
      FlippedSeal tps_todo <- todo
      let patches_to_skip =
              filterFL (\tp' -> listTouchedFiles tp' == [file]) tps_todo
      mapM_ (unseal2 $ decide whch takeOrDrop) patches_to_skip

-- | Undecide the current patch.
postponeNext :: forall p wX wY . Patchy p => InteractiveSelectionM p wX wY ()
postponeNext =
    do
      Just (Sealed2 tp) <- currentPatch
      modChoices $ makeUncertain (tag tp)

-- | Focus the next patch.
skipOne :: forall p wX wY . Patchy p => InteractiveSelectionM p wX wY ()
skipOne = modify so
    where so x = x{tps = right (tps x), current = current x +1}

-- | Focus the previous patch.
backOne :: forall p wX wY . Patchy p => InteractiveSelectionM p wX wY ()
backOne = modify so
    where so isc = isc{tps = left (tps isc), current = max (current isc-1) 0}

-- | Split the current patch (presumably a hunk), and add the replace it
-- with its parts.
splitCurrent :: forall p wX wY . Patchy p => Splitter p
                -> InteractiveSelectionM p wX wY ()
splitCurrent s = do
    FZipper tps_done (tp:>:tps_todo) <- gets tps
    case (applySplitter s (tpPatch tp)) of
      Nothing -> return ()
      Just (text, parse) ->
          do
            newText <- liftIO $ editText "darcs-patch-edit" text
            case parse newText of
               Nothing -> return ()
               Just ps -> do
                 tps_new <- liftIO . return . snd
                             $ patchChoicesTpsSub (Just (tag tp)) ps
                 modify $ \isc -> isc { total = ( total isc
                                                  + lengthFL tps_new - 1 )
                                      , tps = (FZipper tps_done
                                               (tps_new +>+ tps_todo))
                                      , choices = (substitute
                                                   (seal2 (tp :||: tps_new))
                                                   (choices isc))
                                      }

-- | Returns a list of the currently selected patches, in
-- their original context, i.e., not commuted past unselected
-- patches.
selected :: forall p wX wY. Patchy p => WhichChanges ->
           InteractiveSelectionM p wX wY [Sealed2 p]
selected whichch = do
  c <- gets choices
  (first_chs :> _ :> last_chs) <- return $ getChoices c
  return $ if backward whichch
           then
               mapFL (repr whichch . Sealed2 . tpPatch) $ last_chs
           else
               mapFL (repr whichch . Sealed2 . tpPatch) $ first_chs

-- | Prints the list of the selected patches. See 'selected'.
printSelected :: (Patchy p, ShowPatch p) => WhichChanges ->
                InteractiveSelectionM p wX wY ()
printSelected whichch = do
  someThings <- things
  o <- asks opts
  s <- selected whichch
  liftIO $ do
    putStrLn $ "---- Already selected "++someThings++" ----"
    mapM_ (putDocLn . unseal2 (showFriendly o)) s
    putStrLn $ "---- end of already selected "++someThings++" ----"

printSummary :: forall p wX wY . ShowPatch p => p wX wY -> IO ()
printSummary =
  putDocLn . prefix "    " . summary

-- | Skips all remaining patches.
skipAll :: forall p wX wY . Patchy p =>
          InteractiveSelectionM p wX wY ()
skipAll = do
  modify $ \isc -> isc {tps = toEnd $ tps isc}

backAll :: forall p wX wY . Patchy p =>
          InteractiveSelectionM p wX wY ()
backAll = do
  modify $ \isc -> isc {tps = toStart $ tps isc
                       ,current = 0}

isSingleFile :: PatchInspect p => p wX wY -> Bool
isSingleFile p = length (listTouchedFiles p) == 1

askConfirmation :: forall p wX wY . Patchy p =>
                   InteractiveSelectionM p wX wY ()
askConfirmation = do
    jn <- asks jobname
    liftIO $ if jn `elem` ["unpull", "unrecord", "obliterate"]
             then do
               yorn <- askUser $ "Really " ++ jn ++ " all undecided patches? "
               case yorn of
                 ('y':_) -> return ()
                 _ -> exitWith $ ExitSuccess
             else return ()

-- | The singular form of the noun for items of type @p@.
thing :: (Patchy p, ShowPatch p) => InteractiveSelectionM p wX wY String
thing = gets choices >>= return . Darcs.Patch.thing . helper
        where
          helper :: PatchChoices p wA wB -> p wA wB
          helper = undefined

-- | The plural form of the noun for items of type @p@.
things :: (Patchy p, ShowPatch p) => InteractiveSelectionM p wX wY String
things = gets choices >>= return . Darcs.Patch.things . helper
        where
          helper :: PatchChoices p wA wB -> p wA wB
          helper = undefined

-- | The question to ask about one patch.
prompt :: (Patchy p, ShowPatch p) => InteractiveSelectionM p wX wY String
prompt = do
  jn <- asks jobname
  aThing <- thing
  n <- gets current
  n_max <- gets total
  return $ "Shall I "++jn++" this "++aThing++"? "
             ++ "(" ++ show (n+1) ++ "/" ++ show n_max ++ ") "

-- | Asks the user about one patch, returns their answer.
promptUser :: forall p wX wY . (Patchy p, ShowPatch p) => Bool -> Char
              -> InteractiveSelectionM p wX wY Char
promptUser single def = do
  thePrompt <- prompt
  (basicOptions,advancedOptions) <- options single
  liftIO $ promptChar $ PromptConfig { pPrompt = thePrompt
                                     , pBasicCharacters = keysFor basicOptions
                                     , pAdvancedCharacters = keysFor advancedOptions
                                     , pDefault = Just def
                                     , pHelp = "?h"
                                     }

-- | Ask the user what to do with the next patch.
textSelectOne :: forall p wX wY. (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree) => WhichChanges
            -> InteractiveSelectionM p wX wY Bool
textSelectOne whichch = do
 c <- currentPatch
 case c of
   Nothing -> return False
   Just (Sealed2 tp) ->
       do
         jn <- asks jobname
         spl <- asks splitter
         let singleFile = isSingleFile (tpPatch tp)
             reprCur = repr whichch (Sealed2 (tpPatch tp))
         (basicOptions,advancedOptions) <- options singleFile
         theSlot <- liftChoices $ patchSlot' tp
         let
             the_default = getDefault (whichch == Last || whichch == FirstReversed) theSlot
             jnCapital = (toUpper $ head jn) : tail jn
         yorn <- promptUser singleFile the_default
         let nextPatch = skipMundane whichch >> showCur whichch
         case yorn of
               'y' -> decide whichch True tp >> skipOne >> nextPatch
                      >> return False
               'n' -> decide whichch False tp >> skipOne >> nextPatch
                      >> return False
               'w' -> postponeNext >> skipOne >> nextPatch
                      >> return False
               'e' | (Just s) <- spl -> splitCurrent s >> showCur whichch
                                        >> return False
               's' -> currentFile >>= maybe
                       (return ())
                       (\f -> decideWholeFile whichch f False) >> nextPatch
                       >> return False
               'f' -> currentFile >>= maybe
                       (return ())
                       (\f -> decideWholeFile whichch f True) >> nextPatch
                       >> return False
               'v' -> liftIO $ unseal2 printPatch reprCur >> return False
               'p' -> liftIO $ unseal2 printPatchPager reprCur >> return False
               'l' -> printSelected whichch >> showCur whichch >> return False
               'x' -> liftIO $ unseal2 printSummary reprCur >> return False
               'd' -> skipAll >> return True
               'g' -> backAll >> showCur whichch >> return False
               'a' ->
                   do
                     askConfirmation
                     modChoices $ selectAllMiddles (whichch == Last || whichch == FirstReversed)
                     skipAll
                     return True
               'q' -> liftIO $
                      do putStrLn $ jnCapital++" cancelled."
                         exitWith $ ExitSuccess
               'j' -> skipOne >> showCur whichch >> return False
               'k' -> backOne >> showCur whichch >> return False
               _   -> do
                 liftIO . putStrLn $ helpFor jn basicOptions advancedOptions
                 return False

lastQuestion :: forall p wX wY . (Patchy p, ShowPatch p, ApplyState p ~ Tree) => WhichChanges ->
                InteractiveSelectionM p wX wY Bool
lastQuestion whichch = do
  jn <- asks jobname
  theThings <-things
  aThing <- thing
  let (basicOptions, advancedOptions) = optionsLast jn aThing
  yorn <- liftIO . promptChar $
            PromptConfig { pPrompt = "Do you want to "++jn++" these "++ theThings++"?"
                         , pBasicCharacters = "yglqk"
                         , pAdvancedCharacters = "da"
                         , pDefault = Just 'y'
                         , pHelp = "?h"}
  case yorn of c | c `elem` "yda" -> return True
               'g' -> backAll >> showCur whichch >> return False
               'l' -> printSelected whichch >> return False
               'q' -> liftIO $
                      do putStrLn $ jn ++" cancelled."
                         exitWith $ ExitSuccess
               'k' -> backOne >> showCur whichch >> return False
               _ -> do
                 liftIO . putStrLn $ helpFor "this confirmation prompt" basicOptions advancedOptions
                 return False

-- | Shows the current patch as it should be seen by the user.
showCur :: forall p wX wY . (Patchy p, ShowPatch p, ApplyState p ~ Tree) => WhichChanges
           -> InteractiveSelectionM p wX wY ()
showCur whichch = do
  o <- asks opts
  p <- asks pristine
  c <- currentPatch
  case c of
      Nothing -> return ()
      Just (Sealed2 tp) -> do
             let reprCur = repr whichch (Sealed2 (tpPatch tp))
             liftIO . (unseal2 (printFriendly p o)) $ reprCur

-- | The interactive part of @darcs changes@
textView :: forall p . (Patchy p, ShowPatch p, ApplyState p ~ Tree) => [DarcsFlag] -> Maybe Int -> Int
            -> [Sealed2 p] -> [Sealed2 p]
            -> IO ()
textView _ _ _ _ [] = return ()
textView o n_max n
            ps_done ps_todo@(p:ps_todo') = do
      unseal2 (printFriendly Nothing o) p
      repeatThis -- prompt the user
    where
        prev_patch :: IO ()
        prev_patch = case ps_done of
                       [] -> repeatThis
                       (p':ps_done') ->
                         textView o
                            n_max (n-1) ps_done' (p':ps_todo)
        next_patch :: IO ()
        next_patch = case ps_todo' of
                         [] -> -- May as well work out the length now we have all
                                  -- the patches in memory
                               textView o n_max
                                   n ps_done []
                         _ -> textView o n_max
                                  (n+1) (p:ps_done) ps_todo'
        first_patch = textView o n_max 0 [] (ps_done++ps_todo)
        options_yn =
          [ KeyPress 'y' "view this patch and go to the next"
          , KeyPress 'n' "skip to the next patch" ]
        optionsView' =
          [ KeyPress 'v' "view this patch in full"
          , KeyPress 'p' "view this patch in full with pager" ]
        optionsSummary' =
          [ KeyPress 'x' "view a summary of this patch" ]
        optionsNav' =
          [ KeyPress 'q' "quit view changes"
          , KeyPress 'k' "back up to previous patch"
          , KeyPress 'j' "skip to next patch"
          , KeyPress 'g' "start over from the first patch"
          , KeyPress 'c' "count total patch number" ]
        basicOptions = [ options_yn ]
        advancedOptions =
                     [ optionsView' ++
                       if Summary `elem` o then [] else optionsSummary' ]
                  ++ [ optionsNav' ]
        prompt' = "Shall I view this patch? "
               ++ "(" ++ show (n+1) ++ "/" ++ maybe "?" show n_max ++ ")"
        repeatThis :: IO ()
        repeatThis = do
          yorn <- promptChar (PromptConfig prompt' (keysFor basicOptions) (keysFor advancedOptions) (Just 'n') "?h")
          case yorn of
            'y' -> unseal2 printPatch p >> next_patch
            'n' -> next_patch
            'v' -> unseal2 printPatch p >> repeatThis
            'p' -> unseal2 printPatchPager p >> repeatThis
            'x' -> do putDocLn $ prefix "    " $ unseal2 summary p
                      repeatThis
            'q' -> exitWith ExitSuccess
            'k' -> prev_patch
            'j' -> next_patch
            'g' -> first_patch
            'c' -> textView o
                       count_n_max n ps_done ps_todo
            _   -> do putStrLn $ helpFor "view changes" basicOptions advancedOptions
                      repeatThis
        count_n_max | isJust n_max = n_max
                    | otherwise    = Just $ length ps_done + length ps_todo

-- | Skips patches we should not ask the user about
skipMundane :: (Patchy p, ShowPatch p) => WhichChanges ->
              InteractiveSelectionM p wX wY ()
skipMundane whichch = do
  (FZipper tps_done tps_todo) <- gets tps
  o <- asks opts
  crit <- asks matchCriterion
  jn <- asks jobname
  (skipped :> unskipped) <- liftChoices $ spanFL_M
                                 (patchSlot' >=> return . decided)
                                 tps_todo
  let numSkipped = lengthFL skipped
  when (numSkipped > 0) . liftIO $ show_skipped o jn numSkipped skipped
  let boringThenInteresting =
          if DontPromptForDependencies `elem` o
          then spanFL (not.(crit whichch o) . seal2 . tpPatch) $
                                 unskipped
          else NilFL :> unskipped
  case boringThenInteresting of
    boring :> interesting ->
        do
          justDone $ lengthFL boring + numSkipped
          modify $ \isc -> isc {tps = (FZipper (reverseFL boring +<+ reverseFL skipped +<+ tps_done) interesting)}
    where
      show_skipped o jn n ps = do putStrLn $ _nevermind_ jn ++ _these_ n ++ "."
                                  when (Verbose `elem` o) $
                                       showskippedpatch ps
      _nevermind_ jn = "Will not ask whether to " ++ jn ++ " "
      _these_ n  = show n ++ " already decided " ++ _elem_ n ""
      _elem_ n = englishNum n (Noun "patch")
      showskippedpatch :: (Patchy p, ShowPatch p) => FL (TaggedPatch p) wY wT -> IO ()
      showskippedpatch =
                    sequence_ . mapFL (printSummary . tpPatch)

decided :: Slot -> Bool
decided InMiddle = False
decided _ = True

-- | The action bound to space, depending on the current status of the
-- patch.
getDefault :: Bool -> Slot -> Char
getDefault _ InMiddle = 'w'
getDefault True InFirst  = 'n'
getDefault True InLast   = 'y'
getDefault False InFirst = 'y'
getDefault False InLast  = 'n'

-- |Optionally remove any patches (+dependencies) from a sequence that
-- conflict with the recorded or unrecorded changes in a repo
filterOutConflicts :: (RepoPatch p, ApplyState p ~ Tree)
                   => [DarcsFlag]                                    -- ^Command-line options. Only 'SkipConflicts' is
                                                                     -- significant; filtering will happen iff it is present
                   -> RL (PatchInfoAnd p) wX wT                     -- ^Recorded patches from repository, starting from
                                                                     -- same context as the patches to filter
                   -> Repository p wR wU wT                          -- ^Repository itself, used for grabbing unrecorded changes
                   -> FL (PatchInfoAnd p) wX wZ                     -- ^Patches to filter
                   -> IO (Bool, Sealed (FL (PatchInfoAnd p) wX))   -- ^(True iff any patches were removed, possibly filtered patches)
filterOutConflicts o us repository them
  | SkipConflicts `elem` o
     = do let commuter = commuterIdRL selfCommuter
          unrec <- fmap n2pia . (anonymous . fromPrims)
                     =<< unrecordedChanges (UseIndex, ScanKnown) repository Nothing
          them' :> rest <- return $ partitionConflictingFL commuter them (unrec :<: us)
          return (check rest, Sealed them')
  | otherwise
     = return (False, Sealed them)
  where check :: FL p wA wB -> Bool
        check NilFL = False
        check _ = True
