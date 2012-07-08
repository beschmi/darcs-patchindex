--  Copyright (C) 2002-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using Prelude.catch
{-# LANGUAGE CPP #-}

module Darcs.UI.Commands.Unrecord
    ( unrecord
    , unpull
    , obliterate
    , getLastPatches
    ) where

import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode( ExitSuccess ) )
import Data.Maybe( isJust )

import Printer ( text, putDoc )
import English ( presentParticiple )
import Darcs.Patch.PatchInfoAnd ( hopefully, patchDesc )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , nodefaults
    , commandAlias
    , putVerbose
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Arguments
    ( DarcsFlag ( NoPatchIndexFlag )
    , output
    , outputAutoName
    , getOutput
    , workingRepoDir
    , nocompress
    , matchSeveralOrLast
    , depsSel
    , ignoretimes
    , allInteractive
    , umaskOption
    , summary
    , changesReverse
    , patchIndex
    , noPatchIndex
    )
import qualified Darcs.UI.Arguments as A ( dryRun )
import Darcs.UI.Flags
    ( doReverse, compression, verbosity, toMatchFlags, useCache, dryRun, umask )
import Darcs.Repository.Flags( UseIndex(..), ScanKnown(..), UpdateWorking(..) )
import Darcs.Patch.Match ( firstMatch, matchFirstPatchset, matchAPatchread, MatchFlag )
import Darcs.Repository ( PatchInfoAnd, withGutsOf,
                          withRepoLock, RepoJob(..),
                    tentativelyRemovePatches, finalizeRepositoryChanges,
                    tentativelyAddToPending,
                    applyToWorking,
                    readRepo,
                    invalidateIndex, unrecordedChanges )
import Darcs.Patch ( RepoPatch, invert, commute, effect )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), appendPSFL, Origin )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..), (:>)(..), (+<+),
    mapFL_FL, nullFL,
    reverseRL, mapRL, FL(..) )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , runSelection
    )
import Darcs.Patch.Bundle ( makeBundleN, patchFilename, contextPatches )
import Progress ( debugMessage )
import Darcs.Path( useAbsoluteOrStd )
import Darcs.Repository.Lock( writeDocBinFile )
import Storage.Hashed.Tree( Tree )

unrecordDescription :: String
unrecordDescription =
 "Remove recorded patches without changing the working copy."

unrecordHelp :: String
unrecordHelp =
 "Unrecord does the opposite of record in that it makes the changes from\n"++
 "patches active changes again which you may record or revert later.  The\n"++
 "working copy itself will not change.\n"++
 "Beware that you should not use this command if you are going to\n"++
 "re-record the changes in any way and there is a possibility that\n"++
 "another user may have already pulled the patch.\n"

unrecord :: DarcsCommand
unrecord = DarcsCommand {commandProgramName = "darcs",
                         commandName = "unrecord",
                         commandHelp = unrecordHelp,
                         commandDescription = unrecordDescription,
                         commandExtraArgs = 0,
                         commandExtraArgHelp = [],
                         commandCommand = unrecordCmd,
                         commandPrereq = amInHashedRepository,
                         commandGetArgPossibilities = return [],
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions =
                             [nocompress,umaskOption,changesReverse, patchIndex, noPatchIndex],
                         commandBasicOptions = [matchSeveralOrLast,
                                                 depsSel,
                                                 allInteractive,
                                                 workingRepoDir]}

unrecordCmd :: [DarcsFlag] -> [String] -> IO ()
unrecordCmd opts _ = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
  allpatches <- readRepo repository
  let matchFlags = toMatchFlags opts
  (_ :> patches) <- return $ if firstMatch matchFlags
                             then getLastPatches matchFlags allpatches
                             else matchingHead matchFlags allpatches
  let context = selectionContext "unrecord" opts Nothing Nothing
      selector = if doReverse opts
                 then selectChanges Last
                 else selectChanges LastReversed
  (_ :> to_unrecord) <- runSelection (selector patches) context
  when (nullFL to_unrecord) $ do putStrLn "No patches selected!"
                                 exitWith ExitSuccess
  putVerbose opts $ text
                      "About to write out (potentially) modified patches..."
  setEnvDarcsPatches to_unrecord
  invalidateIndex repository
  withGutsOf repository $ do _ <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking to_unrecord
                             finalizeRepositoryChanges repository (dryRun opts) YesUpdateWorking (compression opts) (not $ NoPatchIndexFlag `elem` opts)
  putStrLn "Finished unrecording."

getLastPatches :: RepoPatch p => [MatchFlag] -> PatchSet p Origin wR
                 -> ((PatchSet p) :> (FL (PatchInfoAnd p))) Origin wR
getLastPatches matchFlags ps =
  case matchFirstPatchset matchFlags ps of
  Sealed p1s -> findCommonWithThem ps p1s

unpullDescription :: String
unpullDescription =
 "Opposite of pull; unsafe if patch is not in remote repository."

unpullHelp :: String
unpullHelp =
 "Unpull completely removes recorded patches from your local repository.\n"++
 "The changes will be undone in your working copy and the patches will not be\n"++
 "shown in your changes list anymore.\n"++
 "Beware that if the patches are not still present in another repository you\n"++
 "will lose precious code by unpulling!\n"

unpull :: DarcsCommand
unpull = (commandAlias "unpull" Nothing obliterate)
                      {commandHelp = unpullHelp,
                       commandDescription = unpullDescription,
                       commandCommand = unpullCmd}

unpullCmd :: [DarcsFlag] -> [String] -> IO ()
unpullCmd = genericObliterateCmd "unpull"


obliterateDescription :: String
obliterateDescription =
 "Delete selected patches from the repository. (UNSAFE!)"

obliterateHelp :: String
obliterateHelp =
 "Obliterate completely removes recorded patches from your local repository.\n"++
 "The changes will be undone in your working copy and the patches will not be\n"++
 "shown in your changes list anymore.\n"++
 "Beware that you can lose precious code by obliterating!\n"

obliterate :: DarcsCommand
obliterate = DarcsCommand {commandProgramName = "darcs",
                           commandName = "obliterate",
                           commandHelp = obliterateHelp,
                           commandDescription = obliterateDescription,
                           commandExtraArgs = 0,
                           commandExtraArgHelp = [],
                           commandCommand = obliterateCmd,
                           commandPrereq = amInHashedRepository,
                           commandGetArgPossibilities = return [],
                           commandArgdefaults = nodefaults,
                           commandAdvancedOptions = [nocompress,ignoretimes,umaskOption, changesReverse],
                           commandBasicOptions = [matchSeveralOrLast,
                                                   depsSel,
                                                   allInteractive,
                                                   workingRepoDir,
                                                   summary,
                                                   output,
                                                   outputAutoName]++
                                                   A.dryRun}
obliterateCmd :: [DarcsFlag] -> [String] -> IO ()
obliterateCmd = genericObliterateCmd "obliterate"

-- | genericObliterateCmd is the function that executes the "obliterate" and
--   "unpull" commands.
genericObliterateCmd :: String      -- ^ The name under which the command is invoked (@unpull@ or @obliterate@)
                       -> [DarcsFlag] -- ^ The flags given on the command line
                       -> [String]    -- ^ Files given on the command line (unused)
                       -> IO ()
genericObliterateCmd cmdname opts _ = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
  -- FIXME we may need to honour --ignore-times here, although this command
  -- does not take that option (yet)
  pend <- unrecordedChanges (UseIndex, ScanKnown) repository Nothing
  allpatches <- readRepo repository
  let matchFlags = toMatchFlags opts
  (auto_kept :> removal_candidates) <- return $
                                        if firstMatch matchFlags
                                        then getLastPatches matchFlags allpatches
                                        else matchingHead matchFlags allpatches
  let
      context = selectionContext cmdname opts Nothing Nothing
      selector = if doReverse opts
                 then selectChanges Last
                 else selectChanges LastReversed
  (kept :> removed) <- runSelection (selector removal_candidates) context
  when (nullFL removed) $ do putStrLn "No patches selected!"
                             exitWith ExitSuccess
  case commute (effect removed :> pend) of
    Nothing -> fail $ "Can't "++ cmdname ++
               " patch without reverting some unrecorded change."
    Just (_ :> p_after_pending) -> do
        printDryRunMessageAndExit "obliterate" opts removed
        setEnvDarcsPatches removed
        when (isJust $ getOutput opts "") $
             savetoBundle opts (auto_kept `appendPSFL` kept) removed
        invalidateIndex repository
        withGutsOf repository $
                             do _ <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking removed
                                tentativelyAddToPending repository (dryRun opts) YesUpdateWorking $ invert $ effect removed
                                finalizeRepositoryChanges repository (dryRun opts) YesUpdateWorking (compression opts) (not $ NoPatchIndexFlag `elem` opts)
                                debugMessage "Applying patches to working directory..."
                                _ <- applyToWorking repository (verbosity opts) (invert p_after_pending) `catch` \e ->
                                    fail ("Couldn't undo patch in working dir.\n" ++ show e)
                                return ()
        putStrLn $ "Finished " ++ presentParticiple cmdname ++ "."

-- | matchingHead returns the repository up to some tag. The tag t is
-- the last tag such that there is a patch after t that is matched by
-- the user's query.
matchingHead :: forall p wR. RepoPatch p =>
                [MatchFlag] -> PatchSet p Origin wR
             -> (PatchSet p :> FL (PatchInfoAnd p)) Origin wR
matchingHead matchFlags set =
    case mh set of
      (start :> patches) -> (start :> reverseRL patches)
    where
      mh :: forall wX . PatchSet p Origin wX
         -> (PatchSet p :> RL (PatchInfoAnd p)) Origin wX
      mh s@(PatchSet x _)
          | or (mapRL (matchAPatchread matchFlags) x) = contextPatches s
      mh (PatchSet x (Tagged t _ ps :<: ts))
          = case mh (PatchSet (t:<:ps) ts)
            of (start :> patches) -> (start :> x +<+ patches)
      mh ps = (ps :> NilRL)

savetoBundle :: (RepoPatch p, ApplyState p ~ Tree) => [DarcsFlag]
             -> PatchSet p Origin wZ -> FL (PatchInfoAnd p) wZ wT
             -> IO ()
savetoBundle opts kept removed@(x :>: _) = do
    bundle <- makeBundleN Nothing kept (mapFL_FL hopefully removed)
    let filename = patchFilename $ patchDesc x
        Just outname = getOutput opts filename
    useAbsoluteOrStd writeDocBinFile putDoc outname $ bundle

savetoBundle _ _ NilFL = return ()

