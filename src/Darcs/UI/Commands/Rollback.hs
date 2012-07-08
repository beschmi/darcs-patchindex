--  Copyright (C) 2002-2004,2007 David Roundy
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

module Darcs.UI.Commands.Rollback ( rollback ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import System.Exit ( exitSuccess )
import Data.List ( sort )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , nodefaults
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Unrecord ( getLastPatches )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.UI.Arguments
    ( DarcsFlag
    , fixSubPaths
    , workingRepoDir
    , matchSeveralOrLast
    , allInteractive
    , umaskOption
    )
import Darcs.Path ( toFilePath )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , applyToWorking
    , readRepo
    , withGutsOf
    , finalizeRepositoryChanges
    , tentativelyAddToPending
    , considerMergeToWorking
    , listRegisteredFiles
    )
import Darcs.Patch ( RepoPatch, invert, effect, fromPrims,
                     sortCoalesceFL, canonize, anonymous, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.UI.Flags ( toMatchFlags, verbosity
 , dryRun, umask, useCache, compression, externalMerge, wantGuiPause )
import Darcs.Repository.Flags (AllowConflicts (..), UseIndex(..), ScanKnown(..), UpdateWorking(..))
import Darcs.Patch.Set ( PatchSet(..), newset2FL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch.Match ( firstMatch )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , selectionContextPrim
    , runSelection
    )
import Progress ( debugMessage )
import Storage.Hashed.Tree( Tree )

rollbackDescription :: String
rollbackDescription =
 "Apply the inverse of recorded changes to the working copy."

rollbackHelp :: String
rollbackHelp = unlines [
  "Rollback is used to undo the effects of some changes from patches",
  "in the repository. The selected changes are undone in your working",
  "copy, but the repository is left unchanged. First you are offered a",
  "choice of which patches to undo, then which changes within the",
  "patches to undo.",
  "",
  "If you want to record a patch from the rollback you can use revert",
  "and unrevert to put aside and restore unrecorded changes of your",
  "working copy."
 ]

rollback :: DarcsCommand
rollback = DarcsCommand {commandProgramName = "darcs",
                         commandName = "rollback",
                         commandHelp = rollbackHelp,
                         commandDescription = rollbackDescription,
                         commandExtraArgs = -1,
                         commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                         commandCommand = rollbackCmd,
                         commandPrereq = amInHashedRepository,
                         commandGetArgPossibilities = listRegisteredFiles,
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [umaskOption],
                         commandBasicOptions = [matchSeveralOrLast,
                                                  allInteractive,
                                                  workingRepoDir]}

rollbackCmd :: [DarcsFlag] -> [String] -> IO ()
rollbackCmd opts args =
 withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts)
  $ RepoJob $ \repository -> do
  files <- if null args then return Nothing
    else Just . sort <$> fixSubPaths opts args
  when (files == Just []) $ fail "No valid arguments were given."
  let files_fp = map toFilePath <$> files
  announceFiles files "Recording changes in"
  allpatches <- readRepo repository
  let matchFlags = toMatchFlags opts
  (_ :> patches) <- return $ if firstMatch matchFlags
                             then getLastPatches matchFlags allpatches
                             else PatchSet NilRL NilRL :> newset2FL allpatches
  let patches_context = selectionContext "rollback" opts Nothing files_fp
  (_ :> ps) <- runSelection (selectChanges LastReversed patches) patches_context
  when (nullFL ps) $ do putStrLn "No patches selected!"
                        exitSuccess
  setEnvDarcsPatches ps
  let hunks_context = selectionContextPrim "rollback" opts (Just reversePrimSplitter)
                                           files_fp Nothing
      hunks = concatFL $ mapFL_FL canonize $ sortCoalesceFL $ effect ps
  runSelection (selectChanges Last hunks) hunks_context >>=
               undoItNow opts repository

undoItNow :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
          => [DarcsFlag] -> Repository p wR wU wT
          -> (q :> FL (PrimOf p)) wA wT -> IO ()
undoItNow opts repo (_ :> prims) =
    do
      when (nullFL prims) $ do putStrLn "No changes selected!"
                               exitSuccess
      rbp <- n2pia `fmap` anonymous (fromPrims $ invert prims)
      Sealed pw <- considerMergeToWorking repo "rollback"
                         YesAllowConflictsAndMark YesUpdateWorking
                         (externalMerge opts) (useCache opts) (wantGuiPause opts)
                         (compression opts) (verbosity opts)
                         (UseIndex, ScanKnown)
                         NilFL (rbp :>: NilFL)
      tentativelyAddToPending repo (dryRun opts) YesUpdateWorking pw
      withGutsOf repo $ do
        finalizeRepositoryChanges repo (dryRun opts) YesUpdateWorking (compression opts)
        _ <- applyToWorking repo (verbosity opts) pw `catch` \e ->
            fail ("error applying rolled back patch to working directory\n"
                  ++ show e)
        debugMessage "Finished applying unrecorded rollback patch"

