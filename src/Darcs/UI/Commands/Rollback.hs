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
import System.Exit ( exitWith, ExitCode(..) )
import Data.List ( sort )
import Data.Maybe ( isJust )
import System.Directory ( removeFile )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , nodefaults
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Record ( getLog )
import Darcs.UI.Commands.Unrecord ( getLastPatches )
import Darcs.UI.Commands.Util ( announceFiles, filterExistingFiles )
import Darcs.UI.Arguments
    ( DarcsFlag
    , fixSubPaths
    , getAuthor
    , workingRepoDir
    , nocompress
    , author
    , patchnameOption
    , askLongComment
    , notest
    , matchSeveralOrLast
    , allInteractive
    , umaskOption
    , recordRollback
    )
import qualified Darcs.UI.Arguments as A ( leaveTestDir )
import Darcs.Path ( toFilePath )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , applyToWorking
    , readRepo
    , tentativelyMergePatches
    , withGutsOf
    , testTentative
    , finalizeRepositoryChanges
    , invalidateIndex
    , tentativelyAddToPending
    , considerMergeToWorking
    , listRegisteredFiles
    )
import Darcs.Patch ( RepoPatch, summary, invert, namepatch, effect, fromPrims,
                     sortCoalesceFL, canonize, anonymous, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.UI.Flags ( isInteractive, doRecordRollback, toMatchFlags, verbosity
 , dryRun, umask, useCache, compression, lookForAdds, setScriptsExecutable, runTest, leaveTestDir, externalMerge, wantGuiPause )
import Darcs.Repository.Flags (AllowConflicts (..), UseIndex(..), ScanKnown(..), UpdateWorking(..))
import Darcs.Patch.Set ( PatchSet(..), newset2FL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Witnesses.Ordered
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Repository.Lock ( worldReadableTemp )
import Darcs.Patch.Match ( firstMatch )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , selectionContextPrim
    , runSelection
    )
import Darcs.Utils ( clarifyErrors, PromptConfig(..), promptChar )
import Printer ( renderString )
import Progress ( debugMessage )
import Darcs.Witnesses.Sealed ( Sealed(..) )
import IsoDate ( getIsoDateTime )
import Storage.Hashed.Tree( Tree )
#include "impossible.h"

rollbackDescription :: String
rollbackDescription =
 "Record a new patch reversing some recorded changes."

rollbackHelp :: String
rollbackHelp = unlines $ [
  "Rollback is used to undo the effects of some changes from patches",
  "in the repository. The selected changes are undone in your working",
  "copy, but the repository is left unchanged. First you are offered a",
  "choice of which patches to undo, then which changes within the",
  "patches to undo. Optionally, you can have darcs record this undo",
  "straight away, as an explicit \"rollback patch\". In both cases\n "++
  ", rollback is perfectly safe, unlike obliterate, as you keep all",
  "previous patches intact.\n"
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
                         commandAdvancedOptions = [nocompress,umaskOption],
                         commandBasicOptions = [matchSeveralOrLast,
                                                  allInteractive,
                                                  author, patchnameOption, askLongComment,
                                                  notest, A.leaveTestDir,
                                                  workingRepoDir, recordRollback]}

rollbackCmd :: [DarcsFlag] -> [String] -> IO ()
rollbackCmd opts args = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
  files <- if null args then return Nothing
    else Just . sort <$> fixSubPaths opts args
  when (files == Just []) $ fail "No valid arguments were given."
  let files_fp = map toFilePath <$> files
  announceFiles files "Recording changes in"
  existing_files <- maybe (return Nothing)
    (fmap Just . filterExistingFiles repository (lookForAdds opts)) files
  when (existing_files == Just []) $
       fail "None of the files you specified exist!"
  allpatches <- readRepo repository
  let matchFlags = toMatchFlags opts
  (_ :> patches) <- return $ if firstMatch matchFlags
                             then getLastPatches matchFlags allpatches
                             else (PatchSet NilRL NilRL):> (newset2FL allpatches)
  let patches_context = selectionContext "rollback" opts Nothing files_fp
  (_ :> ps) <- runSelection (selectChanges LastReversed patches) patches_context
  when (nullFL ps) $ do putStrLn "No patches selected!"
                        exitWith ExitSuccess
  setEnvDarcsPatches ps
  let hunks_context = selectionContextPrim "rollback" opts (Just reversePrimSplitter) files_fp Nothing
      hunks = (concatFL $ mapFL_FL canonize $ sortCoalesceFL $ effect ps)
  runSelection (selectChanges Last hunks) hunks_context >>=
               if (doRecordRollback opts)
               then (rollItBackNow opts repository ps)
               else (undoItNow opts repository)

rollItBackNow :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree) =>
                [DarcsFlag] -> Repository p wR wU wT ->  FL (PatchInfoAnd p) wX wY
                            -> (q :> FL (PrimOf p)) wA wT -> IO ()
rollItBackNow opts repository  ps (_ :> ps'') =
         do when (nullFL ps'') $ do putStrLn "No changes selected!"
                                    exitWith ExitSuccess
            let make_log = worldReadableTemp "darcs-rollback"
                newlog = Just ("", "":"rolling back:":"":lines (renderString $ summary ps ))
            --tentativelyRemovePatches repository opts (mapFL_FL hopefully ps)
            (name, my_log, logf) <- getLog opts newlog make_log $ invert ps''
            date <- getIsoDateTime
            my_author <- getAuthor opts
            rbp <- n2pia `fmap` namepatch date name my_author my_log
                                          (fromPrims $ invert ps'')
            debugMessage "Adding rollback patch to repository."
            Sealed pw <-
                tentativelyMergePatches repository "rollback"
                         YesAllowConflictsAndMark YesUpdateWorking
                         (externalMerge opts) (useCache opts) (wantGuiPause opts)
                         (compression opts) (verbosity opts)
                         (UseIndex, ScanKnown)
                         NilFL (rbp :>: NilFL)
            debugMessage "Finalizing rollback changes..."
            invalidateIndex repository
            rc <- testTentative repository (runTest opts)
                                           (leaveTestDir opts)
                                           (setScriptsExecutable opts)
                                           (compression opts)
                                           (verbosity opts)
            when (rc /= ExitSuccess) $ do
                when (not $ isInteractive opts) $ exitWith rc
                putStrLn $ "Looks like you have a bad patch: '"++name++"'"
                let prompt = "Shall I rollback anyway?"
                yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
                case yn of
                  'y' -> return ()
                  _ -> exitWith rc
            withGutsOf repository $ do
              finalizeRepositoryChanges repository (dryRun opts) YesUpdateWorking (compression opts)
              debugMessage "About to apply rolled-back changes to working directory."
              _ <- revertable $ applyToWorking repository (verbosity opts) pw
              return ()
            when (isJust logf) $ removeFile (fromJust logf)
            putStrLn "Finished rolling back."
          where revertable x = x `clarifyErrors` unlines
                  ["Error applying patch to the working directory.","",
                   "This may have left your working directory an inconsistent",
                   "but recoverable state. If you had no un-recorded changes",
                   "by using 'darcs revert' you should be able to make your",
                   "working directory consistent again."]

undoItNow :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
          => [DarcsFlag] -> Repository p wR wU wT
          -> (q :> FL (PrimOf p)) wA wT -> IO ()
undoItNow opts repo (_ :> prims) =
    do
      when (nullFL prims) $ do putStrLn "No changes selected!"
                               exitWith ExitSuccess
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

