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
module Darcs.UI.Commands.Repair
    (
      repair
    , check
    ) where

import Control.Monad ( when, unless )
import Control.Applicative( (<$>) )
import System.Exit ( ExitCode(..), exitWith )
import System.Directory( renameFile )

import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, putInfo, commandAlias, amInHashedRepository )
import Darcs.UI.Arguments ( DarcsFlag(Quiet,DryRun),
                         umaskOption,
                         workingRepoDir,
                         ignoretimes,
                         dryRunNoxml
                      )
import Darcs.UI.Flags
    ( willIgnoreTimes, verbosity, dryRun, umask, useCache, compression )
import Darcs.Repository.Flags ( UpdateWorking (..) )
import Darcs.Repository.Repair( replayRepository, checkIndex,
                                replayRepositoryInTemp,
                                RepositoryConsistency(..) )
import Darcs.Repository ( Repository, withRepository,
                          readRecorded, RepoJob(..),
                          withRepoLock, replacePristine, writePatchSet )
import Darcs.Patch ( RepoPatch, showPatch, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Witnesses.Ordered ( FL(..) )
import Darcs.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.Diff( treeDiff )
import Printer ( text, ($$), (<+>) )
import Storage.Hashed.Tree( Tree )


repairDescription :: String
repairDescription = "Repair a corrupted repository."

repairHelp :: String
repairHelp =
 "The `darcs repair' command attempts to fix corruption in the current\n" ++
 "repository.  Currently it can only repair damage to the pristine tree,\n" ++
 "which is where most corruption occurs.\n" ++
 "This command rebuilds a pristine tree by applying successively the\n" ++
 "patches in the repository to an empty tree.\n" ++
 "\n" ++
 "The flag --dry-run make this operation read-only, making darcs exit\n" ++
 "unsuccessfully (with a non-zeo exit status) if the rebuilt pristine is\n" ++
 "different from the current pristine.\n"

repair :: DarcsCommand
repair = DarcsCommand {commandProgramName = "darcs",
                       commandName = "repair",
                       commandHelp = repairHelp,
                       commandDescription = repairDescription,
                       commandExtraArgs = 0,
                       commandExtraArgHelp = [],
                       commandCommand = repairCmd,
                       commandPrereq = amInHashedRepository,
                       commandGetArgPossibilities = return [],
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [umaskOption],
                       commandBasicOptions = [  workingRepoDir
                                              , ignoretimes
                                              , dryRunNoxml
                                             ]
                      }



repairCmd :: [DarcsFlag] -> [String] -> IO ()
repairCmd opts _
 | DryRun `elem` opts = withRepository (useCache opts) (RepoJob (check' opts))
 | otherwise = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
  replayRepository repository (compression opts) (verbosity opts) $ \state ->
    case state of
      RepositoryConsistent ->
        putStrLn "The repository is already consistent, no changes made."
      BrokenPristine tree -> do
        putStrLn "Fixing pristine tree..."
        replacePristine repository tree
      BrokenPatches tree newps  -> do
        putStrLn "Writing out repaired patches..."
        _ <- writePatchSet newps (useCache opts) (compression opts)
        putStrLn "Fixing pristine tree..."
        replacePristine repository tree
  index_ok <- checkIndex repository (Quiet `elem` opts)
  unless index_ok $ do renameFile "_darcs/index" "_darcs/index.bad"
                       putStrLn "Bad index discarded."

check'
  :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree)
  => [DarcsFlag] -> Repository p wR wU wT -> IO ()
check' opts repository = do
  state <- replayRepositoryInTemp repository (compression opts) (verbosity opts)
  failed <-
    case state of
      RepositoryConsistent -> do
        putInfo opts $ text "The repository is consistent!"
        return False
      BrokenPristine newpris -> do
        brokenPristine newpris
        return True
      BrokenPatches newpris _ -> do
        brokenPristine newpris
        putInfo opts $ text "Found broken patches."
        return True
  bad_index <- case willIgnoreTimes opts of
                 False -> not <$> checkIndex repository (Quiet `elem` opts)
                 True -> return False
  when bad_index $ putInfo opts $ text "Bad index."
  exitWith $ if failed || bad_index then ExitFailure 1 else ExitSuccess
 where
   brokenPristine newpris = do
     putInfo opts $ text "Looks like we have a difference..."
     mc' <- (fmap Just $ readRecorded repository) `catch` (\_ -> return Nothing)
     case mc' of
       Nothing -> do
         putInfo opts $ text "cannot compute that difference, try repair"
         putInfo opts $ text "" $$ text "Inconsistent repository"
       Just mc -> do
         ftf <- filetypeFunction
         Sealed (diff :: FL (PrimOf p) wR wR2)
          <- unFreeLeft `fmap` treeDiff ftf newpris mc :: IO (Sealed (FL (PrimOf p) wR))
         putInfo opts $ case diff of
                  NilFL -> text "Nothing"
                  patch -> text "Difference: " <+> showPatch patch
         putInfo opts $ text "" $$ text "Inconsistent repository!"

-- |check is an alias for repair, with implicit DryRun flag.
check :: DarcsCommand
check = checkAlias { commandCommand = checkCmd
                   , commandDescription = checkDesc
                   }
  where
    checkAlias  = commandAlias "check" Nothing repair
    checkCmd fs = commandCommand repair (DryRun : fs)
    checkDesc   = "Alias for `darcs " ++ commandName repair ++ " --dry-run'."
