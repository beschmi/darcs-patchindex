--  Copyright (C) 2002-2004 David Roundy
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

{-# LANGUAGE CPP, TypeOperators #-}

module Darcs.UI.Commands.Push ( push ) where
import System.Exit ( exitWith, ExitCode( ExitSuccess, ExitFailure ) )
import Control.Monad ( when )
import Data.Char ( toUpper )
import Workaround ( getCurrentDirectory )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , putVerbose
    , putInfo
    , abortRun
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , formatPath
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Arguments
    ( DarcsFlag(
                 Sign
               , SignAs
               , NoSign
               , SignSSL
               , AllowUnrelatedRepos
               )
    , workingRepoDir
    , summary
    , applyas
    , matchSeveral
    , fixUrl
    , depsSel
    , allInteractive
    , remoteRepo
    , networkOptions
    , sign
    , allowUnrelatedRepos
    , changesReverse
   )
import qualified Darcs.UI.Arguments as A ( dryRun, setDefault )
import Darcs.UI.Flags(doReverse, dryRun, useCache, umask, remoteRepos, setDefault )
import Darcs.Repository.Flags ( DryRun (..) )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Repository ( Repository, withRepoReadLock, RepoJob(..), identifyRepositoryFor,
                          readRepo, checkUnrelatedRepos )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Witnesses.Ordered ( (:>)(..), RL, FL, nullRL,
                             nullFL, reverseFL, mapFL_FL, mapRL )
import Darcs.Repository.Prefs ( setDefaultrepo, getPreflist )
import Darcs.UI.External ( maybeURLCmd, signString )
import Darcs.URL ( isHttpUrl, isFile )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , runSelection
    )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Patch.Bundle ( makeBundleN )
import Darcs.Patch.Patchy( ShowPatch )
import Darcs.Patch.Set ( PatchSet, Origin )
import Printer ( Doc, vcat, empty, text, ($$) )
import Darcs.UI.RemoteApply ( remoteApply, applyAs )
import Darcs.UI.Email ( makeEmail )
import English (englishNum, Noun(..))
import Storage.Hashed.Tree( Tree )
#include "impossible.h"


pushDescription :: String
pushDescription =
 "Copy and apply patches from this repository to another one."

pushHelp :: String
pushHelp =
 "Push is the opposite of pull.  Push allows you to copy changes from the\n"++
 "current repository into another repository.\n"

push :: DarcsCommand
push = DarcsCommand {commandProgramName = "darcs",
                     commandName = "push",
                     commandHelp = pushHelp,
                     commandDescription = pushDescription,
                     commandExtraArgs = 1,
                     commandExtraArgHelp = ["[REPOSITORY]"],
                     commandCommand = pushCmd,
                     commandPrereq = amInHashedRepository,
                     commandGetArgPossibilities = getPreflist "repos",
                     commandArgdefaults = defaultRepo,
                     commandAdvancedOptions = [applyas,
                                                 remoteRepo,
                                                 changesReverse] ++
                                                networkOptions,
                     commandBasicOptions = [matchSeveral, depsSel,
                                              allInteractive,
                                              sign]++A.dryRun++[summary,
                                              workingRepoDir,
                                              A.setDefault False,
                                              allowUnrelatedRepos]}

pushCmd :: [DarcsFlag] -> [String] -> IO ()
pushCmd _ [""] = impossible
pushCmd opts [unfixedrepodir] =
 do
 repodir <- fixUrl opts unfixedrepodir
 -- Test to make sure we aren't trying to push to the current repo
 here <- getCurrentDirectory
 checkOptionsSanity opts repodir
 when (repodir == here) $
       fail "Cannot push from repository to itself."
       -- absolute '.' also taken into account by fix_filepath
 (bundle) <- withRepoReadLock (dryRun opts) (useCache opts) (umask opts) $ RepoJob $
                          prepareBundle opts repodir
 sbundle <- signString opts bundle
 let body = if isFile repodir
            then sbundle
            else makeEmail repodir [] Nothing Nothing sbundle Nothing
 rval <- remoteApply opts repodir body
 case rval of ExitFailure ec -> do putStrLn $ "Apply failed!"
                                   exitWith (ExitFailure ec)
              ExitSuccess -> putInfo opts $ text "Push successful."
pushCmd _ _ = impossible

prepareBundle :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> String -> Repository p wR wU wT -> IO (Doc)
prepareBundle opts repodir repository = do
  old_default <- getPreflist "defaultrepo"
  when (old_default == [repodir]) $
       let pushing = if dryRun opts == YesDryRun then "Would push" else "Pushing"
       in  putInfo opts $ text $ pushing++" to "++formatPath repodir++"..."
  them <- identifyRepositoryFor repository (useCache opts) repodir >>= readRepo
  setDefaultrepo repodir (dryRun opts) (remoteRepos opts) (setDefault opts)
  us <- readRepo repository
  common :> us' <- return $ findCommonWithThem us them
  prePushChatter opts us (reverseFL us') them
  let context = selectionContext "push" opts Nothing Nothing
      selector = if doReverse opts
                 then selectChanges FirstReversed
                 else selectChanges First
  runSelection (selector us') context
                   >>= bundlePatches opts common

prePushChatter :: forall p a wX wY wT . (RepoPatch p, ShowPatch a) =>
                 [DarcsFlag] -> PatchSet p Origin wX ->
                 RL a wT wX -> PatchSet p Origin wY -> IO ()
prePushChatter opts us us' them = do
  checkUnrelatedRepos (AllowUnrelatedRepos `elem` opts) us them
  let num_to_pull = snd $ countUsThem us them
      pull_reminder = if num_to_pull > 0
                      then text $ "The remote repository has " ++ show num_to_pull
                      ++ " " ++ englishNum num_to_pull (Noun "patch") " to pull."
                      else empty
  putVerbose opts $ text "We have the following patches to push:" $$ (vcat $ mapRL description us')
  when (not $ nullRL us') $ do putInfo opts $ pull_reminder
  when (nullRL us') $ do putInfo opts $ text "No recorded local changes to push!"
                         exitWith ExitSuccess

bundlePatches :: forall t p wZ wW wA. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet p wA wZ
              -> (FL (PatchInfoAnd p) :> t) wZ wW
              -> IO (Doc)
bundlePatches opts common (to_be_pushed :> _) =
    do
      setEnvDarcsPatches to_be_pushed
      printDryRunMessageAndExit "push" opts to_be_pushed
      when (nullFL to_be_pushed) $ do
          putInfo opts $
            text "You don't want to push any patches, and that's fine with me!"
          exitWith ExitSuccess
      bundle <- makeBundleN Nothing
                     common (mapFL_FL hopefully to_be_pushed)
      return (bundle)

wantSign :: [DarcsFlag] -> Bool
wantSign opts = case opts of
    []            -> False
    Sign:_        -> True
    (SignAs _):_  -> True
    (SignSSL _):_ -> True
    NoSign:_      -> False
    _:opts'       -> wantSign opts'


checkOptionsSanity :: [DarcsFlag] -> String -> IO ()
checkOptionsSanity opts repodir =
  if isHttpUrl repodir then do
       when (applyAs opts /= Nothing) $
           abortRun opts $ text "Cannot --apply-as when pushing to URLs"
       maybeapply <- maybeURLCmd "APPLY" repodir
       when (maybeapply == Nothing) $
         let lprot = takeWhile (/= ':') repodir
             prot = map toUpper lprot
             msg = text ("Pushing to "++lprot++" URLs is not supported.\n"++
                         "You may be able to hack this to work"++
                         " using DARCS_APPLY_"++prot) in
         abortRun opts msg
   else do
       when (wantSign opts) $
        abortRun opts $ text "Signing doesn't make sense for local repositories or when pushing over ssh."

