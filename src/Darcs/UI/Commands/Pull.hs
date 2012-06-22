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

{-# LANGUAGE CPP #-}

module Darcs.UI.Commands.Pull ( pull, fetch ) where
import System.Exit ( ExitCode(..), exitWith )
import Workaround ( getCurrentDirectory )
import Control.Monad ( when, unless )
import Data.List ( nub )
import Data.Maybe ( fromMaybe )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , putVerbose
    , putInfo
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , formatPath
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.CommandsAux ( checkPaths )
import Darcs.UI.Arguments
    ( DarcsFlag
        ( AllowConflicts
        , Complement
        , DryRun
        , Intersection
        , MarkConflicts
        , NoAllowConflicts
        , Verbose
        , XMLOutput
        , Quiet
        , AllowUnrelatedRepos
        , SetScriptsExecutable
        )
    , allInteractive
    , allowUnrelatedRepos
    , changesReverse
    , depsSel
    , fixUrl
    , getOutput
    , ignoretimes
    , matchSeveral
    , networkOptions
    , nocompress
    , output
    , pauseForGui
    , pullConflictOptions
    , remoteRepo
    , repoCombinator
    , restrictPaths
    , setScriptsExecutableOption
    , summary
    , test
    , umaskOption
    , useExternalMerge
    , workingRepoDir
    )
import qualified Darcs.UI.Arguments as A ( dryRun, setDefault )
import Darcs.UI.Flags( doReverse, isInteractive, verbosity,   dryRun, umask, useCache, compression, setScriptsExecutable, runTest, leaveTestDir, remoteRepos, setDefault, allowConflicts, diffingOpts, externalMerge, allowConflicts, wantGuiPause )
import Darcs.Repository.Flags ( UpdateWorking (..) )
import Darcs.Repository
    ( Repository
    , identifyRepositoryFor
    , withGutsOf
    , withRepoLock
    , RepoJob(..)
    , finalizeRepositoryChanges
    , applyToWorking
    , testTentative
    , readRepo
    , checkUnrelatedRepos
    , invalidateIndex
    , modifyCache
    , modifyCache
    , Cache(..)
    , CacheLoc(..)
    , WritableOrNot(..)
    , setScriptsExecutablePatches
    )

import qualified Darcs.Repository.Cache as DarcsCache
import Darcs.Repository.Merge ( tentativelyMergePatches )
import Darcs.Patch.PatchInfoAnd ( info, hopefully, patchDesc )
import Darcs.Patch ( RepoPatch, description, PrimOf )
import Darcs.Patch.Bundle( makeBundleN, patchFilename )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Set ( PatchSet(..), SealedPatchSet )
import Darcs.Witnesses.Unsafe ( unsafeCoercePEnd )
import Darcs.Witnesses.Ordered ( (:>)(..), (:\/:)(..), FL(..), RL(..)
                               , mapFL, nullFL, reverseFL, mapFL_FL )
import Darcs.Patch.Permutations ( partitionFL )
import Darcs.Repository.Prefs ( addToPreflist, setDefaultrepo, getPreflist )
import Darcs.Repository.Motd (showMotd )
import Darcs.Patch.Depends ( findUncommon, findCommonWithThem,
                             newsetIntersection, newsetUnion )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , filterOutConflicts
    , runSelection
    , selectionContext
    )
import Darcs.Utils ( clarifyErrors,
                     PromptConfig(..), promptChar )
import Darcs.Witnesses.Sealed ( Sealed(..), seal )
import Printer ( putDocLn, vcat, ($$), text, putDoc )
import Darcs.Repository.Lock ( writeDocBinFile )
import Darcs.Path ( useAbsoluteOrStd, stdOut )
import Storage.Hashed.Tree( Tree )
#include "impossible.h"


pullDescription :: String
pullDescription =
 "Copy and apply patches from another repository to this one."

fetchDescription :: String
fetchDescription =
 "Fetch patches from another repository, but don't apply them."

pullHelp :: String
pullHelp =
 "Pull is used to bring changes made in another repository into the current\n"++
 "repository (that is, either the one in the current directory, or the one\n"++
 "specified with the --repodir option). Pull allows you to bring over all or\n"++
 "some of the patches that are in that repository but not in this one. Pull\n"++
 "accepts arguments, which are URLs from which to pull, and when called\n"++
 "without an argument, pull will use the repository from which you have most\n"++
 "recently either pushed or pulled.\n" ++
 "\n" ++
 "See 'darcs help apply' for detailed description of many options.\n"

fetchHelp :: String
fetchHelp =
 "fetch is used to bring changes made in another repository\n" ++
 "into the current repository without actually applying\n"++
 "them. Fetch allows you to bring over all or\n"++
 "some of the patches that are in that repository but not in this one. Fetch\n"++
 "accepts arguments, which are URLs from which to fetch, and when called\n"++
 "without an argument, fetch will use the repository from which you have most\n"++
 "recently either pushed or pulled.\n"++
 "The fetched patches are stored into a patch bundle, to be later\n" ++
 "applied using \"darcs apply\"."


fetch :: DarcsCommand
fetch = DarcsCommand {
         commandProgramName = "darcs",
         commandName = "fetch",
         commandHelp = fetchHelp,
         commandDescription = fetchDescription,
         commandExtraArgs = -1,
         commandExtraArgHelp = ["[REPOSITORY]..."],
         commandCommand = fetchCmd,
         commandPrereq = amInHashedRepository,
         commandGetArgPossibilities = getPreflist "repos",
         commandArgdefaults = defaultRepo,
         commandAdvancedOptions =
            [ repoCombinator
            , remoteRepo
            ] ++ networkOptions,
         commandBasicOptions = [matchSeveral,
                                  allInteractive]
                                 ++A.dryRun++
                                 [summary,
                                  depsSel,
                                  A.setDefault False,
                                  workingRepoDir,
                                  output,
                                  allowUnrelatedRepos]}

pull :: DarcsCommand
pull = DarcsCommand {commandProgramName = "darcs",
                     commandName = "pull",
                     commandHelp = pullHelp,
                     commandDescription = pullDescription,
                     commandExtraArgs = -1,
                     commandExtraArgHelp = ["[REPOSITORY]..."],
                     commandCommand = pullCmd,
                     commandPrereq = amInHashedRepository,
                     commandGetArgPossibilities = getPreflist "repos",
                     commandArgdefaults = defaultRepo,
                     commandAdvancedOptions =
                        [ repoCombinator
                        , nocompress
                        , ignoretimes
                        , remoteRepo
                        , setScriptsExecutableOption
                        , umaskOption
                        , restrictPaths
                        , changesReverse
                        , pauseForGui
                        ] ++ networkOptions,
                     commandBasicOptions = [matchSeveral,
                                              allInteractive,
                                              pullConflictOptions,
                                              useExternalMerge,
                                              test]++A.dryRun++[summary,
                                              depsSel,
                                              A.setDefault False,
                                              workingRepoDir,
                                              allowUnrelatedRepos]}

mergeOpts :: [DarcsFlag] -> [DarcsFlag]
mergeOpts opts | NoAllowConflicts `elem` opts = opts
                | AllowConflicts   `elem` opts = opts
                | otherwise                    = MarkConflicts : opts

pullCmd :: [DarcsFlag] -> [String] -> IO ()
pullCmd opts repos =
  do
    pullingFrom <- mapM (fixUrl opts) repos
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \ initRepo -> do
      let repository = modifyCache initRepo $ addReposToCache pullingFrom
      r <- fetchPatches opts' repos "pull" repository
      applyPatches opts' repository r
    where
      opts' = mergeOpts opts
      addReposToCache repos' (Ca cache) = Ca $ [ toReadOnlyCache r | r <- repos' ] ++  cache
      toReadOnlyCache = Cache DarcsCache.Repo NotWritable


fetchCmd :: [DarcsFlag] -> [String] -> IO ()
fetchCmd opts repos =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \ repository ->
        fetchPatches opts repos "fetch" repository
                         >>= makeBundle opts

fetchPatches :: forall p wR wU . (RepoPatch p, ApplyState p ~ Tree)
             => [DarcsFlag] -> [String] -> String
             -> Repository p wR wU wR
             -> IO (SealedPatchSet p Origin,
                    Sealed ((FL (PatchInfoAnd p)  :\/: FL (PatchInfoAnd p)) wR))
fetchPatches opts unfixedrepodirs@(_:_) jobname repository = do
  here <- getCurrentDirectory
  repodirs <- (nub . filter (/= here)) `fmap` mapM (fixUrl opts) unfixedrepodirs
  -- Test to make sure we aren't trying to pull from the current repo
  when (null repodirs) $
        fail "Can't pull from current repository!"
  old_default <- getPreflist "defaultrepo"
  when (old_default == repodirs && not (XMLOutput `elem` opts)) $
      let pulling = if DryRun `elem` opts then "Would pull" else "Pulling"
      in  putInfo opts $ text $ pulling++" from "++concatMap formatPath repodirs++"..."
  (Sealed them, Sealed compl) <- readRepos repository opts repodirs
  setDefaultrepo (head repodirs) (dryRun opts) (remoteRepos opts) (setDefault opts)
  mapM_ (addToPreflist "repos") repodirs
  unless (Quiet `elem` opts || XMLOutput `elem` opts) $ mapM_ showMotd repodirs
  us <- readRepo repository
  checkUnrelatedRepos (AllowUnrelatedRepos `elem` opts) us them

  common :> _ <- return $ findCommonWithThem us them
  us' :\/: them' <- return $ findUncommon us them
  _   :\/: compl' <- return $ findUncommon us compl

  let avoided = mapFL info compl'
  ps :> _ <- return $ partitionFL (not . (`elem` avoided) . info) them'
  when (Verbose `elem` opts) $
       do case us' of
            (x@(_:>:_)) -> putDocLn $ text "We have the following new (to them) patches:"
                                                             $$ (vcat $ mapFL description x)
            _ -> return ()
          when (not $ nullFL ps) $ putDocLn $ text "They have the following patches to pull:"
                                                             $$ (vcat $ mapFL description ps)
  (hadConflicts, Sealed psFiltered) <- filterOutConflicts opts (reverseFL us') repository ps
  when hadConflicts $ putStrLn "Skipping some patches which would cause conflicts."
  when  (nullFL psFiltered) $ do putInfo opts $ text "No remote changes to pull in!"
                                 setEnvDarcsPatches psFiltered
                                 exitWith ExitSuccess
  let context = selectionContext jobname opts Nothing Nothing
      selector = if doReverse opts
                 then selectChanges FirstReversed
                 else selectChanges First
  (to_be_pulled :> _) <- runSelection (selector psFiltered) $ context
  return (seal common, seal $ us' :\/: to_be_pulled)

fetchPatches _ [] jobname _ = fail $ "No default repository to " ++ jobname ++
                                " from, please specify one"

applyPatches :: forall p wR wU. (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
             => [DarcsFlag] -> Repository p wR wU wR
             -> (SealedPatchSet p Origin,
                 Sealed ((FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) wR))
             -> IO ()
applyPatches opts repository (_, Sealed (us' :\/: to_be_pulled)) =
         do
           printDryRunMessageAndExit "pull" opts to_be_pulled
           setEnvDarcsPatches to_be_pulled
           when (nullFL to_be_pulled) $ do
                               putStrLn "You don't want to pull any patches, and that's fine with me!"
                               exitWith ExitSuccess
           checkPaths opts to_be_pulled
           putVerbose opts $ text "Getting and merging the following patches:"
           putVerbose opts $ vcat $ mapFL description to_be_pulled
           Sealed pw <- tentativelyMergePatches repository "pull"
                                                (allowConflicts opts) YesUpdateWorking
                                                (externalMerge opts) (useCache opts)
                                                (wantGuiPause opts) (compression opts) (verbosity opts)
                                                (diffingOpts opts)
                                                us' to_be_pulled
           invalidateIndex repository
           rc <- testTentative repository (runTest opts)
                                          (leaveTestDir opts)
                                          (setScriptsExecutable opts)
                                          (compression opts)
                                          (verbosity opts)
           when (rc /= ExitSuccess) $ do
               when (not $ isInteractive opts) $ exitWith rc
               putStrLn $ "Looks like those patches do not pass the tests."
               let prompt = "Shall I apply them anyway?"
               yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
               case yn of
                 'y' -> return ()
                 _ -> exitWith rc
           withGutsOf repository $ do finalizeRepositoryChanges repository (dryRun opts) YesUpdateWorking (compression opts)
                                      _ <- revertable $ applyToWorking repository (verbosity opts) pw
                                      when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches pw
                                      return ()
           putInfo opts $ text "Finished pulling and applying."

makeBundle :: forall p wR . (RepoPatch p, ApplyState p ~ Tree)
           => [DarcsFlag]
           -> (SealedPatchSet p Origin,
               Sealed ((FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) wR))
           -> IO ()
makeBundle opts (Sealed common, Sealed (_ :\/: to_be_fetched)) =
    do
      bundle <- makeBundleN Nothing (unsafeCoercePEnd common) $
                 mapFL_FL hopefully to_be_fetched
      let fname = case to_be_fetched of
                    (x:>:_)-> patchFilename $ patchDesc x
                    _ -> impossible
          o = fromMaybe stdOut (getOutput opts fname)
      useAbsoluteOrStd writeDocBinFile putDoc o $ bundle

revertable :: IO a -> IO a
revertable x =
    x `clarifyErrors` unlines
          ["Error applying patch to the working directory.","",
           "This may have left your working directory an inconsistent",
           "but recoverable state. If you had no un-recorded changes",
           "by using 'darcs revert' you should be able to make your",
           "working directory consistent again."]

{- Read in the specified pull-from repositories.  Perform
Intersection, Union, or Complement read.  In patch-theory terms
(stated in set algebra, where + is union and & is intersection
and \ is complement):

    Union =         ((R1 + R2 + ... + Rn) \ Rc)
    Intersection =  ((R1 & R2 & ... & Rn) \ Rc)
    Complement =    (R1 \ Rc) \ ((R2 + R3 + ... + Rn) \ Rc)

                        where Rc = local repo
                              R1 = 1st specified pull repo
                              R2, R3, Rn = other specified pull repo

Since Rc is not provided here yet, the result of readRepos is a
tuple: the first patchset(s) to be complemented against Rc and then
the second patchset(s) to be complemented against Rc.
-}

readRepos :: (RepoPatch p, ApplyState p ~ Tree)
          => Repository p wR wU wT -> [DarcsFlag] -> [String]
          -> IO (SealedPatchSet p Origin,SealedPatchSet p Origin)
readRepos _ _ [] = impossible
readRepos to_repo opts us =
    do rs <- mapM (\u -> do r <- identifyRepositoryFor to_repo (useCache opts) u
                            ps <- readRepo r
                            return $ seal ps) us
       return $ if Intersection `elem` opts
                then (newsetIntersection rs, seal (PatchSet NilRL NilRL))
                else if Complement `elem` opts
                     then (head rs, newsetUnion $ tail rs)
                     else (newsetUnion rs, seal (PatchSet NilRL NilRL))

