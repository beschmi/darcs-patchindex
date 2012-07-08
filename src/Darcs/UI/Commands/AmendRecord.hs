--  Copyright (C) 2004,2007 David Roundy
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

-- |
-- Module      : Darcs.UI.Commands.AmendRecord
-- Copyright   : 2004, 2007 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.AmendRecord
    (
      amendrecord
    , amendunrecord
    ) where



import Data.Maybe ( isJust, isNothing )
import Data.List ( intersect )
import Control.Applicative ( (<$>) )
import Control.Monad ( when, unless )
import System.Directory ( removeFile )
import System.Exit ( ExitCode(..), exitWith )

import Darcs.UI.Arguments
    ( DarcsFlag ( All
                , AmendUnrecord
                , Unified
                , NoPatchIndexFlag
                )
    , fixSubPaths
    , allInteractive
    , ignoretimes
    , askLongComment
    , askdeps
    , keepDate
    , author
    , patchnameOption
    , nocompress
    , lookforadds
    , workingRepoDir
    , matchOneNontag
    , umaskOption
    , test
    , getEasyAuthor
    , setScriptsExecutableOption
    , amendUnrecord
    , unified
    , patchIndex
    , noPatchIndex
    )
import qualified Darcs.UI.Arguments as A ( leaveTestDir )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , commandAlias
    , nodefaults
    , setEnvDarcsFiles
    , amInHashedRepository
    )
import Darcs.UI.Commands.Record ( getDate, getLog, askAboutDepends )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.UI.Flags ( DarcsFlag(Author, LogFile, PatchName, AskDeps,
                               EditLongComment, PromptLongComment, KeepDate)
                   , isInteractive
                   , diffingOpts, compression, verbosity, removeFromAmended, useCache, dryRun, umask, setScriptsExecutable, runTest, leaveTestDir )
import Darcs.Repository.Flags ( UpdateWorking(..) )
import Darcs.Repository.Lock ( worldReadableTemp )
import Darcs.Patch ( RepoPatch, description, PrimOf, fromPrims,
                     infopatch, getdeps, adddeps, effect, invertFL
                   )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( piAuthor, piName, piLog, piDateString,
                          PatchInfo, patchinfo, isInverted, isTag, invertName,
                        )
import Darcs.Patch.Prim ( canonizeFL )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully, info, patchDesc )
import Darcs.Path ( toFilePath, SubPath() )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , withGutsOf
    , tentativelyRemovePatches
    , tentativelyAddPatch
    , finalizeRepositoryChanges
    , invalidateIndex
    , unrecordedChanges
    , testTentative
    , readRecorded
    , listRegisteredFiles
    )
import Darcs.Repository.Prefs ( globalPrefsDirDoc )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContextPrim
    , runSelection
    , withSelectedPatchFromRepo
    )
import Darcs.Utils ( askUser, clarifyErrors, PromptConfig(..), promptChar )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), (+>+), nullFL, reverseRL )

import Printer ( putDocLn )
import Storage.Hashed.Tree( Tree )


amendrecordDescription :: String
amendrecordDescription = "Improve a patch before it leaves your repository."


amendrecordHelp :: String
amendrecordHelp =
    "Amend-record updates a `draft' patch with additions or improvements,\n" ++
    "resulting in a single `finished' patch.  This is better than recording\n" ++
    "the additions and improvements as separate patches, because then\n" ++
    "whenever the `draft' patch is copied between repositories, you would\n" ++
    "need to make sure all the extra patches are copied, too.\n" ++
    "\n" ++
    "Do not copy draft patches between repositories, because a finished\n" ++
    "patch cannot be copied into a repository that contains a draft of the\n" ++
    "same patch.  If this has already happened, `darcs obliterate' can be\n" ++
    "used to remove the draft patch.\n" ++
    "\n" ++
    -- FIXME: is the following still true in Darcs 2.1? --twb
    "Do not run amend-record in repository that other developers can pull\n" ++
    "from, because if they pull while an amend-record is in progress, their\n" ++
    "repository may be corrupted.\n" ++
    "\n" ++
    "When recording a draft patch, it is a good idea to start the name with\n" ++
    "`DRAFT:' so that other developers know it is not finished.  When\n" ++
    "finished, remove it with `darcs amend-record --edit-long-comment'.\n" ++
    "To change the patch name without starting an editor, use --patch-name.\n" ++
    "\n" ++
    "Like `darcs record', if you call amend-record with files as arguments,\n" ++
    "you will only be asked about changes to those files.  So to amend a\n" ++
    "patch to foo.c with improvements in bar.c, you would run:\n" ++
    "\n" ++
    "    darcs amend-record --match 'touch foo.c' bar.c\n" ++
    "\n" ++
    "It is usually a bad idea to amend another developer's patch.  To make\n" ++
    "amend-record only ask about your own patches by default, you can add\n" ++
    "something like `amend-record match David Roundy' to " ++ globalPrefsDirDoc ++
    "defaults, \n" ++
    "where `David Roundy' is your name.\n"


amendrecord :: DarcsCommand
amendrecord = DarcsCommand
    {
      commandProgramName          = "darcs"
    , commandName                 = "amend-record"
    , commandHelp                 = amendrecordHelp
    , commandDescription          = amendrecordDescription
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = ["[FILE or DIRECTORY]..."]
    , commandCommand              = amendrecordCmd
    , commandPrereq               = amInHashedRepository
    , commandGetArgPossibilities  = listRegisteredFiles
    , commandArgdefaults          = nodefaults
    , commandAdvancedOptions =
        [
          nocompress
        , ignoretimes
        , umaskOption
        , setScriptsExecutableOption
        , patchIndex
        , noPatchIndex
        ]
    , commandBasicOptions =
        [
          matchOneNontag
        , test
        , A.leaveTestDir
        , allInteractive
        , author
        , patchnameOption
        , askdeps
        , askLongComment
        , keepDate
        , lookforadds
        , workingRepoDir
        , amendUnrecord
        , unified
        ]
    }


amendunrecord :: DarcsCommand
amendunrecord = (commandAlias "amend-unrecord" Nothing amendrecord)
    { commandCommand = \fs -> commandCommand amendrecord (AmendUnrecord : fs)
    , commandDescription = "Alias for `darcs " ++ commandName amendrecord ++
      " --unrecord '.\n" ++
      "This allows changes already recorded in the patch to be removed."
    }


amendrecordCmd :: [DarcsFlag]
               -> [String]
               -> IO ()
amendrecordCmd opts [] = doAmendRecord opts Nothing
amendrecordCmd opts args = do
    files <- fixSubPaths opts args
    if null files
      then fail "No valid arguments were given, nothing to do."
      else doAmendRecord opts $ Just files


doAmendRecord :: [DarcsFlag] -> Maybe [SubPath] -> IO ()
doAmendRecord opts files =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \(repository :: Repository p wR wU wR) -> do
    withSelectedPatchFromRepo "amend" repository opts $ \ (_ :> oldp) -> do
        announceFiles files "Amending changes in"
            -- auxiliary function needed because the witness types differ for the isTag case
        pristine <- readRecorded repository
        let go :: forall wU1 . FL (PrimOf p) wR wU1 -> IO ()
            go NilFL | not (hasEditMetadata opts) = putStrLn "No changes!"
            go ch =
              do let context = selectionContextPrim "add"
                                      (intersect [All,Unified] opts)
                                      (Just primSplitter)
                                      (map toFilePath <$> files)
                                      (Just pristine)
                 (chosenPatches :> _) <- runSelection (selectChanges First ch) context
                 addChangesToPatch opts repository oldp chosenPatches
        if not (isTag (info oldp))
              -- amending a normal patch
           then if removeFromAmended opts
                   then do let sel = selectChanges Last (effect oldp)
                               context = selectionContextPrim "unrecord"
                                             (intersect [All,Unified] opts)
                                             (Just primSplitter)
                                             (map toFilePath <$> files)
                                             (Just pristine)
                           (_ :> chosenPrims) <- runSelection sel context
                           let invPrims = reverseRL (invertFL chosenPrims)
                           addChangesToPatch opts repository oldp invPrims
                   else go =<< unrecordedChanges (diffingOpts opts) repository files
              -- amending a tag
           else if hasEditMetadata opts && isNothing files
                        -- the user is not trying to add new changes to the tag so there is
                        -- no reason to warn.
                   then go NilFL
                        -- the user is trying to add new changes to a tag.
                   else do if hasEditMetadata opts
                                -- the user already knows that it is possible to edit tag metadata,
                                -- note that s/he is providing editing options!
                             then putStrLn "You cannot add new changes to a tag."
                                -- the user may not be aware that s/he can edit tag metadata.
                             else putStrLn "You cannot add new changes to a tag, but you are allowed to edit tag's metadata (see darcs help amend-record)."
                           go NilFL


addChangesToPatch :: forall p wR wU wT wX wY . (RepoPatch p, ApplyState p ~ Tree)
                  => [DarcsFlag]
                  -> Repository p wR wU wT
                  -> PatchInfoAnd p wX wT
                  -> FL (PrimOf p) wT wY
                  -> IO ()
addChangesToPatch opts repository oldp chs =
    if (nullFL chs && not (hasEditMetadata opts))
    then putStrLn "You don't want to record anything!"
    else do
         invalidateIndex repository
         withGutsOf repository $ do
           repository' <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking
                                                   (oldp :>: NilFL)
           (mlogf, newp) <- updatePatchHeader opts repository' oldp chs
           setEnvDarcsFiles newp
           repository'' <- tentativelyAddPatch repository' (compression opts) (verbosity opts) YesUpdateWorking newp
           let failmsg = maybe "" (\lf -> "\nLogfile left in "++lf++".") mlogf
           rc <- testTentative repository (runTest opts)
                                          (leaveTestDir opts)
                                          (setScriptsExecutable opts)
                                          (compression opts)
                                          (verbosity opts)
           when (rc /= ExitSuccess) $ do
               when (not $ isInteractive opts) $ exitWith rc `clarifyErrors` failmsg
               putStrLn $ "Looks like you have a bad patch: '" ++ patchDesc newp ++ "'"
               let prompt = "Shall I amend it anyway?"
               yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
               case yn of
                 'y' -> return ()
                 _ -> exitWith rc `clarifyErrors` failmsg
           finalizeRepositoryChanges repository'' (dryRun opts) YesUpdateWorking (compression opts) (not $ NoPatchIndexFlag `elem` opts) `clarifyErrors` failmsg
           maybe (return ()) removeFile mlogf
           putStrLn "Finished amending patch:"
           putDocLn $ description newp


updatePatchHeader :: forall p wX wY wR wU wT . (RepoPatch p, ApplyState p ~ Tree)
                  => [DarcsFlag]
                  -> Repository p wR wU wT
                  -> PatchInfoAnd p wT wX
                  -> FL (PrimOf p) wX wY
                  -> IO (Maybe String, PatchInfoAnd p wT wY)
updatePatchHeader opts repository oldp chs = do

    let newchs = canonizeFL (effect oldp +>+ chs)

    let old_pdeps = getdeps $ hopefully oldp
    newdeps <- if AskDeps `elem` opts
               then askAboutDepends repository newchs opts old_pdeps
               else return old_pdeps

    let old_pinf = info oldp
        prior    = (piName old_pinf, piLog old_pinf)
        make_log = worldReadableTemp "darcs-amend-record"
        old_author = piAuthor old_pinf
    date <- if KeepDate `elem` opts then return (piDateString old_pinf) else getDate opts
    warnIfHijacking opts old_author
    (new_name, new_log, mlogf) <- getLog opts (Just prior) make_log chs
    let new_author = case getAuthor opts of
                     Just a  -> a
                     Nothing -> piAuthor old_pinf
        maybe_invert = if isInverted old_pinf then invertName else id
    new_pinf <- maybe_invert `fmap` patchinfo date new_name
                                              new_author new_log

    let newp = n2pia (adddeps (infopatch new_pinf (fromPrims newchs)) newdeps)

    return (mlogf, newp)


warnIfHijacking :: [DarcsFlag]
                -> String
                -> IO ()
warnIfHijacking opts old_author = do
    authors_here <- getEasyAuthor
    let edit_author = isJust (getAuthor opts)
    unless (edit_author || any (== old_author) authors_here) $ do
      yorn <- askUser $
          "You're not " ++ old_author ++"! Amend anyway? "
      case yorn of ('y':_) -> return ()
                   _       -> exitWith ExitSuccess


hasEditMetadata :: [DarcsFlag]
                -> Bool
hasEditMetadata []                    = False
hasEditMetadata (Author _:_)          = True
hasEditMetadata (LogFile _:_)         = True
hasEditMetadata (PatchName _:_)       = True
hasEditMetadata (EditLongComment:_)   = True
hasEditMetadata (PromptLongComment:_) = True
hasEditMetadata (AskDeps:_)           = True
hasEditMetadata (_:fs)                = hasEditMetadata fs


getAuthor :: [DarcsFlag]
          -> Maybe String
getAuthor (Author a:_)  = Just a
getAuthor (_:as)        = getAuthor as
getAuthor []            = Nothing

