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

{-# LANGUAGE CPP #-}


module Darcs.UI.Commands.WhatsNew
    (
      whatsnew
    , status
    ) where

import Control.Applicative ( (<$>) )
import Data.List ( delete )
import System.Exit ( ExitCode(..), exitWith )

import Storage.Hashed.Tree( Tree )

import Darcs.UI.Arguments
    ( DarcsFlag(..)
    , workingRepoDir
    , lookforadds
    , ignoretimes
    , noskipBoring
    , unified
    , summary
    , fixSubPaths
    )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, commandAlias, amInRepository )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.Repository.Diff( treeDiff )
import Darcs.UI.Flags( isUnified, diffingOpts, useCache )
import Darcs.Patch ( RepoPatch, PrimPatch, PrimOf, plainSummaryPrims,
                     primIsHunk, applyToTree )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Permutations ( partitionRL )
import Darcs.Patch.Prim.Class ( PrimDetails(..) )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.TouchesFiles( choosePreTouching )
import Darcs.Path( SubPath, toFilePath )
import Darcs.Repository
    ( Repository
    , withRepository
    , RepoJob(..)
    , unrecordedChanges
    , readRecorded
    , listRegisteredFiles
    )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.UI.PrintPatch ( printPatch, contextualPrintPatch )
import Darcs.Witnesses.Ordered ( FL(..), reverseRL, reverseFL, (:>)(..) )
import Darcs.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Printer ( putDocLn, renderString, vcat, text )

whatsnew :: DarcsCommand
whatsnew = DarcsCommand { commandProgramName = "darcs"
                        , commandName = "whatsnew"
                        , commandHelp = whatsnewHelp
                        , commandDescription = whatsnewDescription
                        , commandExtraArgs = -1
                        , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
                        , commandCommand = whatsnewCmd
                        , commandPrereq = amInRepository
                        , commandGetArgPossibilities = listRegisteredFiles
                        , commandArgdefaults = nodefaults
                        , commandAdvancedOptions = [ignoretimes, noskipBoring]
                        , commandBasicOptions = [ summary
                                                , unified
                                                , lookforadds
                                                , workingRepoDir
                                                ]
                        }

whatsnewDescription :: String
whatsnewDescription = "List unrecorded changes in the working tree."

whatsnewHelp :: String
whatsnewHelp =
 "The `darcs whatsnew' command lists unrecorded changes to the working\n" ++
 "tree.  If you specify a set of files and directories, only unrecorded\n" ++
 "changes to those files and directories are listed.\n" ++
 "\n" ++
 "With the --summary option, the changes are condensed to one line per\n" ++
 "file, with mnemonics to indicate the nature and extent of the change.\n" ++
 "The --look-for-adds option causes candidates for `darcs add' to be\n" ++
 "included in the summary output.  Summary mnemonics are as follows:\n" ++
 "\n" ++
 "  `A f' and `A d/' respectively mean an added file or directory.\n" ++
 "  `R f' and `R d/' respectively mean a removed file or directory.\n" ++
 "  `M f -N +M rP' means a modified file, with N lines deleted, M\n" ++
 "  lines added, and P lexical replacements.\n" ++
 "  `f -> g' means a moved file or directory.\n" ++
 "  `a f' and `a d/' respectively mean a new, but unadded, file or\n" ++
 "  directory, when using --look-for-adds.\n" ++
 "\n" ++
 "  An exclamation mark (!) as in `R! foo.c', means the hunk is known to\n" ++
 "  conflict with a hunk in another patch.  The phrase `duplicated'\n" ++
 "  means the hunk is known to be identical to a hunk in another patch.\n" ++
 "\n" ++
 "By default, `darcs whatsnew' uses Darcs' internal format for changes.\n" ++
 "To see some context (unchanged lines) around each change, use the\n" ++
 "--unified option.  To view changes in conventional `diff' format, use\n" ++
 "the `darcs diff' command; but note that `darcs whatsnew' is faster.\n" ++
 "\n" ++
 "This command exits unsuccessfully (returns a non-zero exit status) if\n" ++
 "there are no unrecorded changes.\n"

whatsnewCmd :: [DarcsFlag] -> [String] -> IO ()
whatsnewCmd opts args =
   withRepository (useCache opts) $ RepoJob $ \(repo :: Repository p wR wU wR) -> do
    files <- if null args
                 then return Nothing
                 else Just <$> fixSubPaths opts args
    let isLookForAdds = LookForAdds `elem` opts && NoSummary `notElem` opts
        -- LookForAdds implies Summary, unless it's explcitly disabled.
        optsModifier = if isLookForAdds
                           then (Summary :) . (LookForAdds `delete`)
                           else id
        opts' = optsModifier opts
    Sealed noLookChanges <- filteredUnrecordedChanges opts' repo files
    pristine <- readRecorded repo
    -- If we are looking for adds, return the corresponding FL of changes.
    Sealed unaddedNewPathsPs <- if isLookForAdds
        then do
            -- Use opts not opts', here, since we *do* want to look for adds.
            Sealed lookChanges <- filteredUnrecordedChanges opts repo files
            noLookAddsTree <- applyAddPatchesToPristine noLookChanges pristine
            lookAddsTree <- applyAddPatchesToPristine lookChanges pristine
            ftf <- filetypeFunction
            -- Return the patches that create files/dirs that aren't yet added.
            unFreeLeft <$> treeDiff ftf noLookAddsTree lookAddsTree
        else return (Sealed NilFL)
    announceFiles files "What's new in"
    exitOnNoChanges (unaddedNewPathsPs, noLookChanges)
    printChanges opts' pristine noLookChanges
    printUnaddedPaths unaddedNewPathsPs
  where
    -- |Filter out hunk patches (leaving add patches) and return the tree
    -- resulting from applying the filtered patches to the pristine tree.
    applyAddPatchesToPristine ps pristine = do
        adds :> _ <- return $ partitionRL primIsHunk $ reverseFL ps
        applyToTree (reverseRL adds) pristine

    exitOnNoChanges :: (FL p wX wY, FL p wU wV) -> IO ()
    exitOnNoChanges (NilFL, NilFL) = do putStrLn "No changes!"
                                        exitWith $ ExitFailure 1
    exitOnNoChanges _ = return ()

    printUnaddedPaths :: PrimPatch p => FL p wX wY -> IO ()
    printUnaddedPaths NilFL = return ()
    printUnaddedPaths ps =
        putDocLn . lowercaseAs . renderString . plainSummaryPrims $ ps

    -- Make any add markers lowercase, to distinguish new-but-unadded files
    -- from those that are unrecorded, but added.
    lowercaseAs x = vcat $ map (text . lowercaseA) $ lines x
    lowercaseA ('A' : x) = 'a' : x
    lowercaseA x = x

    -- |Appropriately print changes, according to the passed flags.
    printChanges :: (PatchListFormat p, IsHunk p, Patchy p, ShowPatch p, PrimDetails p,
                 ApplyState p ~ Tree) => [DarcsFlag] -> Tree IO -> FL p wX wY
                 -> IO ()
    printChanges opts' pristine changes
        | Summary `elem` opts' = putDocLn $ plainSummaryPrims changes
        | isUnified opts' = contextualPrintPatch pristine changes
        | otherwise = printPatch changes

    -- |return the unrecorded changes that affect an optional list of paths.
    filteredUnrecordedChanges :: (RepoPatch p, ApplyState p ~ Tree,
                              ApplyState (PrimOf p) ~ Tree) => [DarcsFlag]
                              -> Repository p wR wU wT -> Maybe [SubPath]
                              -> IO (Sealed (FL (PrimOf p) wT))
    filteredUnrecordedChanges  opts' repo files =
        let filePaths = map toFilePath <$> files in
        let diffOpts = diffingOpts opts' in
        choosePreTouching filePaths <$> unrecordedChanges diffOpts repo files

-- |status is an alias for whatsnew, with implicit Summary and LookForAdds
-- flags. We override the default description, to include the implicit flags.
status :: DarcsCommand
status = statusAlias { commandCommand = statusCmd
                     , commandDescription = statusDesc
                     }
  where
    statusAlias = commandAlias "status" Nothing whatsnew
    statusCmd fs = commandCommand whatsnew (Summary : LookForAdds : fs)
    statusDesc = "Alias for `darcs " ++ commandName whatsnew ++ " -ls '."
