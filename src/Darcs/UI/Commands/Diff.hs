--  Copyright (C) 2003-2004 David Roundy
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

module Darcs.UI.Commands.Diff ( diffCommand ) where

import Prelude hiding ( all )
import System.FilePath.Posix ( takeFileName, (</>) )
import Workaround ( getCurrentDirectory )
import Darcs.Utils ( askEnter, withCurrentDirectory )
import Control.Monad ( when )
import Data.List ( (\\) )
import Storage.Hashed.Plain( writePlainTree )
import Storage.Hashed.Darcs( hashedTreeIO )

import CommandLine ( parseCmd )
import Darcs.UI.External
    ( diffProgram
    , execPipeIgnoreError
    )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInHashedRepository )
import Darcs.UI.Arguments
    ( DarcsFlag(AfterPatch, DiffCmd, DiffFlags, LastN)
    , diffCmdFlag
    , diffflags
    , fixSubPaths
    , matchRange
    , pauseForGui
    , storeInMemory
    , unidiff
    , workingRepoDir
    )
import Darcs.UI.Flags ( isNotUnified, wantGuiPause, toMatchFlags, useCache )
import Darcs.Repository.Flags ( WantGuiPause (..) )
import Darcs.Patch.PatchInfoAnd ( info, n2pia )
import Darcs.Path ( toFilePath, SubPath, simpleSubPath, isSubPathOf )
import Darcs.Global ( darcsdir )
import Darcs.Patch.Match
    ( firstMatch
    , secondMatch
    , matchFirstPatchset
    , matchSecondPatchset
    )
import Darcs.Repository ( withRepository, RepoJob(..), readRepo )
import Darcs.Repository.State ( readUnrecorded, restrictSubpaths
                              , readRecorded, unrecordedChanges
                              , UseIndex(..), ScanKnown(..), applyTreeFilter )
import Darcs.Patch.Witnesses.Ordered ( mapRL, (:>)(..), (+>+), RL(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd )
import Darcs.Patch.Witnesses.Sealed ( unseal, Sealed(..), seal )
import Darcs.Patch ( RepoPatch, apply, listTouchedFiles, invert, fromPrims, anonymous )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Patch.Set ( PatchSet(..), newset2RL )
import Darcs.Patch.Info ( PatchInfo, humanFriendly )
import Darcs.Repository.Lock ( withTempDir )
import Printer ( Doc, putDoc, vcat, empty, ($$) )

#include "impossible.h"


diffDescription :: String
diffDescription = "Create a diff between two versions of the repository."

diffHelp :: String
diffHelp =
 "The `darcs diff' command compares two versions of the working tree of\n" ++
 "the current repository.  Without options, the pristine (recorded) and\n" ++
 "unrecorded working trees are compared.  This is lower-level than\n" ++
 "the `darcs whatsnew' command, since it outputs a line-by-line diff,\n" ++
 "and it is also slower.  As with `darcs whatsnew', if you specify\n" ++
 "files or directories, changes to other files are not listed.\n" ++
 "The command always uses an external diff utility.\n" ++
 "\n" ++
 "With the --patch option, the comparison will be made between working\n" ++
 "trees with and without that patch.  Patches `after' the selected patch\n" ++
 "are not present in either of the compared working trees.  The\n" ++
 "--from-patch and --to-patch options allow the set of patches in the\n" ++
 "`old' and `new' working trees to be specified separately.\n" ++
 "\n" ++
 "The associated tag and match options are also understood, e.g. `darcs\n" ++
 "diff --from-tag 1.0 --to-tag 1.1'.  All these options assume an\n" ++
 "ordering of the patch set, so results may be affected by operations\n" ++
 "such as `darcs optimize --reorder'.\n" ++
 "\n" ++
 "diff(1) is called with the arguments -rN.  The --unified option causes\n" ++
 "-u to be passed to diff(1).  An additional argument can be passed\n" ++
 "using --diff-opts, such as --diff-opts=-ud or --diff-opts=-wU9.\n" ++
 "\n" ++
 "The --diff-command option can be used to specify an alternative\n" ++
 "utility, such as meld (GNOME) or opendiff (OS X).  Arguments may be\n" ++
 "included, separated by whitespace.  The value is not interpreted by a\n" ++
 "shell, so shell constructs cannot be used.  The arguments %1 and %2\n" ++
 "MUST be included, these are substituted for the two working trees\n" ++
 "being compared.  If this option is used, --diff-opts is ignored.\n"

diffCommand :: DarcsCommand
diffCommand = DarcsCommand {commandProgramName = "darcs",
                             commandName = "diff",
                             commandHelp = diffHelp,
                             commandDescription = diffDescription,
                             commandExtraArgs = -1,
                             commandExtraArgHelp
                                 = ["[FILE or DIRECTORY]..."],
                             commandCommand = diffCmd,
                             commandPrereq = amInHashedRepository,
                             commandGetArgPossibilities = return [],
                             commandArgdefaults = nodefaults,
                             commandAdvancedOptions =
                                [ pauseForGui
                                ],
                             commandBasicOptions =
                                [ matchRange
                                , diffCmdFlag
                                , diffflags
                                , unidiff
                                , workingRepoDir
                                , storeInMemory
                                ]
                           }

getDiffOpts :: [DarcsFlag] -> [String]
getDiffOpts opts | isNotUnified opts = get_nonU_diff_opts opts
                 | otherwise         = "-u" : get_nonU_diff_opts opts
    where get_nonU_diff_opts (DiffFlags f:fs) = f : get_nonU_diff_opts fs
          get_nonU_diff_opts (_:fs) = get_nonU_diff_opts fs
          get_nonU_diff_opts [] = []

-- | Returns the command we should use for diff as a tuple (command, arguments).
-- This will either be whatever the user specified via --diff-command  or the
-- default 'diffProgram'.  Note that this potentially involves parsing the
-- user's diff-command, hence the possibility for failure with an exception.
getDiffCmdAndArgs :: String -> [DarcsFlag] -> String -> String
                      -> Either String (String, [String])
getDiffCmdAndArgs cmd opts f1 f2 = helper opts where
  helper (DiffCmd c:_) =
    case parseCmd [ ('1', f1) , ('2', f2) ] c of
    Left err        -> Left $ show err
    Right ([],_)    -> bug $ "parseCmd should never return empty list"
    Right ((h:t),_) -> Right (h,t)
  helper [] = -- if no command specified, use 'diff'
    Right (cmd, ("-rN":getDiffOpts opts++[f1,f2]))
  helper (_:t) = helper t

diffCmd :: [DarcsFlag] -> [String] -> IO ()
diffCmd opts args
  | not (null [i | LastN i <- opts]) &&
      not (null [p | AfterPatch p <- opts]) =
        fail $ "using --patch and --last at the same time with the 'diff'" ++
          " command doesn't make sense. Use --from-patch to create a diff" ++
          " from this patch to the present, or use just '--patch' to view" ++
          " this specific patch."
  | null args = doDiff opts Nothing
  | otherwise = doDiff opts . Just =<< fixSubPaths opts args

doDiff :: [DarcsFlag] -> Maybe [SubPath] ->  IO ()
doDiff opts msubpaths = withRepository (useCache opts) $ RepoJob $ \repository -> do
  formerdir <- getCurrentDirectory

  thename <- return $ takeFileName formerdir

  patchset <- readRepo repository

  unrecorded <- fromPrims `fmap` unrecordedChanges (UseIndex, ScanKnown) repository msubpaths
  unrecorded' <- n2pia `fmap` anonymous unrecorded

  let matchFlags = toMatchFlags opts
  Sealed all <- return $ case (secondMatch matchFlags, patchset) of
    (True, _) -> seal patchset
    (False, (PatchSet untagged tagged)) -> seal $ PatchSet (unrecorded' :<: untagged) tagged

  Sealed ctx <- return $ if firstMatch matchFlags
                            then matchFirstPatchset matchFlags patchset
                            else seal patchset

  Sealed match <- return $ if secondMatch matchFlags
                             then matchSecondPatchset matchFlags patchset
                             else seal all

  (_ :> todiff) <- return $ findCommonWithThem match ctx
  (_ :> tounapply) <- return $ findCommonWithThem all match

  base <- if secondMatch matchFlags
           then readRecorded repository
           else readUnrecorded repository Nothing

  let touched = map (fromJust . simpleSubPath) $ listTouchedFiles todiff
      files = case msubpaths of
               Nothing -> touched
               Just subpaths -> concatMap (\s -> filter (isSubPathOf s) touched) subpaths
  relevant <- restrictSubpaths repository files
  let filt = applyTreeFilter relevant . snd
      ppath = darcsdir </> "pristine.hashed"

  oldtree <- filt `fmap` hashedTreeIO
                (apply . invert $ unsafeCoercePEnd todiff +>+ tounapply) base ppath
  newtree <- filt `fmap` hashedTreeIO
                (apply . invert $ tounapply) base ppath

  withTempDir ("old-"++thename) $ \odir -> do
    withTempDir ("new-"++thename) $ \ndir -> do
      withCurrentDirectory formerdir $ do
        writePlainTree oldtree (toFilePath odir)
        writePlainTree newtree (toFilePath ndir)
        thediff <- withCurrentDirectory (toFilePath odir ++ "/..") $
                       rundiff (takeFileName $ toFilePath odir) (takeFileName $ toFilePath ndir)
        morepatches <- readRepo repository
        putDoc $ changelog (getDiffInfo opts morepatches)
               $$ thediff
    where rundiff :: String -> String -> IO Doc
          rundiff f1 f2 = do
            cmd <- diffProgram
            case getDiffCmdAndArgs cmd opts f1 f2 of
             Left err -> fail err
             Right (d_cmd, d_args) ->
              let pausingForGui = (wantGuiPause opts == YesWantGuiPause) in
              do when pausingForGui $ putStrLn $
                   "Running command '" ++ unwords (d_cmd:d_args) ++ "'"
                 output <- execPipeIgnoreError d_cmd d_args empty
                 when pausingForGui $
                    askEnter "Hit return to move on..."
                 return output

getDiffInfo :: RepoPatch p => [DarcsFlag] -> PatchSet p wStart wX -> [PatchInfo]
getDiffInfo opts ps =
    let matchFlags = toMatchFlags opts
        infos = mapRL info . newset2RL
        handle (match_cond, do_match)
          | match_cond matchFlags = unseal infos (do_match matchFlags ps)
          | otherwise = infos ps
    in handle (secondMatch, matchSecondPatchset)
         \\ handle (firstMatch, matchFirstPatchset)

changelog :: [PatchInfo] -> Doc
changelog pis = vcat $ map humanFriendly pis

