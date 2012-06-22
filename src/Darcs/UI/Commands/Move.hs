--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.UI.Commands.Move ( move, mv ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( when, unless, zipWithM_ )
import Data.Maybe ( catMaybes )
import Darcs.SignalHandler ( withSignalsBlocked )

import Darcs.UI.Commands
    ( DarcsCommand(..), nodefaults, commandAlias, amInHashedRepository )
import Darcs.UI.Arguments
    ( DarcsFlag()
    , maybeFixSubPaths
    , fixSubPaths
    , workingRepoDir
    , allowProblematicFilenames
    , umaskOption
    )
import Darcs.UI.Flags ( doAllowCaseOnly, doAllowWindowsReserved, useCache, dryRun, umask)
import Darcs.Repository.Flags ( UpdateWorking (..) )
import System.FilePath.Posix ( (</>), takeFileName )
import System.Directory ( renameDirectory )
import Workaround ( renameFile )
import Darcs.Repository.State ( readRecordedAndPending, readRecorded )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , addToPending
    , listFiles
    )
import Darcs.Witnesses.Ordered ( FL(..), toFL )
import Darcs.Witnesses.Sealed ( Sealed(..), unseal, freeGap, FreeLeft, unFreeLeft )
import Darcs.Global ( debugMessage )
import qualified Darcs.Patch
import Darcs.Patch ( RepoPatch, PrimPatch )
import Darcs.Patch.Apply( ApplyState )
import Data.List ( nub, sort )
import qualified System.FilePath.Windows as WindowsFilePath

import Darcs.Utils( treeHas, treeHasDir, treeHasAnycase, treeHasFile )
import Storage.Hashed.Tree( Tree, modifyTree )
import Storage.Hashed.Plain( readPlainTree )
import Darcs.Path
    ( floatPath
    , fp2fn
    , fn2fp
    , superName
    , SubPath()
    , toFilePath
    )


moveDescription :: String
moveDescription = "Move or rename files."

moveHelp :: String
moveHelp =
 "Darcs cannot reliably distinguish between a file being deleted and a\n" ++
 "new one added, and a file being moved.  Therefore Darcs always assumes\n" ++
 "the former, and provides the `darcs mv' command to let Darcs know when\n" ++
 "you want the latter.  This command will also move the file in the\n" ++
 "working tree (unlike `darcs remove'), unless it has already been moved.\n" ++
 "\n" ++
 -- Note that this paragraph is very similar to one in ./Add.lhs.
 "Darcs will not rename a file if another file in the same folder has\n" ++
 "the same name, except for case.  The --case-ok option overrides this\n" ++
 "behaviour.  Windows and OS X usually use filesystems that do not allow\n" ++
 "files a folder to have the same name except for case (for example,\n" ++
 "`ReadMe' and `README').  If --case-ok is used, the repository might be\n" ++
 "unusable on those systems!\n"

move :: DarcsCommand
move = DarcsCommand {commandProgramName = "darcs",
                   commandName = "move",
                   commandHelp = moveHelp,
                   commandDescription = moveDescription,
                   commandExtraArgs = -1,
                   commandExtraArgHelp = ["<SOURCE> ... <DESTINATION>"],
                   commandCommand = moveCmd,
                   commandPrereq = amInHashedRepository,
                   commandGetArgPossibilities = listFiles,
                   commandArgdefaults = nodefaults,
                   commandAdvancedOptions = [umaskOption],
                   commandBasicOptions = [allowProblematicFilenames, workingRepoDir]}

moveCmd :: [DarcsFlag] -> [String] -> IO ()
moveCmd opts args
  | length args < 2 =
      fail $ "The `darcs move' command requires at least two arguments."
  | length args == 2 = do
      xs <- maybeFixSubPaths opts args
      case xs of
        [Just from, Just to]
          | from == to -> fail "Cannot rename a file or directory onto itself!"
          | toFilePath from == "" -> fail "Cannot move the root of the repository"
          | otherwise -> moveFile opts from to
        _ -> fail "Both source and destination must be valid."
  | otherwise = let (froms, to) = (init args, last args) in do
      x <- head <$> maybeFixSubPaths opts [to]
      case x of
        Nothing -> fail "Invalid destination directory."
        Just to' -> do
          xs <- nub . sort <$> fixSubPaths opts froms
          if to' `elem` xs
            then fail "Cannot rename a file or directory onto itself!"
            else case xs of
              [] -> fail "Nothing to move."
              froms' -> moveFilesToDir opts froms' to'

data FileKind = Dir | File
              deriving (Show, Eq)

data FileStatus =
  Nonexistant
  | Unadded FileKind
  | Shadow FileKind -- ^ known to darcs, but absent in working copy
  | Known FileKind
  deriving Show

fileStatus :: Tree IO -- ^ tree of the working directory
           -> Tree IO -- ^ tree of recorded and pending changes
           -> Tree IO -- ^ tree of recorded changes
           -> FilePath
           -> IO FileStatus
fileStatus work cur rec fp = do
  existsInCur <- treeHas cur fp
  existsInRec <- treeHas rec fp
  existsInWork <- treeHas work fp
  case (existsInRec, existsInCur, existsInWork) of
    (_, True, True) -> do
      isDirCur <- treeHasDir cur fp
      isDirWork <- treeHasDir work fp
      unless (isDirCur == isDirWork) . fail $ "don't know what to do with " ++ fp
      return . Known $ if isDirCur then Dir else File

    (_, False, True) -> do
      isDir <- treeHasDir work fp
      if isDir
        then return $ Unadded Dir
        else return $ Unadded File
    (False, False, False) -> return Nonexistant
    (_, _, False) -> do
      isDir <- treeHasDir cur fp
      if isDir
        then return $ Shadow Dir
        else return $ Shadow File

-- | Takes two filenames (as 'Subpath'), and tries to move the first
-- into/onto the second. Needs to guess what that means: renaming or moving
-- into a directory, and whether it is a post-hoc move.
moveFile :: [DarcsFlag] -> SubPath -> SubPath -> IO ()
moveFile opts old new = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts)  $ RepoJob $ \repository -> do
  work <- readPlainTree "."
  cur <- readRecordedAndPending repository
  rec <- readRecorded repository
  let old_fp = toFilePath old
      new_fp = toFilePath new
  new_fs <- fileStatus work cur rec new_fp
  old_fs <- fileStatus work cur rec old_fp
  case (old_fs, new_fs) of
    (Nonexistant, _) -> fail $ old_fp ++ " does not exist."
    (Unadded k, _) -> fail $ show k ++ " " ++ old_fp ++ " is unadded."
    (Known _, Nonexistant) -> simpleMove repository opts cur work old_fp new_fp
    (Known _, Shadow _) -> simpleMove repository opts cur work old_fp new_fp
    (_, Nonexistant) -> fail $ old_fp ++ " is not in the repository."
    (Known _, Known Dir) -> moveToDir repository opts [old_fp] new_fp
    (Known _, Unadded Dir) -> fail $ new_fp ++ " is not known to darcs; please add it to the repository."
    (Known _, _) -> fail $ new_fp ++ " already exists."
    (Shadow k, Unadded k') | k == k' -> simpleMove repository opts cur work old_fp new_fp
    (Shadow _, Known Dir) -> moveToDir repository opts [old_fp] new_fp
    (Shadow k, _) -> fail $ "cannot move " ++ show k ++ " " ++ old_fp ++ " into " ++ new_fp ++ " : " ++
                                          "did you already move it elsewhere?"

simpleMove :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT
           -> [DarcsFlag] -> Tree IO -> Tree IO -> FilePath -> FilePath
           -> IO ()
simpleMove repository opts cur work old_fp new_fp = do
    addpatch <- checkNewAndOldFilenames opts cur work (old_fp,new_fp)
    withSignalsBlocked $ do
      case unFreeLeft <$> addpatch of
        Nothing -> addToPending repository YesUpdateWorking (Darcs.Patch.move old_fp new_fp :>: NilFL)
        Just (Sealed p) -> addToPending repository YesUpdateWorking (p :>: Darcs.Patch.move old_fp new_fp :>: NilFL)
      moveFileOrDir work old_fp new_fp

moveFilesToDir :: [DarcsFlag] -> [SubPath] -> SubPath -> IO ()
moveFilesToDir opts froms to = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repo ->
  moveToDir repo opts (map toFilePath froms) $ toFilePath to

moveToDir :: (RepoPatch p, ApplyState p ~ Tree)
          => Repository p wR wU wT -> [DarcsFlag] -> [FilePath] -> FilePath -> IO ()
moveToDir repository opts moved finaldir =
  let movefns = map takeFileName moved
      movetargets = map (finaldir </>) movefns
      movepatches = zipWith (\a b -> freeGap (Darcs.Patch.move a b)) moved movetargets
  in do
    cur <- readRecordedAndPending repository
    work <- readPlainTree "."
    addpatches <- mapM (checkNewAndOldFilenames opts cur work) $ zip moved movetargets
    withSignalsBlocked $ do
      unseal (addToPending repository YesUpdateWorking) $ toFL $ catMaybes addpatches ++ movepatches
      zipWithM_ (moveFileOrDir work) moved movetargets

checkNewAndOldFilenames
    :: PrimPatch prim => [DarcsFlag] -> Tree IO -> Tree IO -> (FilePath, FilePath) -> IO (Maybe (FreeLeft prim))
checkNewAndOldFilenames opts cur work (old,new) = do
  unless (doAllowWindowsReserved opts || WindowsFilePath.isValid new) $
     fail $ "The filename " ++ new ++ " is not valid under Windows.\n" ++
            "Use --reserved-ok to allow such filenames."
  has_work <- treeHas work old
  maybe_add_file_thats_been_moved <-
     if has_work -- We need to move the object
     then do has_target <- treeHasDir work (fn2fp $ superName $ fp2fn new)
             unless has_target $
                    fail $ "The target directory " ++
                             (fn2fp $ superName $ fp2fn new)++
                             " isn't known in working directory, did you forget to add it?"
             has_new <- it_has work
             when has_new $ fail $ already_exists "working directory"
             return Nothing
     else do
       has_new <- treeHas work new
       has_cur_dir <- treeHasDir cur old
       unless has_new $ fail $ doesnt_exist "working directory"
       let add_patch = if has_cur_dir
                       then Darcs.Patch.adddir old
                       else Darcs.Patch.addfile old
       return (Just (freeGap (add_patch)))
  has_target <- treeHasDir cur (fn2fp $ superName $ fp2fn new)
  unless has_target $
    fail $ "The target directory " ++
     (fn2fp $ superName $ fp2fn new)++
     " isn't known in working directory, did you forget to add it?"
  has_new <- it_has cur
  when has_new $ fail $ already_exists "repository"
  return maybe_add_file_thats_been_moved
    where it_has s = treeHas_case (modifyTree s (floatPath old) Nothing) new
          treeHas_case = if doAllowCaseOnly opts then treeHas else treeHasAnycase
          already_exists what_slurpy =
              if doAllowCaseOnly opts
              then "A file or dir named "++new++" already exists in "
                       ++ what_slurpy ++ "."
              else "A file or dir named "++new++" (or perhaps differing"++
                       " only in case)\nalready exists in "++
                       what_slurpy ++ ".\n"++
                   "Use --case-ok to allow files differing only in case."
          doesnt_exist what_slurpy =
              "There is no file or dir named " ++ old ++
              " in the "++ what_slurpy ++ "."

moveFileOrDir :: Tree IO -> FilePath -> FilePath -> IO ()
moveFileOrDir work old new = do
  has_file <- treeHasFile work old
  has_dir <- treeHasDir work old
  when has_file $ do debugMessage $ unwords ["renameFile",old,new]
                     renameFile old new
  when has_dir $ do debugMessage $ unwords ["renameDirectory",old,new]
                    renameDirectory old new

mv :: DarcsCommand
mv = commandAlias "mv" Nothing move

