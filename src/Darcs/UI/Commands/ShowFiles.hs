--  Copyright (C) 2005 Florian Weimer
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
module Darcs.UI.Commands.ShowFiles ( showFiles
                                , manifestCmd, toListManifest -- for alias
                                , manifest
                                ) where
import Darcs.UI.Arguments ( DarcsFlag(..), workingRepoDir,
                        files, directories, pending, nullFlag, matchOne )
import Darcs.UI.Flags ( toMatchFlags, useCache, compression )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInRepository )
import Darcs.Repository ( Repository, withRepository,
                          RepoJob(..) )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Repository.State ( readRecorded, readRecordedAndPending )
import Storage.Hashed.Tree( Tree, TreeItem(..), list, expand )
import Darcs.Path( anchorPath )
import Storage.Hashed.Plain( readPlainTree )
import System.FilePath ( splitDirectories )

import Data.List( isPrefixOf )

import Darcs.Patch.Match ( haveNonrangeMatch )
import Darcs.Repository.Match ( getNonrangeMatch )
import Darcs.Repository.Lock ( withDelayedDir )

showFilesDescription :: String
showFilesDescription = "Show version-controlled files in the working copy."

showFilesHelp :: String
showFilesHelp =
 "The `darcs show files' command lists those files and directories in\n" ++
 "the working tree that are under version control.  This command is\n" ++
 "primarily for scripting purposes; end users will probably want `darcs\n" ++
 "whatsnew --summary'.\n" ++
 "\n" ++
 "A file is `pending' if it has been added but not recorded.  By\n" ++
 "default, pending files (and directories) are listed; the --no-pending\n" ++
 "option prevents this.\n" ++
 "\n" ++
 "By default `darcs show files' lists both files and directories, but\n" ++
 "the alias `darcs show manifest' only lists files.  The --files,\n" ++
 "--directories, --no-files and --no-directories modify this behaviour.\n" ++
 "\n" ++
 "By default entries are one-per-line (i.e. newline separated).  This\n" ++
 "can cause problems if the files themselves contain newlines or other\n" ++
 "control characters.  To get aroudn this, the --null option uses the\n" ++
 "null character instead.  The script interpreting output from this\n" ++
 "command needs to understand this idiom; `xargs -0' is such a command.\n" ++
 "\n" ++
 "For example, to list version-controlled files by size:\n" ++
 "\n" ++
 "    darcs show files -0 | xargs -0 ls -ldS\n"

showFiles :: DarcsCommand
showFiles = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "files",
  commandHelp = showFilesHelp,
  commandDescription = showFilesDescription,
  commandExtraArgs = -1,
  commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
  commandCommand = manifestCmd toListFiles,
  commandPrereq = amInRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, pending, nullFlag, matchOne,
                          workingRepoDir] }

toListFiles, toListManifest :: [DarcsFlag] -> Tree m -> [FilePath]
toListFiles    opts = filesDirs (NoFiles `notElem` opts) (NoDirectories `notElem` opts)
toListManifest opts = filesDirs (NoFiles `notElem` opts) (Directories `elem` opts)

filesDirs :: Bool -> Bool -> Tree m -> [FilePath]
filesDirs False False _ = []
filesDirs False True  t = "." : [ anchorPath "." p | (p, SubTree _) <- list t ]
filesDirs True  False t = [ anchorPath "." p | (p, File _) <- list t ]
filesDirs True  True  t = "." : (map (anchorPath "." . fst) $ list t)

manifest :: [DarcsFlag] -> [String] -> IO [FilePath]
manifest = manifestHelper toListFiles

manifestCmd :: ([DarcsFlag] -> Tree IO -> [FilePath]) -> [DarcsFlag] -> [String] -> IO ()
manifestCmd to_list opts argList = do
    mapM_ output =<< manifestHelper to_list opts argList
  where
    output_null name = do { putStr name ; putChar '\0' }
    output = if NullFlag `elem` opts then output_null else putStrLn

manifestHelper :: ([DarcsFlag] -> Tree IO -> [FilePath]) -> [DarcsFlag] -> [String] -> IO [FilePath]
manifestHelper to_list opts argList = do
    list' <- (to_list opts) `fmap` withRepository (useCache opts) (RepoJob myslurp)
    case argList of
        []       -> return list'
        prefixes -> return (onlysubdirs prefixes list')
    where myslurp :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wR -> IO (Tree IO)
          myslurp r = do let fRevisioned = haveNonrangeMatch (toMatchFlags opts)
                             fPending = Pending `elem` opts
                             fNoPending = NoPending `elem` opts
                       -- this covers all 8 options
                         expand =<< case (fRevisioned,fPending,fNoPending) of
                            (True,False,_) -> slurpRevision opts r
                            (True,True,_) -> error $ "can't mix revisioned and pending flags"
                            (False,False,True) -> readRecorded r
                            (False,_,False) -> readRecordedAndPending r -- pending is default
                            (False,True,True) -> error $ "can't mix pending and no-pending flags"
          isParentDir a' b' =
            let a = splitDirectories a'
                b = splitDirectories b'
            in (a `isPrefixOf` b) || (("." : a) `isPrefixOf` b)
          onlysubdirs dirs = filter (\p -> any (`isParentDir` p) dirs)

slurpRevision :: (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> Repository p wR wU wR -> IO (Tree IO)
slurpRevision opts r = withDelayedDir "revisioned.showfiles" $ \_ -> do
  getNonrangeMatch r (compression opts) (toMatchFlags opts)
  expand =<< readPlainTree "."

