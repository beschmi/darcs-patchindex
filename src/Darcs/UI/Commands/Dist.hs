--  Copyright (C) 2003 David Roundy
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
-- Module      : Darcs.UI.Commands.Dist
-- Copyright   : 2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.Dist
    (
      dist
    ) where

import Prelude hiding ( writeFile )

import Data.ByteString.Lazy ( writeFile )
import Data.Char ( isAlphaNum )
import Control.Monad ( when )
import System.Directory ( setCurrentDirectory )
import System.Cmd ( system )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath.Posix ( takeFileName, (</>) )

import Workaround ( getCurrentDirectory )
import Codec.Archive.Tar ( pack, write )
import Codec.Archive.Tar.Entry ( entryPath )
import Codec.Compression.GZip ( compress )

import Darcs.UI.Flags ( toMatchFlags, dryRun, useCache, umask, compression )
import Darcs.UI.Arguments ( DarcsFlag(Verbose, Quiet, DistName, SetScriptsExecutable), distnameOption,
                         workingRepoDir, matchOne, storeInMemory,
                         setScriptsExecutableOption )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInHashedRepository )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Patch.Match
    ( haveNonrangeMatch
    , firstMatch
    )
import Darcs.Repository.Match
    ( getFirstMatch
    , getNonrangeMatch
    )
import Darcs.Repository ( withRepoReadLock, RepoJob(..),
                          setScriptsExecutable,
                          createPartialsPristineDirectoryTree )
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Path ( AbsolutePath, toFilePath )
import Darcs.Utils ( withCurrentDirectory )


distDescription :: String
distDescription = "Create a distribution tarball."


distHelp :: String
distHelp =
    "The `darcs dist' command creates a compressed archive (a `tarball') in\n" ++
     "the repository's root directory, containing the recorded state of the\n" ++
     "working tree (unrecorded changes and the _darcs directory are\n" ++
     "excluded).\n" ++
     "\n" ++
     "If a predist command is set (see `darcs setpref'), that command will\n" ++
     "be run on the tarball contents prior to archiving.  For example,\n" ++
     "autotools projects would set it to `autoconf && automake'.\n" ++
     "\n" ++
     "By default, the tarball (and the top-level directory within the\n" ++
     "tarball) has the same name as the repository, but this can be\n" ++
     "overridden with the --dist-name option.\n"

     -- FIXME: this is tedious and ugly.
     {-
     ++ "\n" ++
     "Suppose you use a version numbering scheme `major.minor.patch', and\n" ++
     "you tag each release `major.minor'.  You can then calculate the\n" ++
     "version number by taking the newest tag and appending a dot and the\n" ++
     "number of patches since that tag.  If you use the directory name as\n" ++
     "the project name, you can make tarballs of the form name-version.tgz\n" ++
     "using the following shell script:\n" ++
     "\n" ++
     "  major_minor=$(darcs show tags | head -1) &&\n" ++
     "  patch_level=$(($(darcs changes --count --from-tag .) - 1)) &&\n" ++
     "  version=$major_minor.$patch_level &&\n" ++
     "  project=${PWD##*/} &&\n" ++
     "  darcs dist --dist-name \"$project\"-\"$version\".tar.gz\n"
     -}


dist :: DarcsCommand
dist = DarcsCommand
    {
      commandProgramName = "darcs"
    , commandName = "dist"
    , commandHelp = distHelp
    , commandDescription = distDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = distCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions =
        [
          distnameOption
        , workingRepoDir
        , matchOne
        , setScriptsExecutableOption
        , storeInMemory
        ]
    }


distCmd :: [DarcsFlag]
        -> [String]
        -> IO ()
distCmd opts _ = withRepoReadLock (dryRun opts) (useCache opts) (umask opts) $ RepoJob $ \repository -> do
  let matchFlags = toMatchFlags opts
  formerdir <- getCurrentDirectory
  let distname = getDistName formerdir [x | DistName x <- opts]
  predist <- getPrefval "predist"
  let resultfile = formerdir </> distname ++ ".tar.gz"
  withTempDir "darcsdist" $ \tempdir -> do
    setCurrentDirectory formerdir
    withTempDir (toFilePath tempdir </> takeFileName distname) $ \ddir -> do
      if haveNonrangeMatch matchFlags
        then
            if firstMatch matchFlags
                then withCurrentDirectory ddir $ getFirstMatch repository (compression opts) matchFlags
                else withCurrentDirectory ddir $ getNonrangeMatch repository (compression opts) matchFlags
        else createPartialsPristineDirectoryTree repository (compression opts) [""] (toFilePath ddir)
      ec <- case predist of Nothing -> return ExitSuccess
                            Just pd -> system pd
      if ec == ExitSuccess
          then
              do
              withCurrentDirectory ddir $
                  when (SetScriptsExecutable `elem` opts) setScriptsExecutable
              doDist opts tempdir ddir resultfile
          else
              do
              putStrLn "Dist aborted due to predist failure"
              exitWith ec


-- | This function performs the actual distribution action itself.
-- NB - it does /not/ perform the pre-dist, that should already
-- have completed successfully before this is invoked.
doDist :: [DarcsFlag] -> AbsolutePath -> AbsolutePath -> FilePath -> IO ()
doDist opts tempdir ddir resultfile = do
    setCurrentDirectory (toFilePath tempdir)
    let safeddir = safename $ takeFileName $ toFilePath ddir
    entries <- pack "." [safeddir]
    when (Verbose `elem` opts) $ putStr $ unlines $ map entryPath entries
    writeFile resultfile $ compress $ write entries
    when (Quiet `notElem` opts) $ putStrLn $ "Created dist as " ++ resultfile
  where
    safename n@(c:_) | isAlphaNum c  = n
    safename n = "./" ++ n


getDistName :: FilePath -> [String] -> FilePath
getDistName _ (dn:_) = dn
getDistName currentDirectory _ = takeFileName currentDirectory

