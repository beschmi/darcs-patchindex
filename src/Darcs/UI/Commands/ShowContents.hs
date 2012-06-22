--  Copyright (C) 2007 Eric Kow
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

module Darcs.UI.Commands.ShowContents ( showContents ) where

import Control.Monad ( filterM, forM_, forM, unless )
import System.IO ( stdout )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, findRepository )
import Darcs.UI.Arguments ( DarcsFlag, matchOne,
                         workingRepoDir, fixSubPaths )
import Darcs.UI.Flags ( toMatchFlags, useCache )
import Darcs.Patch.ApplyMonad ( withFiles )
import Darcs.Patch.Match
    ( haveNonrangeMatch
    , applyInvToMatcher
    , nonrangeMatcher
    , InclusiveOrExclusive(..)
    , matchExists
    , applyNInv
    , hasIndexRange )
import Darcs.Repository ( withRepository, RepoJob(..), readRepo, readRecorded )
import qualified Storage.Hashed.Monad as HSM
import Darcs.Path( floatPath, anchorPath, FileName, fp2fn,
                   sp2fn, toFilePath )

showContentsDescription :: String
showContentsDescription = "Outputs a specific version of a file."

showContentsHelp :: String
showContentsHelp =
  "Show contents can be used to display an earlier version of some file(s).\n"++
  "If you give show contents no version arguments, it displays the recorded\n"++
  "version of the file(s).\n"

showContents :: DarcsCommand
showContents = DarcsCommand {commandProgramName = "darcs",
                              commandName = "contents",
                              commandHelp = showContentsHelp,
                              commandDescription = showContentsDescription,
                              commandExtraArgs = -1,
                              commandExtraArgHelp
                                    = ["[FILE]..."],
                              commandCommand = showContentsCmd,
                              commandPrereq = findRepository,
                              commandGetArgPossibilities = return [],
                              commandArgdefaults = nodefaults,
                              commandAdvancedOptions = [],
                              commandBasicOptions = [matchOne, workingRepoDir]}

showContentsCmd :: [DarcsFlag] -> [String] -> IO ()
showContentsCmd _ [] = fail "show contents needs at least one argument."
showContentsCmd opts args = withRepository (useCache opts) $ RepoJob $ \repository -> do
  path_list <- map sp2fn `fmap` fixSubPaths opts args
  pristine <- readRecorded repository
  unapply <- let matchFlags = toMatchFlags opts
             in
             if haveNonrangeMatch matchFlags
               then do
                  patchset <- readRepo repository
                  case nonrangeMatcher matchFlags of
                    -- Index cannot be a Matcher, so handle it manually.
                    Nothing -> case hasIndexRange matchFlags of
                      Just (n, m) | n == m -> return $ applyNInv (n-1) patchset
                      _ -> fail "Couldn't obtain a valid matcher."
                    Just m -> do
                      unless (matchExists m patchset) $
                        fail $ "Couldn't match pattern " ++ show m
                      return $ applyInvToMatcher Exclusive m patchset
                else return (return ())
  let dump :: HSM.TreeIO [(FileName, B.ByteString)]
      dump = do
        let floatedPaths = map (floatPath . toFilePath) path_list
        okpaths <- filterM HSM.fileExists floatedPaths
        forM okpaths $ \f -> do
          content <- (B.concat . BL.toChunks) `fmap` HSM.readFile f
          return (fp2fn $ ("./" ++) $ anchorPath "" f, content)
  files <- flip withFiles unapply `fmap` fst
    `fmap` HSM.virtualTreeIO dump pristine
  forM_ files $ \(_, f) -> B.hPut stdout f
