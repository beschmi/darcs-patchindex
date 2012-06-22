--  Copyright (C) 2003 David Roundy, 2010-2011 Petr Rockai
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
{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -cpp #-}

module Darcs.UI.Commands.Annotate ( annotate ) where

import Control.Monad ( when )

import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInHashedRepository )
import Darcs.UI.Arguments
    ( DarcsFlag(..)
    , workingRepoDir
    , summary
    , unified
    , machineReadable
    , xmloutput
    , creatorhash
    , fixSubPaths
    , matchOne
    )
import Darcs.UI.Flags ( isUnified, toMatchFlags, useCache, compression )
import Storage.Hashed.Plain( readPlainTree )
import Darcs.Repository.State ( readRecorded )
import Darcs.Repository
    ( Repository
    , withRepository
    , RepoJob(..)
    , readRepo
    , listRegisteredFiles
    )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch ( RepoPatch, Named, patch2patchinfo, xmlSummary, invertRL )
import Darcs.Patch.Apply( ApplyState )
import qualified Darcs.Patch ( summary )
import qualified Data.ByteString.Char8 as BC ( pack, concat, intercalate )
import Data.ByteString.Lazy ( toChunks )
import Darcs.UI.PrintPatch ( printPatch, contextualPrintPatch )
import Darcs.Patch.ApplyMonad( withFileNames )
import System.FilePath( (</>) )
import Darcs.Patch.Info ( humanFriendly, toXml, showPatchInfo )
import Darcs.Patch.Match ( matchPatch, haveNonrangeMatch, getNonrangeMatchS  )
import Darcs.Repository.Match ( getFirstMatch, getOnePatchset )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Witnesses.Sealed ( Sealed2(..), Sealed(..), seal )
import qualified Darcs.Patch.Annotate as A
import Printer ( putDocLn, Doc )

import Storage.Hashed.Tree( Tree, TreeItem(..), readBlob, list, expand )
import Storage.Hashed.Monad( findM, virtualTreeIO )
import Darcs.Path( floatPath, anchorPath, fp2fn, toFilePath )
#include "impossible.h"

annotateDescription :: String
annotateDescription = "Display which patch last modified something."

annotateHelp :: String
annotateHelp =
 "The `darcs annotate' command provides two unrelated operations.  When\n" ++
 "called on a file, it will find the patch that last modified each line\n" ++
 "in that file.  When called on a patch (e.g. using --patch), it will\n" ++
 "print the internal representation of that patch.\n" ++
 "\n" ++
 "The --summary option will result in a summarized patch annotation,\n" ++
 "similar to `darcs whatsnew'.  It has no effect on file annotations.\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The --xml-output\n" ++
 "option can be used to generate output for machine postprocessing.\n"

annotate :: DarcsCommand
annotate = DarcsCommand {commandProgramName = "darcs",
                         commandName = "annotate",
                         commandHelp = annotateHelp,
                         commandDescription = annotateDescription,
                         commandExtraArgs = -1,
                         commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                         commandCommand = annotateCmd,
                         commandPrereq = amInHashedRepository,
                         commandGetArgPossibilities = listRegisteredFiles,
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [],
                         commandBasicOptions = [summary,unified,
                                                 machineReadable,
                                                 xmloutput,
                                                 matchOne, creatorhash,
                                                 workingRepoDir]}

annotateCmd :: [DarcsFlag] -> [String] -> IO ()
annotateCmd opts files = withRepository (useCache opts) (RepoJob (annotate' opts files))

annotate' :: (RepoPatch p, ApplyState p ~ Tree)
          => [DarcsFlag] -> [String] -> Repository p wR wU wR -> IO ()

annotate' opts [] repository = do
  let matchFlags = toMatchFlags opts
  when (not $ haveNonrangeMatch matchFlags) $
      fail $ "Annotate requires either a patch pattern or a " ++
               "file or directory argument."
  Sealed2 p <- matchPatch matchFlags `fmap` readRepo repository
  if Summary `elem` opts
     then do putDocLn $ showpi $ patch2patchinfo p
             putDocLn $ show_summary p
     else if isUnified opts
          then withTempDir "context" $ \_ ->
               do getFirstMatch repository (compression opts) matchFlags
                  c <- readPlainTree "."
                  contextualPrintPatch c p
          else printPatch p
    where showpi | MachineReadable `elem` opts = showPatchInfo
                 | XMLOutput `elem` opts       = toXml
                 | otherwise                   = humanFriendly
          show_summary :: RepoPatch p => Named p wX wY -> Doc
          show_summary = if XMLOutput `elem` opts
                         then xmlSummary
                         else Darcs.Patch.summary

annotate' opts [""] repository = annotate' opts [] repository
annotate' opts args@[_] repository = do
  let matchFlags = toMatchFlags opts
  r <- readRepo repository
  (origpath:_) <- fixSubPaths opts args
  recorded <- readRecorded repository

  (Sealed patches, initial, path) <-
    if haveNonrangeMatch matchFlags
       then do Sealed x <- getOnePatchset repository matchFlags
               let fn = [fp2fn $ toFilePath origpath]
                   nonRangeMatch = getNonrangeMatchS matchFlags r
                   (_, [path], _) = withFileNames Nothing fn nonRangeMatch
               initial <- snd `fmap` virtualTreeIO (getNonrangeMatchS matchFlags r) recorded
               return $ (seal $ newset2RL x, initial, toFilePath path)
       else return $ (seal $ newset2RL r, recorded, toFilePath origpath)

  found <- findM initial (floatPath $ toFilePath path)
  -- TODO need to decide about the --machine flag
  let fmt = if MachineReadable `elem` opts then A.machineFormat else A.format
  case found of
    Nothing -> fail $ "No such file or directory: " ++ toFilePath path
    Just (SubTree s) -> do
      s' <- expand s
      let subs = map (fp2fn . (path </>) . anchorPath "" . fst) $ list s'
          showPath (n, File _) = BC.pack (path </> n)
          showPath (n, _) = BC.concat [BC.pack (path </> n), "/"]
      putStrLn $ fmt (BC.intercalate "\n" $ map showPath $
                       map (\(x,y) -> (anchorPath "" x, y)) $ list s') $
        A.annotateDirectory (invertRL patches) (fp2fn $ "./" ++ path) subs
    Just (File b) -> do con <- BC.concat `fmap` toChunks `fmap` readBlob b
                        putStrLn $ fmt con $ A.annotate (invertRL patches) (fp2fn $ "./" ++ path) con
    Just (Stub _ _) -> impossible

annotate' _ _ _ = fail "annotate accepts at most one argument"
