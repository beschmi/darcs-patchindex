--  Copyright (C) 2004-2005 David Roundy
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

{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module Darcs.Repository.Match
    (
      getNonrangeMatch
    , getPartialNonrangeMatch
    , getFirstMatch
    , getOnePatchset
    ) where

import Darcs.Patch.Match
    ( getNonrangeMatchS
    , getFirstMatchS
    , nonrangeMatcherIsTag
    , getMatchingTag
    , matchAPatchset
    , nonrangeMatcher
    , MatchFlag(..)
    )

import Darcs.Repository.Flags ( Compression )
import Darcs.Patch.Bundle ( scanContext )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set ( PatchSet(..), SealedPatchSet, Origin )
import Darcs.Patch.Witnesses.Sealed ( seal )

import Darcs.Repository.Internal
    ( Repository, readRepo, createPristineDirectoryTree )

import Storage.Hashed.Tree ( Tree )

import Darcs.Path ( FileName, toFilePath )

import ByteStringUtils ( mmapFilePS )
#include "impossible.h"

getNonrangeMatch :: (ApplyMonad IO (ApplyState p), RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wT
                 -> Compression
                 -> [MatchFlag]
                 -> IO ()
getNonrangeMatch r compr fs = withRecordedMatch r compr (getNonrangeMatchS fs)

getPartialNonrangeMatch :: (RepoPatch p, ApplyMonad IO (ApplyState p), ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> Compression
                        -> [MatchFlag]
                        -> [FileName]
                        -> IO ()
getPartialNonrangeMatch r compr fs _ =
    withRecordedMatch r compr (getNonrangeMatchS fs)

getFirstMatch :: (ApplyMonad IO (ApplyState p), RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT
              -> Compression
              -> [MatchFlag]
              -> IO ()
getFirstMatch r compr fs = withRecordedMatch r compr (getFirstMatchS fs)

getOnePatchset :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository p wR wU wT
               -> [MatchFlag]
               -> IO (SealedPatchSet p Origin)
getOnePatchset repository fs =
    case nonrangeMatcher fs of
        Just m -> do ps <- readRepo repository
                     if nonrangeMatcherIsTag fs
                        then return $ getMatchingTag m ps
                        else return $ matchAPatchset m ps
        Nothing -> (seal . scanContext) `fmap` mmapFilePS (toFilePath $ context_f fs)
    where context_f [] = bug "Couldn't match_nonrange_patchset"
          context_f (Context f:_) = f
          context_f (_:xs) = context_f xs

withRecordedMatch :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository p wR wU wT
                  -> Compression
                  -> (PatchSet p Origin wR -> IO ())
                  -> IO ()
withRecordedMatch r compr job
    = do createPristineDirectoryTree r compr "."
         readRepo r >>= job
