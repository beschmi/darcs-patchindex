-- Copyright (C) 2009 Benedikt Schmidt
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,

-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.


{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP,GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Darcs.FileModMonad ( FileModMonad, withPatchMods ) where

import Darcs.Repository.FileModTypes ( PatchMod(..) )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Control.Monad.State
import Control.Arrow
import Darcs.Path ( FileName, fn2fp, movedirfilename )
import qualified Data.Set as S
import Data.Set ( Set )
import Data.List ( isPrefixOf )
import Storage.Hashed.Tree (Tree)
#include "impossible.h"

newtype FileModMonad a = FMM (State ((Set FileName), [PatchMod FileName]) a)
  deriving (Functor, Monad, MonadState ((Set FileName), [PatchMod FileName]))

withPatchMods :: FileModMonad a -> (Set FileName) -> (Set FileName, [PatchMod FileName])
withPatchMods (FMM m) fps = (id *** reverse) $ execState m (fps,[])

instance MonadPlus FileModMonad where
    mzero = bug "mzero FileModMonad"
    a `mplus` _ = a

-- These instances are defined to be used only with
-- apply.
instance ApplyMonad FileModMonad Tree where
    type ApplyMonadBase FileModMonad = IO
    mDoesDirectoryExist d = do
      fps <- gets fst
      return $ S.member d fps
    mDoesFileExist f = do
      fps <- gets fst
      return $ S.member f fps
    mReadFilePS _ = bug "mReadFilePS FileModMonad"
    nestedApply _ _ = bug "nestedApply FileModMonad"
    liftApply _ _ = bug "liftApply FileModMonad"
    getApplyState = bug "getApplyState FileModMonad"
    putApplyState _ = bug "putApplyState FileModMonad"
    mCreateFile f = createFile f
    mCreateDirectory d = createDir d
    mRemoveFile f = remove f
    mRemoveDirectory f = remove f
    mRename a b = do
      fns <- gets fst
      if (S.notMember a fns) then do
         addMod (PInvalid a)  -- works around some old repo inconsistencies
       else
        do -- we have to account for directory moves
           addMod (PRename a b)
           modifyFps (S.delete a)
           addFile b
           forM_ (S.toList fns) $ \fn ->
             when (fn2fp a `isPrefixOf` fn2fp fn) $ do
               modifyFps (S.delete fn)
               let newfn = (movedirfilename a b) fn
               addFile newfn
               addMod (PRename fn newfn)

    mModifyFilePS f _ = addMod (PTouch f)
    mModifyFilePSs f _ = addMod (PTouch f)

-- ---------------------------------------------------------------------
-- State Handling Functions

addMod :: PatchMod FileName -> FileModMonad ()
addMod pm = modify (id *** (pm:))

addFile :: FileName -> FileModMonad ()
addFile f = modify ((S.insert f) *** id)

createFile :: FileName -> FileModMonad ()
createFile fn = do
  fs <- gets fst
  when (S.member fn fs) $ error "file created twice"
  addMod (PCreateFile fn)
  addFile fn

createDir :: FileName -> FileModMonad ()
createDir fn = do
  fs <- gets fst
  when (S.member fn fs) $ error "directory created twice"
  addMod (PCreateDir fn)
  addFile fn

remove :: FileName -> FileModMonad ()
remove f = addMod (PRemove f) >> modifyFps (S.delete f)

modifyFps :: (Set FileName -> Set FileName) -> FileModMonad ()
modifyFps f = modify (f *** id)
