-- Copyright (C) 2002-2005,2007 David Roundy
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

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Darcs.Repository.ApplyPatches
    ( applyPatches
    , runTolerantly
    , runSilently
    ) where

import Prelude hiding ( catch )
import Control.Exception.Extensible ( catch, SomeException, IOException )
import Data.Char ( toLower )
import Data.List ( isSuffixOf )
import System.IO.Error ( isDoesNotExistError, isPermissionError )
import Control.Monad.Error
import System.Directory ( createDirectory,
                          removeDirectory, removeFile,
                          renameFile, renameDirectory,
                          doesDirectoryExist, doesFileExist
                        )

import Darcs.Patch.ApplyMonad( ApplyMonad(..) )
import Darcs.Patch.ApplyPatches ( applyPatches )
import Darcs.Repository.Prefs( changePrefval )
import Darcs.Repository.Lock ( writeAtomicFilePS )

import Darcs.Utils ( prettyException )
import Darcs.Repository.External ( backupByCopying, backupByRenaming )
import Darcs.Path ( FileName, fn2fp )
import qualified Data.ByteString as B (empty, null, readFile)

import Storage.Hashed.Tree( Tree )

instance ApplyMonad IO Tree where
    type ApplyMonadBase IO = IO
    mDoesDirectoryExist = doesDirectoryExist . fn2fp
    mChangePref = changePrefval
    mModifyFilePS f j = B.readFile (fn2fp f) >>= j >>= writeAtomicFilePS (fn2fp f)
    mCreateDirectory = createDirectory . fn2fp
    mCreateFile f = do exf <- doesFileExist (fn2fp f)
                       if exf then fail $ "File '"++fn2fp f++"' already exists!"
                              else do exd <- doesDirectoryExist $ fn2fp f
                                      if exd then fail $ "File '"++fn2fp f++"' already exists!"
                                             else writeAtomicFilePS (fn2fp f) B.empty
    mRemoveFile f = do let fp = fn2fp f
                       x <- B.readFile fp
                       when (not $ B.null x) $
                            fail $ "Cannot remove non-empty file "++fp
                       removeFile fp
    mRemoveDirectory = removeDirectory . fn2fp
    mRename a b = catch
                  (renameDirectory x y `mplus` renameFile x y)
                  -- We need to catch does not exist errors, since older
                  -- versions of darcs allowed users to rename nonexistent
                  -- files.  :(
                  (\e -> if isDoesNotExistError e
                                 then return ()
                                 else ioError e)
      where x = fn2fp a
            y = fn2fp b

class Monad m => TolerantMonad m where
    warning :: IO () -> m ()
    runIO :: m a -> IO a
    runTM :: IO a -> m a

newtype TolerantIO a = TIO { runTolerantly :: IO a }
instance TolerantMonad TolerantIO where
    warning io = TIO $ io `catch` \e -> putStrLn $ "Warning: " ++ prettyException e
    runIO (TIO io) = io
    runTM io = TIO io

newtype SilentIO a = SIO { runSilently :: IO a }
instance TolerantMonad SilentIO where
    warning io = SIO $ io `catch` \(_ :: SomeException) -> return ()
    runIO (SIO io) = io
    runTM io = SIO io

-- NOTE: The following instance declarations are duplicated merely to avoid
-- enabling -fallow-undecidable-instances.  If we used
-- -fallow-undecidable-instances, we would write instead:

-- instance TolerantMonad m => Monad m where
--      ...

-- etc.
instance Functor TolerantIO where
    fmap f m = m >>= return . f

instance Monad TolerantIO where
    f >>= g = runTM $ runIO f >>= runIO . g
    f >> g = runTM $ runIO f >> runIO g
    fail s = runTM $ fail s
    return x = runTM $ return x

instance Functor SilentIO where
    fmap f m = m >>= return . f

instance Monad SilentIO where
    f >>= g = runTM $ runIO f >>= runIO . g
    f >> g = runTM $ runIO f >> runIO g
    fail s = runTM $ fail s
    return x = runTM $ return x

instance ApplyMonad TolerantIO Tree where
    type ApplyMonadBase TolerantIO = IO
    mDoesDirectoryExist d = runTM $ mDoesDirectoryExist d
    mReadFilePS f = runTM $ mReadFilePS f
    mChangePref a b c = warning $ mChangePref a b c
    mModifyFilePS f j = warning $ mModifyFilePS f (runIO . j)
    mCreateFile f = warning $ backup f >> mCreateFile f
    mCreateDirectory d = warning $ backup d >> mCreateDirectory d
    mRemoveFile f = warning $ mRemoveFile f
    mRemoveDirectory d = warning $ catch
                                 (mRemoveDirectory d)
                                 (\(e :: IOException) ->
                                   if "(Directory not empty)" `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because:\n" ++ show e)
    mRename a b = warning $ catch
                          (let do_backup = if (map toLower x == map toLower y)
                                           then backupByCopying (fn2fp b) -- avoid making the original vanish
                                           else backupByRenaming (fn2fp b)
                           in do_backup >> mRename a b)
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | otherwise -> ioError e
                          )
       where
        x = fn2fp a
        y = fn2fp b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y

instance ApplyMonad SilentIO Tree where
    type ApplyMonadBase SilentIO = IO
    mDoesDirectoryExist d = runTM $ mDoesDirectoryExist d
    mReadFilePS f = runTM $ mReadFilePS f
    mChangePref a b c = warning $ mChangePref a b c
    mModifyFilePS f j = warning $ mModifyFilePS f (runIO . j)
    mCreateFile f = warning $ backup f >> mCreateFile f
    mCreateDirectory d = warning $ backup d >> mCreateDirectory d
    mRemoveFile f = warning $ mRemoveFile f
    mRemoveDirectory d = warning $ catch
                                 (mRemoveDirectory d)
                                 (\(e :: SomeException) ->
                                   if "(Directory not empty)" `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because:\n" ++ show e)
    mRename a b = warning $ catch
                          (let do_backup = if (map toLower x == map toLower y)
                                           then backupByCopying (fn2fp b) -- avoid making the original vanish
                                           else backupByRenaming (fn2fp b)
                           in do_backup >> mRename a b)
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | otherwise -> ioError e
                          )
       where
        x = fn2fp a
        y = fn2fp b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y

backup :: FileName -> IO ()
backup f = backupByRenaming (fn2fp f)
