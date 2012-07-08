-- Copyright (C) 2002-2004 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using Prelude.catch
{-# LANGUAGE CPP, ScopedTypeVariables #-}


module Darcs.Repository
    ( Repository
    , HashedDir(..)
    , Cache(..)
    , CacheLoc(..)
    , WritableOrNot(..)
    , RepoJob(..)
    , maybeIdentifyRepository
    , identifyRepositoryFor
    , withRecorded
    , withRepoLock
    , withRepoReadLock
    , withRepository
    , withRepositoryDirectory
    , withGutsOf
    , makePatchLazy
    , writePatchSet
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , replacePristine
    , readRepo
    , prefsUrl
    , readRepoUsingSpecificInventory
    , addToPending
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyAddToPending
    , tentativelyReplacePatches
    , readTentativeRepo
    , tentativelyMergePatches
    , considerMergeToWorking
    , revertRepositoryChanges
    , finalizeRepositoryChanges
    , createRepository
    , copyRepository
    , patchSetToRepository
    , unrevertUrl
    , applyToWorking
    , patchSetToPatches
    , createPristineDirectoryTree
    , createPartialsPristineDirectoryTree
    , reorderInventory
    , cleanRepository
    , PatchSet
    , SealedPatchSet
    , PatchInfoAnd
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , checkUnrelatedRepos
    , testTentative
    , modifyCache
    , reportBadSources
    -- * Recorded and unrecorded and pending.
    , readRecorded
    , readUnrecorded
    , unrecordedChanges
    , readPending
    , readRecordedAndPending
    -- * Index.
    , readIndex
    , invalidateIndex
    -- * Used as command arguments
    , listFiles
    , listRegisteredFiles
    , listUnregisteredFiles
    ) where

import System.Exit ( ExitCode(..), exitWith )
import Data.List ( (\\), isPrefixOf )
import Data.Maybe( catMaybes, isJust, listToMaybe )

import Darcs.Repository.State
    ( readRecorded
                             , readUnrecorded
                             , readWorking
                             , unrecordedChanges
                             , readPending
                             , readIndex
                             , invalidateIndex
                             , readRecordedAndPending
    , restrictBoring
    , applyTreeFilter
                             )

import Darcs.Repository.Internal
    (Repository(..)
    , RepoType(..)
    , RepoJob(..)
    , maybeIdentifyRepository
    , identifyRepositoryFor
    , identifyDarcsRepository
    , IdentifyRepo(..)
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , makePatchLazy
    , readRepo
    , readTentativeRepo
    , readRepoUsingSpecificInventory
    , prefsUrl
    , withRecorded
    , withRepoLock
    , withRepoReadLock
    , withRepository
    , withRepositoryDirectory
    , withGutsOf
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyAddToPending
    , tentativelyReplacePatches
    , revertRepositoryChanges
    , finalizeRepositoryChanges
    , unrevertUrl
    , applyToWorking
    , patchSetToPatches
    , createPristineDirectoryTree
    , createPartialsPristineDirectoryTree
    , reorderInventory
    , cleanRepository
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , makeNewPending
    , seekRepo
    )
import Darcs.Repository.Test
    ( testTentative )

import Darcs.Repository.Merge( tentativelyMergePatches
                             , considerMergeToWorking
                             )
import Darcs.Repository.Cache ( unionRemoteCaches
                              , fetchFileUsingCache
                              , speculateFileUsingCache
                              , HashedDir(..)
                              , Cache(..)
                              , CacheLoc(..)
                              , WritableOrNot(..)
                              , hashedDir
                              , CacheType(Directory)
                              , reportBadSources
                              )
import Darcs.Patch.Set ( Origin
                       , PatchSet(..)
                       , SealedPatchSet
                       , newset2RL
                       , newset2FL
                       , progressPatchSet
                       )
import URL ( maxPipelineLength )

import Control.Exception ( Exception, throw, finally )
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( MVar
                               , newMVar
                               , putMVar
                               , takeMVar
                               )
import Control.Monad ( unless, when )
import Control.Applicative( (<$>) )
import System.Directory ( createDirectory
                        , createDirectoryIfMissing
                        , renameFile
                        , doesFileExist
                        , removeFile
                        , getDirectoryContents
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.IO ( stderr )
import System.IO.Error ( isAlreadyExistsError )
import System.Posix.Files ( createLink )

import qualified Darcs.Repository.HashedRepo as HashedRepo

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, extractHash )
import Darcs.Repository.ApplyPatches ( applyPatches )
import Darcs.Repository.HashedRepo ( applyToTentativePristine
                                   , pris2inv
                                   , revertTentativeChanges
                                   , copySources
                                   )
import Darcs.Repository.InternalTypes ( modifyCache )
import Darcs.Patch ( RepoPatch, PrimOf )
import Darcs.Patch.Apply( ApplyState )

import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , bunchFL
    , mapFL
    , mapRL
    , lengthRL
    , (+>+)
    , (:\/:)(..)
    )
import Darcs.Repository.Format ( RepoProperty ( HashedInventory )
                               , RepoFormat
                               , createRepoFormat
                               , formatHas
                               , writeRepoFormat
                               , readfromAndWritetoProblem
                               )
import Darcs.Repository.Prefs ( writeDefaultPrefs )
import Darcs.Patch.Depends ( areUnrelatedRepos, findUncommon )

import Darcs.Utils ( withCurrentDirectory
                   , catchall
                   , promptYorn
                   )
import Darcs.Repository.External
    ( copyFileOrUrl
    , Cachable(..)
    , fetchFileLazyPS
    )
import Progress ( debugMessage
                , tediousSize
                , beginTedious
                , endTedious
                )
import Darcs.Patch.Progress
    ( progressRLShowTags
    , progressFL
    )
import Darcs.Repository.Lock
    ( writeBinFile
    , writeDocBinFile
    , withTemp
    )
import Darcs.Repository.Flags
    ( UpdateWorking(..)
    , UseCache(..)
    , UseIndex(..)
    , ScanKnown(..)
    , RemoteDarcs (..)
    , Compression (..)
    , GetKind (..)
    , Verbosity (..)
    , DryRun (..)
    , UMask (..)
    , AllowConflicts (..)
    , ExternalMerge (..)
    , WantGuiPause (..)
    )

import Darcs.Global ( darcsdir )
import Darcs.URL ( isFile )
import Darcs.SignalHandler ( catchInterrupt )
import Printer ( Doc, text, hPutDocLn, putDocLn )

import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.Tree( Tree, emptyTree, expand, list )
import Storage.Hashed.Hash( encodeBase16 )
import Darcs.Path( anchorPath )
import Storage.Hashed.Darcs( writeDarcsHashed, darcsAddMissingHashes )
import ByteStringUtils( gzReadFilePS )

import System.FilePath( (</>)
                      , takeFileName
                      , splitPath
                      , joinPath
                      , takeDirectory
                      )
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip ( compress, decompress )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Darcs.Repository.FileMod (createOrUpdatePatchIndexDisk)
#include "impossible.h"


-- @createRepository useFormat1 useNoWorkingDir patchIndex@
createRepository :: Bool -> Bool -> Bool -> IO ()
createRepository useFormat1 useNoWorkingDir createPatchIndex = do
  createDirectory darcsdir `catch`
      (\e-> if isAlreadyExistsError e
            then fail "Tree has already been initialized!"
            else fail $ "Error creating directory `"++darcsdir++"'.")
  cwd <- getCurrentDirectory
  x <- seekRepo
  when (isJust x) $ do
      setCurrentDirectory cwd
      putStrLn "WARNING: creating a nested repository."
  createDirectory $ darcsdir ++ "/pristine.hashed"
  createDirectory $ darcsdir ++ "/patches"
  createDirectory $ darcsdir ++ "/prefs"
  writeDefaultPrefs
  let repoFormat = createRepoFormat useFormat1 useNoWorkingDir
  writeRepoFormat repoFormat (darcsdir++"/format")
  writeBinFile (darcsdir++"/hashed_inventory") ""
  writePristine "." emptyTree
  when createPatchIndex $ withRepository NoUseCache $ RepoJob $ \repo -> createOrUpdatePatchIndexDisk repo

data RepoSort = Hashed | Old

repoSort :: RepoFormat -> RepoSort
repoSort f
  | formatHas HashedInventory f = Hashed
  | otherwise = Old

copyInventory :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT
              -> UseCache -> RemoteDarcs -> Compression
              -> IO ()
copyInventory fromRepo@(Repo fromDir fromFormat (DarcsRepository _ fromCache)) useCache remoteDarcs compression = do
  toRepo@(Repo toDir toFormat (DarcsRepository toPristine toCache)) <-
    identifyDarcsRepository useCache "."
  let (_ :: Repository p wR wU wT) = toRepo --The witnesses are wrong, but cannot escape
  case readfromAndWritetoProblem fromFormat toFormat of
    Just e ->  fail $ "Incompatibility with repository " ++ fromDir ++ ":\n" ++ e
    Nothing -> return ()
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo2 :: Repository p wR wU wT
      toRepo2 = Repo toDir toFormat $ DarcsRepository toPristine toCache2
      copyHashedHashed = HashedRepo.copyRepo toRepo2 remoteDarcs fromDir
  case repoSort fromFormat of
    Hashed -> copyHashedHashed
    Old -> withCurrentDirectory toDir $ do
                HashedRepo.revertTentativeChanges
                patches <- readRepo fromRepo
                let k = "Copying patch"
                beginTedious k
                tediousSize k (lengthRL $ newset2RL patches)
                let patches' = progressPatchSet k patches
                HashedRepo.writeTentativeInventory toCache compression patches'
                endTedious k
                HashedRepo.finalizeTentativeChanges toRepo compression


-- *copyRepository fromRepo ... withWorkingDir usePacks@
copyRepository :: forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
               => Repository p wR wU wT
               -> Verbosity -> UseCache
               -> GetKind -> Compression
               -> DryRun
               -> UMask -> RemoteDarcs
               -> Bool -> Bool -> Bool -> IO ()
copyRepository fromRepo@(Repo fromDir _ _) v uc gk compr dry um rdarcs withWorkingDir usePacks patchIndex = do
  debugMessage "Copying prefs"
  copyFileOrUrl rdarcs (fromDir ++ "/" ++ darcsdir ++ "/prefs/prefs")
    (darcsdir ++ "/prefs/prefs") (MaxAge 600) `catchall` return ()
  -- try packs for remote repositories
  if (not . isFile) fromDir && usePacks
    then copyPackedRepository    fromRepo v uc gk compr dry um rdarcs withWorkingDir patchIndex
    else copyNotPackedRepository fromRepo v uc gk compr dry um rdarcs withWorkingDir patchIndex

putInfo :: Verbosity -> Doc -> IO ()
putInfo Quiet _ = return ()
putInfo _ d = hPutDocLn stderr d

copyNotPackedRepository :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> Verbosity -> UseCache
                        -> GetKind -> Compression
                        -> DryRun
                        -> UMask -> RemoteDarcs
                        -> Bool
                        -> Bool
                        -> IO ()
copyNotPackedRepository fromrepository@(Repo _ rffrom _) verb useCache getKind compression dryRun umask rdarcs withWorkingDir patchIndex = do
  copyInventory fromrepository useCache rdarcs compression
  debugMessage "Grabbing lock in new repository..."
  withRepoLock dryRun useCache YesUpdateWorking umask
   $ RepoJob $ \torepository ->
    if formatHas HashedInventory rffrom
    then do
      when withWorkingDir $ do
        debugMessage "Writing working directory contents..."
        createPristineDirectoryTree torepository compression "."
      fetchPatchesIfNecessary getKind verb torepository `catchInterrupt`
        (putInfo verb $ text "Using lazy repository.")
    else do
      local_patches <- readRepo torepository
      replacePristine torepository emptyTree
      let patchesToApply = progressFL "Applying patch" $ newset2FL local_patches
      sequence_ $ mapFL applyToTentativePristine $ bunchFL 100 patchesToApply
      finalizeRepositoryChanges torepository dryRun YesUpdateWorking compression patchIndex
      when withWorkingDir $ do
        debugMessage "Writing working directory contents..."
        createPristineDirectoryTree torepository compression "."

copyPackedRepository ::
  forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
  => Repository p wR wU wT
  -> Verbosity -> UseCache
  -> GetKind -> Compression
  -> DryRun
  -> UMask -> RemoteDarcs
  -> Bool -> Bool -> IO ()
copyPackedRepository r verb useCache getKind compr dryRun umask rdarcs withWorkingDir patchIndex =
  -- fallback to no-packs get in case of error
  copyPackedRepository2 r verb useCache getKind compr dryRun withWorkingDir patchIndex
      `catchall` copyNotPackedRepository r verb useCache getKind compr dryRun umask rdarcs withWorkingDir patchIndex

copyPackedRepository2 ::
  forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
  => Repository p wR wU wT
  -> Verbosity -> UseCache
  -> GetKind -> Compression
  -> DryRun
  -> Bool -> Bool -> IO ()
copyPackedRepository2 fromRepo@(Repo fromDir _ (DarcsRepository _ fromCache)) verb useCache getKind compression dryRun withWorkingDir patchIndex = do
  b <- fetchFileLazyPS (fromDir ++ "/" ++ darcsdir ++ "/packs/basic.tar.gz") Uncachable
  when (verb == Verbose) $ putDocLn $ text "Getting packed repository."
  Repo toDir toFormat (DarcsRepository toPristine toCache) <-
    identifyRepositoryFor fromRepo useCache "."
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo :: Repository p wR wU wR -- In empty repo, t(entative) = r(ecorded)
      toRepo = Repo toDir toFormat $ DarcsRepository toPristine toCache2
      fromPacksDir = fromDir ++ "/" ++ darcsdir ++ "/packs/"
  createDirectoryIfMissing False $ darcsdir </> "inventories"
  copySources toRepo fromDir
  Repo _ _ (DarcsRepository _ toCache3) <-
    identifyRepositoryFor toRepo useCache "."
  -- unpack inventory & pristine cache
  let isLazy = getKind == LazyGet
  cleanDir "pristine.hashed"
  removeFile $ darcsdir </> "hashed_inventory"
  unpackBasic toCache3 . Tar.read $ decompress b
  when withWorkingDir $
    createPristineDirectoryTree toRepo compression "."
  -- pull new patches
  us <- readRepo toRepo
  them <- readRepo fromRepo
  us' :\/: them' <- return $ findUncommon us them
  revertTentativeChanges
  Sealed pw <- tentativelyMergePatches toRepo "get" NoAllowConflicts YesUpdateWorking NoExternalMerge useCache NoWantGuiPause compression verb ( UseIndex, ScanKnown ) us' them'
  invalidateIndex toRepo
  withGutsOf toRepo $ do
    finalizeRepositoryChanges toRepo dryRun YesUpdateWorking compression patchIndex
    when withWorkingDir $
      applyToWorking toRepo verb pw >> return ()
    return ()
  -- get old patches
  unless isLazy $ (do
    cleanDir "patches"
    putInfo verb $ text "Copying patches, to get lazy repository hit ctrl-C..."
    unpackPatches toCache3 (mapFL hashedPatchFileName $ newset2FL us) .
      Tar.read . decompress =<< fetchFileLazyPS (fromPacksDir ++
      "patches.tar.gz") Uncachable
    ) `catchInterrupt` (putInfo verb $ text "Using lazy repository.")
 where
  cleanDir d = mapM_ (\x -> removeFile $ darcsdir </> d </> x) .
    filter (\x -> head x /= '.') =<< getDirectoryContents (darcsdir </> d)

withControlMVar :: (MVar () -> IO ()) -> IO ()
withControlMVar f = do
  mv <- newMVar ()
  f mv
  takeMVar mv

forkWithControlMVar :: MVar () -> IO () -> IO ()
forkWithControlMVar mv f = do
  takeMVar mv
  _ <- forkIO $ flip finally (putMVar mv ()) f
  return ()

removeMetaFiles :: IO ()
removeMetaFiles = mapM_ (removeFile . (darcsdir </>)) .
  filter ("meta-" `isPrefixOf`) =<< getDirectoryContents darcsdir

unpackBasic :: Exception e => Cache -> Tar.Entries e -> IO ()
unpackBasic c x = do
  withControlMVar $ \mv -> unpackTar c (basicMetaHandler c mv) x
  removeMetaFiles

unpackPatches :: Exception e => Cache -> [String] -> Tar.Entries e -> IO ()
unpackPatches c ps x = do
  withControlMVar $ \mv -> unpackTar c (patchesMetaHandler c ps mv) x
  removeMetaFiles

unpackTar :: Exception e => Cache -> IO () -> Tar.Entries e -> IO ()
unpackTar  _ _ Tar.Done = return ()
unpackTar  _ _ (Tar.Fail e)= throw e
unpackTar c mh (Tar.Next x xs) = case Tar.entryContent x of
  Tar.NormalFile x' _ -> do
    let p = Tar.entryPath x
    if "meta-" `isPrefixOf` takeFileName p
      then do
        BL.writeFile p x'
        mh
        unpackTar c mh xs
      else do
        ex <- doesFileExist p
        if ex
          then debugMessage $ "Tar thread: STOP " ++ p
          else do
            if p == darcsdir </> "hashed_inventory"
              then writeFile' Nothing p x'
              else writeFile' (cacheDir c) p $ compress x'
            debugMessage $ "Tar thread: GET " ++ p
            unpackTar c mh xs
  _ -> fail "Unexpected non-file tar entry"
 where
  writeFile' Nothing z y = withTemp $ \x' -> do
    BL.writeFile x' y
    renameFile x' z
  writeFile' (Just ca) z y = do
    let x' = joinPath . tail $ splitPath z -- drop darcsdir
    ex <- doesFileExist $ ca </> x'
    if ex
      then createLink' (ca </> x') z
      else withTemp $ \x'' -> do
        BL.writeFile x'' y
        createLink' x'' $ ca </> x'
        renameFile x'' z
  createLink' z y = do
    createDirectoryIfMissing True $ takeDirectory y
    createLink z y `catchall` return ()

basicMetaHandler :: Cache -> MVar () -> IO ()
basicMetaHandler ca mv = do
  ex <- doesFileExist $ darcsdir </> "meta-filelist-pristine"
  when ex . forkWithControlMVar mv $
    fetchFilesUsingCache ca HashedPristineDir . lines =<<
      readFile (darcsdir </> "meta-filelist-pristine")

patchesMetaHandler :: Cache -> [String] -> MVar () -> IO ()
patchesMetaHandler ca ps mv = do
  ex <- doesFileExist $ darcsdir </> "meta-filelist-inventories"
  when ex $ do
    forkWithControlMVar mv $ fetchFilesUsingCache ca HashedPristineDir .
      lines =<< readFile (darcsdir </> "meta-filelist-inventories")
    forkWithControlMVar mv $ fetchFilesUsingCache ca HashedPatchesDir ps

cacheDir :: Cache -> Maybe String
cacheDir (Ca cs) = listToMaybe . catMaybes .flip map cs $ \x -> case x of
  Cache Directory Writable x' -> Just x'
  _ -> Nothing

hashedPatchFileName :: PatchInfoAnd p wA wB -> String
hashedPatchFileName x = case extractHash x of
  Left _ -> fail "unexpected unhashed patch"
  Right h -> h
 
-- | fetchFilesUsingCache is similar to mapM fetchFileUsingCache, exepts
-- it stops execution if file it's going to fetch already exists.
fetchFilesUsingCache :: Cache -> HashedDir -> [FilePath] -> IO ()
fetchFilesUsingCache _ _ [] = return ()
fetchFilesUsingCache c d (f:fs) = do
  ex <- doesFileExist $ darcsdir </> hashedDir d </> f
  if ex
    then debugMessage $ "Cache thread: STOP " ++
      (darcsdir </> hashedDir d </> f)
    else do
      debugMessage $ "Cache thread: GET " ++
        (darcsdir </> hashedDir d </> f)
      _ <- fetchFileUsingCache c d f
      fetchFilesUsingCache c d fs

-- | writePatchSet is like patchSetToRepository, except that it doesn't
-- touch the working directory or pristine cache.
writePatchSet :: (RepoPatch p, ApplyState p ~ Tree)
              => PatchSet p Origin wX
              -> UseCache -> Compression
              -> IO (Repository p wR wU wT)
writePatchSet patchset useCache compr = do
    maybeRepo <- maybeIdentifyRepository useCache "."
    let repo@(Repo _ _ (DarcsRepository _ c)) =
          case maybeRepo of
            GoodRepository r -> r
            BadRepository e -> bug ("Current directory is a bad repository in writePatchSet: " ++ e)
            NonRepository e -> bug ("Current directory not a repository in writePatchSet: " ++ e)
    debugMessage "Writing inventory"
    HashedRepo.writeTentativeInventory c compr patchset
    HashedRepo.finalizeTentativeChanges repo compr
    return repo

-- | patchSetToRepository takes a patch set, and writes a new repository
--   in the current directory that contains all the patches in the patch
--   set.  This function is used when 'darcs get'ing a repository with
--   the --to-match flag.
patchSetToRepository :: (RepoPatch p, ApplyState p ~ Tree)
                     => Repository p wR1 wU1 wR1
                     -> PatchSet p Origin wX
                     -> UseCache -> Compression -> RemoteDarcs
                     -> IO ()
patchSetToRepository (Repo fromrepo rf _) patchset useCache compr remoteDarcs = do
    when (formatHas HashedInventory rf) $ -- set up sources and all that
       do writeFile "_darcs/tentative_pristine" "" -- this is hokey
          repox <- writePatchSet patchset useCache compr
          HashedRepo.copyRepo repox remoteDarcs fromrepo
    repo <- writePatchSet patchset useCache compr
    readRepo repo >>= (applyPatches . newset2FL)
    debugMessage "Writing the pristine"
    pristineFromWorking repo

checkUnrelatedRepos :: RepoPatch p
                    => Bool
                    -> PatchSet p wStart wX
                    -> PatchSet p wStart wY
                    -> IO ()
checkUnrelatedRepos allowUnrelatedRepos us them =
    when ( not allowUnrelatedRepos && areUnrelatedRepos us them ) $
         do confirmed <- promptYorn "Repositories seem to be unrelated. Proceed?"
            unless confirmed $ do putStrLn "Cancelled."
                                  exitWith ExitSuccess

-- | This function fetches all patches that the given repository has
--   with fetchFileUsingCache, unless --lazy is passed.
--   This is used as a helper in copyRepository.
fetchPatchesIfNecessary :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                        => GetKind -> Verbosity
                        -> Repository p wR wU wT
                        -> IO ()
fetchPatchesIfNecessary LazyGet _ _ = return ()
fetchPatchesIfNecessary getKind verb torepository@(Repo _ _ (DarcsRepository _ c)) =
  do  unless (getKind == CompleteGet) $
          putInfo verb $ text "Copying patches, to get lazy repository hit ctrl-C..."
      r <- readRepo torepository
      pipelineLength <- maxPipelineLength
      let patches = newset2RL r
          ppatches = progressRLShowTags "Copying patches" patches
          (first, other) = splitAt (pipelineLength - 1) $ tail $ hashes patches
          speculate | pipelineLength > 1 = [] : first : map (:[]) other
                    | otherwise = []
      mapM_ fetchAndSpeculate $ zip (hashes ppatches) (speculate ++ repeat [])
  where hashes :: forall wX wY . RL (PatchInfoAnd p) wX wY -> [String]
        hashes = catMaybes . mapRL ((either (const Nothing) Just) . extractHash)
        fetchAndSpeculate :: (String, [String]) -> IO ()
        fetchAndSpeculate (f, ss) = do
          _ <- fetchFileUsingCache c HashedPatchesDir f
          mapM_ (speculateFileUsingCache c HashedPatchesDir) ss

addToPending :: (RepoPatch p, ApplyState p ~ Tree)
             => Repository p wR wU wT -> UpdateWorking -> FL (PrimOf p) wU wY -> IO ()
addToPending (Repo _ _ _) NoUpdateWorking _ = return ()
addToPending repo@(Repo{}) uw p =
    do pend <- unrecordedChanges (UseIndex, ScanKnown) repo Nothing
       invalidateIndex repo
       makeNewPending repo uw (pend +>+ p)

-- | Replace the existing pristine with a new one (loaded up in a Tree object).
replacePristine :: Repository p wR wU wT -> Tree IO -> IO ()
replacePristine (Repo r _ _) = writePristine r

writePristine :: FilePath -> Tree IO -> IO ()
writePristine r tree = withCurrentDirectory r $
    do let t = darcsdir </> "hashed_inventory"
       i <- gzReadFilePS t
       tree' <- darcsAddMissingHashes tree
       root <- writeDarcsHashed tree' $ darcsdir </> "pristine.hashed"
       writeDocBinFile t $ pris2inv (BS.unpack $ encodeBase16 root) i

pristineFromWorking :: RepoPatch p => Repository p wR wU wT -> IO ()
pristineFromWorking repo@(Repo dir _ _) =
  withCurrentDirectory dir $ readWorking >>= replacePristine repo

-- | Get a list of all non-boring files and directories in the working copy.
listFiles :: IO [String]
listFiles =  do nonboring <- restrictBoring emptyTree
                working <- expand =<< applyTreeFilter nonboring <$> readPlainTree "."
                return $ map (anchorPath "" . fst) $ list working

-- | 'listUnregisteredFiles' returns the list of all non-boring unregistered
-- files in the repository.
listUnregisteredFiles :: IO [String]
listUnregisteredFiles =
    do unregd <- listFiles
       regd <- listRegisteredFiles
       return $ unregd \\ regd -- (inefficient)

-- | 'listRegisteredFiles' returns the list of all registered files in the repository.
listRegisteredFiles :: IO [String]
listRegisteredFiles =
    do recorded <- expand =<< withRepository YesUseCache (RepoJob readRecordedAndPending)
       return $ map (anchorPath "" . fst) $ list recorded
