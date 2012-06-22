{-# LANGUAGE CPP, NamedFieldPuns #-}

-- Copyright (C) 2009-2010 Benedikt Schmidt
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

module Darcs.Repository.FileMod (
  PatchIndex(..),
  FileIdSpan(..),
  FileInfo(..),
  loadPatchIndex,
  loadSafePatchIndex,
  doesPatchIndexExist,
  isPatchIndexInSync,
  createPatchIndexDisk,
  updatePatchIndexDisk,
  createOrUpdatePatchIndexDisk,
  dumpPatchIndex,
  dumpPatchIndexFiles,
  lookupFid,
  lookupFids,
  removePidSuffix,
  filterPatches
) where

import Prelude hiding ( catch, pi )
import Data.Binary ( encodeFile, decodeFile )
import Data.Int ( Int32 )
import Data.List ( group, mapAccumL, sort, isPrefixOf )
import Data.Maybe ( fromMaybe, isNothing )
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception ( catch )
import Control.Monad.State ( evalState, execState, State, gets, modify )
import Control.Monad ( liftM2 )
import Control.Applicative ( (<$>) )
import System.Directory ( createDirectory, renameDirectory, doesFileExist )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository.Read ( readRepo )
import Darcs.Witnesses.Ordered ( mapFL, filterFL, RL(..) )
import Darcs.Witnesses.Sealed ( Sealed2(..), seal2 )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd(..), info )
import Darcs.Repository.Lock ( withPermDir, rmRecursive )
import Darcs.Patch ( RepoPatch, listTouchedFiles )
import Darcs.Path ( FileName, fp2fn, fn2fp )
import Darcs.Patch.Apply ( applyToFileMods, ApplyState(..) )
import Darcs.Patch.Set ( newset2FL, PatchSet(..), Tagged(..) )
import Darcs.Patch.Patchy ( Commute )
import Darcs.Patch.Info ( makePatchname )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Path ( FilePathLike, toFilePath )
import Darcs.Global ( darcsdir )
import Progress ( debugMessage )
import Darcs.Repository.FileModTypes
import System.FilePath( (</>) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypt.SHA256 (sha256sum )
import Storage.Hashed.Tree ( Tree(..) )

#include "impossible.h"

{- -----------------------------------------------------------------------------
   The patch index stores additional information that is extracted from
   the PatchSet for the repository to speed up certain commands.

  createPatchIndexDisk:
     Create the on-disk patch-index index from scratch.
   updatePatchIndexDisk:
     Update the on-disk patch-index index.
  ----------------------------------------------------------------------------- -}

-- ---------------------------------------------------------------------
-- Data structures for the patch-index

data FileIdSpan = FidSpan
                    !FileId           -- ^ the fileid has some fixed name in the
                    !PatchId          -- ^ span starting here
                    !(Maybe PatchId)  -- ^ and (maybe) ending here
  deriving (Show,Eq,Ord)

data FilePathSpan = FpSpan
                      !FileName         -- ^ the file path has some fixed fileid in the
                      !PatchId          -- ^ span starting here
                      !(Maybe PatchId)  -- ^ and (maybe) ending here
  deriving (Show,Eq,Ord)

-- | info about a given fileid, e.g.. is a file or a directory
data FileInfo = FileInfo { isFile::Bool,
                           touching::Set PatchId}
  deriving (Show,Eq,Ord)

-- | timespans where a certain filename corresponds to a file with a given id
type FileIdSpans = Map FileName [FileIdSpan]

-- | timespans where a file with a certain id corresponds to given filenames
type FilePathSpans = Map FileId [FilePathSpan]

-- | information file with a given ID
type InfoMap = Map FileId FileInfo

-- | the patch-index
data PatchIndex = PatchIndex {fidspans::FileIdSpans,
                              fpspans::FilePathSpans,
                              infom::InfoMap}

-- | an empty patch-index
emptyPatchIndex :: PatchIndex
emptyPatchIndex = PatchIndex M.empty M.empty M.empty

-- ---------------------------------------------------------------------
-- Query the patch-index

getInventoryHash :: FilePath -> IO String
getInventoryHash repodir = do
  inv <- B.readFile (repodir </> darcsdir </> "hashed_inventory")
  return $ sha256sum inv

-- ---------------------------------------------------------------------
-- create patch-index

-- | 'applyPatchMods pmods pindex' applies a list of PatchMods to the given
--   patch index pindex
applyPatchMods :: [(PatchId, PatchMod FileName)] -> PatchIndex -> PatchIndex
applyPatchMods pmods pindex =
  flip execState pindex $ mapM_ go (nubSeq pmods)
  -- nubSeq handles invalid patch in darcs repo:
  --   move with identical target name "rename darcs_patcher to darcs-patcher."
 where go :: (PatchId, PatchMod FileName) -> PIM ()
       go (pid, PCreateFile fn) = do
         fid <- createFidStartSpan fn pid
         startFpSpan fid fn pid
         createInfo fid True
         insertTouch fid pid
       go (pid, PCreateDir fn) = do
         fid <- createFidStartSpan fn pid
         startFpSpan fid fn pid
         createInfo fid False
         insertTouch fid pid
       go (pid, PTouch fn) = do
         fid <- lookupFid fn ("touch "++show fn)
         insertTouch fid pid
       go (pid, PRename oldfn newfn) = do
         fid <- lookupFid oldfn ("rename "++show oldfn++" "++show newfn)
         stopFpSpan fid pid
         startFpSpan fid newfn pid
         insertTouch fid pid
         stopFidSpan oldfn pid
         startFidSpan newfn pid fid
       go (pid, PRemove fn) = do
         fid <- lookupFid fn ("remove"++show fn)
         insertTouch fid pid
         stopFidSpan fn pid
         stopFpSpan fid pid
       go (_, PInvalid _) = return () -- just ignore invalid changes
       go (pid, PDuplicateTouch fn) = do
         fidm <- gets fidspans
         case M.lookup fn fidm of
           Just (FidSpan fid _ _:_) -> insertTouch fid pid
           Nothing -> return ()
           Just [] -> error $ "applyPatchMods: impossible, no entry for "++show fn
                              ++" in FileIdSpans in duplicate, empty list"

-- ---------------------------------------------------------------------
-- Update and query patch index

type PIM a = State PatchIndex a

-- | create new filespan for created file
createFidStartSpan :: FileName -> PatchId -> PIM FileId
createFidStartSpan fn pstart = do
  fidspans <- gets fidspans
  case M.lookup fn fidspans of
    Nothing -> do
      let fid = FileId fn 1
      modify (\pind -> pind {fidspans=M.insert fn [(FidSpan fid pstart Nothing)] fidspans})
      return fid
    Just fspans -> do
      let fid = FileId fn ((length fspans)+1)
      modify (\pind -> pind {fidspans=M.insert fn ((FidSpan fid pstart Nothing):fspans) fidspans})
      return fid

-- | start new span for name fn for file fid starting with patch pid
startFpSpan :: FileId -> FileName -> PatchId -> PIM ()
startFpSpan fid fn pstart = modify (\pind -> pind {fpspans=M.alter alt fid (fpspans pind)})
  where alt Nothing = Just [FpSpan fn pstart Nothing]
        alt (Just spans) = Just ((FpSpan fn pstart Nothing):spans)

-- | stop current span for file name fn
stopFpSpan :: FileId -> PatchId -> PIM ()
stopFpSpan fid pend = modify (\pind -> pind {fpspans=M.alter alt fid (fpspans pind)})
  where alt Nothing = error $ "impossible: no span for " ++ show fid
        alt (Just []) = error $ "impossible: no span for " ++ show fid++", empty list"
        alt (Just ((FpSpan fp pstart Nothing):spans)) =
          Just (FpSpan fp pstart (Just pend):spans)
        alt _ = error $ "impossible: span already ended for " ++ show fid

-- | start new span for name fn for file fid starting with patch pid
startFidSpan :: FileName -> PatchId -> FileId -> PIM ()
startFidSpan fn pstart fid = modify (\pind -> pind {fidspans=M.alter alt fn (fidspans pind)})
  where alt Nothing = Just [FidSpan fid pstart Nothing]
        alt (Just spans) = Just ((FidSpan fid pstart Nothing):spans)

-- | stop current span for file name fn
stopFidSpan :: FileName -> PatchId -> PIM ()
stopFidSpan fn pend = modify (\pind -> pind {fidspans=M.alter alt fn (fidspans pind)})
  where alt Nothing = error $ "impossible: no span for " ++ show fn
        alt (Just []) = error $ "impossible: no span for " ++ show fn++", empty list"
        alt (Just ((FidSpan fid pstart Nothing):spans)) =
          Just (FidSpan fid pstart (Just pend):spans)
        alt _ = error $ "impossible: span already ended for " ++ show fn

-- | insert touching patchid for given file id
createInfo :: FileId -> Bool -> PIM ()
createInfo fid isF = modify (\pind -> pind {infom=M.alter alt fid (infom pind)})
  where alt Nothing = Just (FileInfo isF S.empty)
        alt (Just _) = impossible "Fileid already exists"

-- | insert touching patchid for given file id
insertTouch :: FileId -> PatchId -> PIM ()
insertTouch fid pid = modify (\pind -> pind {infom=M.alter alt fid (infom pind)})
  where alt Nothing =  impossible "Fileid does not exist"
        alt (Just (FileInfo isF pids)) = Just (FileInfo isF (S.insert pid pids))

-- | lookup current fid of filepath
lookupFid :: FileName -> String -> PIM FileId
lookupFid fn hint = do
  fidm <- gets fidspans
  case M.lookup fn fidm of
    Just (FidSpan fid _ Nothing:_) -> return fid
    Nothing ->
      error $ "lookupFid: impossible, no entry for "++show fn++" in FileIdSpans in "++hint
    Just [] ->
      error $ "lookupFid: impossible, no entry for "++show fn++" in FileIdSpans in "
              ++hint++", empty list "

-- | lookup current fid(s) of filepath
--   matching subpatchs if the filepatch
--   is a directory
lookupFids :: FileName -> PIM [FileId]
lookupFids fn = do
   fid <- lookupFid fn ""
   info_map <- gets infom
   fid_spans <- gets fidspans
   case M.lookup fid info_map of
      Just (FileInfo True _) -> return [fid]
      Just (FileInfo False _) -> mapM (\fn -> lookupFid fn "") $ map fp2fn $ filter (isPrefixOf (fn2fp fn)) (fpSpans2filePaths' fid_spans)
      Nothing -> error $ "lookupFids: fid not in infomap"

-- | remove sequential duplicates
nubSeq :: Eq a => [a] -> [a]
nubSeq = map head . group

-- ---------------------------------------------------------------------
-- Create/Update patch-index on disk

-- | create patch index that corresponds to all patches in repo
createPatchIndexDisk :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
createPatchIndexDisk repository = do
  rawpatches <- newset2FL `fmap` readRepo repository
  let patches = mapFL Sealed2 rawpatches
  createPatchIndexFrom repository $ patches2patchMods patches S.empty

-- | convert patches to patchmods
patches2patchMods :: forall p. (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
                  => [Sealed2 (PatchInfoAnd p)] -> Set FileName -> [(PatchId, [PatchMod FileName])]
patches2patchMods patches fns = snd $ mapAccumL go fns patches
  where
    go filenames (Sealed2 p) = (filenames', (pid, pmods_effect ++ pmods_dup))
      where pid = make_patchID . info $ p
            (filenames', pmods_effect) = applyToFileMods p filenames
            -- applyToFileMods only returns patchmods that actually modify a file,
            -- i.e., never duplicate patches
            touched pm = case pm of {PTouch f -> [f]; PRename a b -> [a,b];
                                     PCreateDir f -> [f]; PCreateFile f -> [f];
                                     PRemove f -> [f]; _ -> []}
            touched_all = map fp2fn $ listTouchedFiles p
            touched_effect = concatMap touched pmods_effect
            touched_invalid = [ f | (PInvalid f) <- pmods_effect]
            -- listTouchedFiles returns all files that touched by these
            --  patches, even if they have no effect, e.g. by duplicate patches
            pmods_dup = map PDuplicateTouch . S.elems
                            $ S.difference (S.fromList touched_all)
                                           (S.union (S.fromList touched_invalid)
                                                    (S.fromList touched_effect))

-- | return set of current filenames in patch index
fpSpans2fileNames :: FilePathSpans -> Set FileName
fpSpans2fileNames fpSpans =
  S.fromList [fn | (FpSpan fn _ Nothing:_)<- M.elems fpSpans]

-- | remove all patch effects of given patches from patch index.
--   assumes that the given list of patches is a suffix of the
--   patches tracked by the patch-index
removePidSuffix :: Map PatchId Int -> [PatchId] -> PatchIndex -> PatchIndex
removePidSuffix _ [] pindex = pindex
removePidSuffix pid2idx (oldpid:_) (PatchIndex fidspans fpspans infom) =
    (PatchIndex (M.mapMaybe removefid fidspans)
                (M.mapMaybe removefp fpspans)
                (M.mapMaybe removetouch infom))
  where
    findIdx pid = fromMaybe (impossible "removePidSuffix") (M.lookup pid pid2idx)
    oldidx = findIdx oldpid
    from `before` idx = findIdx from < idx
    mto `beforeM` idx | Just to <- mto, findIdx to < idx = True
                      | otherwise = False
    removefid fidsps = if null fidsps' then Nothing else Just fidsps'
      where
        fidsps' = concatMap go fidsps
        go (FidSpan fid from mto)
          | from `before` oldidx && mto `beforeM` oldidx = [FidSpan fid from mto]
          | from `before` oldidx = [FidSpan fid from Nothing]
          | otherwise = []
    removefp fpsps = if null fpsps' then Nothing else Just fpsps'
      where
        fpsps' = concatMap go fpsps
        go (FpSpan fn from mto)
          | from `before` oldidx && mto `beforeM` oldidx = [FpSpan fn from mto]
          | from `before` oldidx = [FpSpan fn from Nothing]
          | otherwise = []
    removetouch (FileInfo isF pids) = if S.size pids' == 0 then Nothing else Just (FileInfo isF pids')
      where pids' = S.filter (\pid -> findIdx pid < oldidx) pids


-- | update the patch index to the current state of the repository
updatePatchIndexDisk :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
updatePatchIndexDisk repo@(Repo repodir  _ _) = do
    (pids,_,pid2idx,pindex) <- loadPatchIndex repodir
    -- check that patch index is up to date
    patches <- newset2FL `fmap` readRepo repo
    let pidsrepo = mapFL (make_patchID . info) $ patches
        (oldpids,newpids) = uncommon pids pidsrepo
        pindex' = removePidSuffix pid2idx oldpids pindex
        filenames = fpSpans2fileNames (fpspans pindex')
        cdir = repodir </> index_dir
    -- reread to prevent holding onto patches for too long
    rawpatches <- newset2FL `fmap` readRepo repo
    let newpatches = filterFL (\p -> make_patchID (info p) `elem` newpids) $ rawpatches
        newpmods = patches2patchMods newpatches filenames
        newmods = [ (pid,pm) | (pid, pms) <- newpmods, pm <- pms]
    inv_hash <- getInventoryHash repodir
    storePatchIndex repodir cdir pidsrepo inv_hash (applyPatchMods newmods pindex')
  where
    -- return uncommon suffixes after dropping common prefix of as and bs
    uncommon (a:as) (b:bs)
      | a == b     = uncommon as bs
      | otherwise  = (a:as,b:bs)
    uncommon as bs = (as,bs)

-- | 'createPatchIndexFrom repo pmods' creates a patch index from the given
--   patchmods.
createPatchIndexFrom :: forall p wR wU wT.
                     RepoPatch p
                     => Repository p wR wU wT -> [(PatchId, [PatchMod FileName])] -> IO ()
createPatchIndexFrom (Repo repodir _ _) pmods = do
    inv_hash <- getInventoryHash repodir
    storePatchIndex repodir cdir pids inv_hash (applyPatchMods mods emptyPatchIndex)
  where cdir = repodir </> index_dir
        pids = map fst pmods
        mods = [ (pid,pm) | (pid, pms) <- pmods, pm <- pms]


-- ---------------------------------------------------------------------
-- Load/Store patch-Index

-- | load patch-index from disk
loadPatchIndex :: FilePath -> IO ([PatchId], String, Map PatchId Int, PatchIndex)
loadPatchIndex repodir = do
  let pindex_dir = repodir </> index_dir
  (inv_hash, pids) <- loadRepoState (pindex_dir </> repo_state_file)
  let idx2pid  = M.fromList $ zip [(1::Int32)..] pids
  let pid2idx  = M.fromList $ zip pids [(1::Int)..]
  infom <- loadInfoMap (pindex_dir </> touch_map_file) idx2pid
  fidspans <- loadFidMap (pindex_dir </> fid_map_file)  idx2pid
  fpspans <- loadFpMap (pindex_dir </> fp_map_file) idx2pid
  return (pids, inv_hash, pid2idx, PatchIndex fidspans fpspans infom)

-- | load patch-index,
-- | ensuring that whenever loaded, the patch-index
-- | is always up to date.
loadSafePatchIndex :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree) => 
                      Repository p wR wU wT -> IO ([PatchId], String, Map PatchId Int, PatchIndex)
loadSafePatchIndex repo@(Repo repodir _ _) = do
   can_use <- liftM2 (&&) (doesPatchIndexExist repodir) (isPatchIndexInSync repo)
   if can_use
     then loadPatchIndex repodir
     else do
       updatePatchIndexDisk repo
       loadPatchIndex repodir

-- | check if patch-index exits for this repository
doesPatchIndexExist :: FilePath -> IO Bool
doesPatchIndexExist repodir =  fmap and $ sequence $ map (doesFileExist . (pindex_dir </>)) [repo_state_file, touch_map_file, fid_map_file, fp_map_file]
   where pindex_dir = repodir </> index_dir 

-- | filter given patches so as to keep only the patches that modify the given files
filterPatches :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd p) => Repository p wR wU wT -> [FilePath] -> [Sealed2 a] -> IO [Sealed2 a]
filterPatches repository fps ops = do
   (pids, inv_hash, pid2idx, pi@(PatchIndex fidspans fpspans infom)) <- loadSafePatchIndex repository
   let fids = concat $ map ((\fn -> evalState (lookupFids fn) pi). fp2fn) fps
       ans_pidss = map ((\(FileInfo _ a) -> a).fromJust.(\fid -> M.lookup fid infom)) fids
       npids = foldl S.union S.empty ans_pidss
   return $ filter (flip S.member npids . (\(Sealed2 (PIAP pin _)) -> PID $ BC.pack $ makePatchname pin)) ops
-- | create or update patch index
createOrUpdatePatchIndexDisk :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree) =>
                                Repository p wR wU wT -> IO ()
createOrUpdatePatchIndexDisk repo@(Repo repodir _ _)= do
   dpie <- doesPatchIndexExist repodir
   if dpie
      then updatePatchIndexDisk repo
      else createPatchIndexDisk repo

-- | check if patch-index is in sync with repository
isPatchIndexInSync :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO Bool
isPatchIndexInSync repo@(Repo repodir _ _) = do
   (_, inv_hash_pindex, _, _) <- loadPatchIndex repodir
   inv_hash <- getInventoryHash repodir
   return (inv_hash == inv_hash_pindex)

-- | store patch-index on disk
storePatchIndex :: FilePath -> FilePath -> [PatchId] -> String -> PatchIndex -> IO ()
storePatchIndex repodir cdir pids inv_hash (PatchIndex fidspans fpspans infom) = do
  createDirectory cdir `catch` \(_ :: IOError) -> return ()
  let pid2idx = M.fromList $ zip pids [(1::Int)..]
  tmpdir <- withPermDir (repodir </> "filecache-tmp") $ \dir -> do
              debugMessage "About to create patch index..."
              let tmpdir = toFilePath dir
              storeRepoState (tmpdir </> repo_state_file) pids inv_hash
              storeInfoMap (tmpdir </> touch_map_file) pid2idx infom
              storeFidMap (tmpdir </> fid_map_file) pid2idx fidspans
              storeFpMap (tmpdir </> fp_map_file) pid2idx fpspans
              debugMessage "Patch index created"
              return tmpdir
  rmRecursive cdir `catch` \(_ :: IOError) -> return ()
  renameDirectory tmpdir cdir

storeRepoState :: FilePath -> [PatchId] -> String -> IO ()
storeRepoState fp pids inv_hash = encodeFile fp (inv_hash, pids)

loadRepoState :: FilePath -> IO (String,[PatchId])
loadRepoState fp = decodeFile fp

storeFidMap :: FilePath -> Map PatchId Int -> FileIdSpans -> IO ()
storeFidMap fp pid2idx fidm =
  encodeFile fp $ M.map (map (\(FidSpan a b c) -> (a, lookupIdx pid2idx b, toIdxM c))) fidm
 where toIdxM (Nothing) = 0
       toIdxM (Just pid) = lookupIdx pid2idx pid

loadFidMap :: FilePath -> Map Int32 PatchId -> IO FileIdSpans
loadFidMap fp idx2pid = M.map (map (\(a,b,c) -> FidSpan a (toPid b) (toPidM c))) <$> decodeFile fp
  where toPid idx = fromMaybe impossible $ M.lookup idx idx2pid
        toPidM 0 = Nothing
        toPidM idx = case M.lookup idx idx2pid of
                       Nothing -> impossible
                       mpid -> mpid

storeFpMap :: FilePath -> Map PatchId Int -> FilePathSpans -> IO ()
storeFpMap fp pid2idx fidm =
  encodeFile fp $ M.map (map (\(FpSpan a b c) -> (a, lookupIdx pid2idx b, toIdxM c))) fidm
 where toIdxM (Nothing) = 0
       toIdxM (Just pid) = lookupIdx pid2idx pid

loadFpMap :: FilePath -> Map Int32 PatchId -> IO FilePathSpans
loadFpMap fp idx2pid = M.map (map (\(a,b,c) -> FpSpan a (toPid b) (toPidM c))) <$> decodeFile fp
  where toPid idx = fromMaybe impossible $ M.lookup idx idx2pid
        toPidM 0 = Nothing
        toPidM idx = case M.lookup idx idx2pid of
                       Nothing -> impossible
                       mpid -> mpid

storeInfoMap :: FilePath -> Map PatchId Int -> InfoMap -> IO ()
storeInfoMap fp pid2idx infom =
  encodeFile fp $ M.map (\fi -> (isFile fi,S.map (lookupIdx pid2idx) (touching fi))) infom

loadInfoMap :: FilePath -> Map Int32 PatchId -> IO InfoMap
loadInfoMap fp idx2pid = M.map (\(isF,idxs) -> FileInfo isF (S.map toPid idxs)) <$> decodeFile fp
   where toPid idx = fromMaybe impossible $ M.lookup idx idx2pid

lookupIdx :: Map PatchId Int -> PatchId -> Int32
lookupIdx pid2idx pid = maybe impossible (fromIntegral :: Int -> Int32) (M.lookup pid pid2idx)

-- | Base directory for the patch index
index_dir :: String
index_dir = darcsdir </> "patch_index"

repo_state_file :: String
repo_state_file = "repo_state"

fid_map_file :: String
fid_map_file = "fid_map"

fp_map_file :: String
fp_map_file = "fp_map"

touch_map_file :: String
touch_map_file = "touch_map"

-- ---------------------------------------------------------------------
-- Dump information in patch index

dumpRepoState :: [PatchId] -> String
dumpRepoState = unlines . map pid2string

dumpFileIdSpans :: FileIdSpans -> String
dumpFileIdSpans fidspans =
  unlines [fn2fp fn++" -> "++showFileId fid++" from "++pid2string from++" to "++maybe "-" pid2string mto
           | (fn, fids) <- M.toList fidspans, FidSpan fid from mto <- fids]

dumpFilePathSpans :: FilePathSpans -> String
dumpFilePathSpans fpspans =
  unlines [showFileId fid++" -> "++ fn2fp fn++" from "++pid2string from++" to "++maybe "-" pid2string mto
           | (fid, fns) <- M.toList fpspans, FpSpan fn from mto <- fns]

dumpTouchingMap :: InfoMap -> String
dumpTouchingMap infom = unlines [showFileId fid++(if isF then "" else "/")++" -> "++pid2string pid
                                | (fid,FileInfo isF pids) <- M.toList infom, pid <- S.elems pids]

-- | return set of current filepaths in patch index
fpSpans2filePaths :: FilePathSpans -> InfoMap -> [FilePath]
fpSpans2filePaths fpSpans infom =
  sort [fn2fp fn ++ (if isF then "" else "/") | (fid,FpSpan fn _ Nothing:_) <- M.toList fpSpans,
                                                let Just (FileInfo isF _) = M.lookup fid infom]

-- | return set of current filepaths in patch index, for internal use
fpSpans2filePaths' :: FileIdSpans -> [FilePath]
fpSpans2filePaths' fidSpans = [fn2fp fp | (fp, FidSpan _ _ ep:_)  <- M.toList fidSpans, isNothing ep]

dumpPatchIndex :: FilePath -> IO ()
dumpPatchIndex repodir = do
  (pids,inv_hash,_,PatchIndex fidspans fpspans infom) <- loadPatchIndex repodir
  putStrLn $ "Inventory hash:" ++ inv_hash
  putStrLn "================="
  putStrLn "Repo state:"
  putStrLn "==========="
  putStrLn $ dumpRepoState pids
  putStrLn "Fileid spans:"
  putStrLn "============="
  putStrLn $ dumpFileIdSpans fidspans
  putStrLn "Filepath spans:"
  putStrLn "=============="
  putStrLn $ dumpFilePathSpans fpspans
  putStrLn "Info Map:"
  putStrLn "========="
  putStrLn $ dumpTouchingMap infom
  putStrLn "Files:"
  putStrLn "=============="
  putStrLn $ unlines $ fpSpans2filePaths fpspans infom


dumpPatchIndexFiles :: FilePath -> IO ()
dumpPatchIndexFiles repodir = do
  (_,_,_,PatchIndex _ fpspans infom) <- loadPatchIndex repodir
  putStr $ unlines $ fpSpans2filePaths fpspans infom
