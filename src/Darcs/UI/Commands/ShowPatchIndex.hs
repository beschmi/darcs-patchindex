module Darcs.UI.Commands.ShowPatchIndex ( showPatchIndexFiles, showPatchIndexAnnotate, showPatchIndexAll, showPatchIndexStatus ) where
import Darcs.UI.Arguments ( DarcsFlag(..), workingRepoDir,
                         getRepourl, files, directories, nullFlag, fixSubPaths, machineReadable, xmloutput, matchOne
                         , creatorhash, unified, summary )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInHashedRepository )
import Darcs.Repository.State ( readRecorded )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository ( withRepository, withRepositoryDirectory, RepoJob(..), listRegisteredFiles)
import Darcs.Repository.FileMod
import Data.List ( find )
import Data.Maybe ( fromMaybe, fromJust )
import Data.Set (Set)
import qualified Data.Foldable ( foldl )
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( forM_, mapM )
import Darcs.Repository ( readRepo )
import Darcs.Witnesses.Ordered ( RL(..), mapFL )
import Darcs.Witnesses.Sealed ( Sealed2(..), Sealed(..), seal, seal2, unseal2 )
import Darcs.Patch.Set ( newset2RL, newset2FL )
import Darcs.Path ( toFilePath, fp2fn, FileName )
import qualified Darcs.Patch.Annotate as A
import Storage.Hashed.Monad( findM, virtualTreeIO )
import Storage.Hashed.AnchoredPath( floatPath, anchorPath )
import Data.ByteString.Lazy ( toChunks )
import Storage.Hashed.Tree( Tree, TreeItem(..), readBlob, list, expand )
import Darcs.Patch ( RepoPatch, Named, patch2patchinfo, xmlSummary, invertRL )
import Darcs.Patch.ApplyMonad( withFileNames )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd(..) )
import Darcs.Patch ( RepoPatch )
import qualified Darcs.Patch ( summary )
import Darcs.Patch.Apply ( ApplyState(..) )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..) )
import Darcs.Patch.Show ( description )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.PatchInfoAnd ( hopefullyM )
import Darcs.Patch.Info ( makePatchname )
import Darcs.Patch.Match ( matchPatch, haveNonrangeMatch, getNonrangeMatchS  )
import Darcs.ColorPrinter (fancyPrinters)
import Darcs.Repository.FileModTypes
import Darcs.UI.Flags ( toMatchFlags, useCache )
import Darcs.Repository.Match ( getFirstMatch, getOnePatchset )
import qualified Data.ByteString.Char8 as BC
import Storage.Hashed.Tree ( Tree(..) )
import Printer (putDocLnWith)
import Darcs.Patch.Invert (invert)
import Control.Monad.State ( evalState )
import System.FilePath( (</>) )

showPatchIndexAll :: DarcsCommand
showPatchIndexAll = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-all",
  commandDescription = "Dump complete content of patch index.",
  commandHelp =
      "The `darcs show patch-index all' command lists all information in the patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexAllCmd,
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

showPatchIndexFiles :: DarcsCommand
showPatchIndexFiles = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-files",
  commandDescription = "Dump current files registered in patch index.",
  commandHelp =
      "The `darcs show patch-index files' command lists all current files registered in the patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexFilesCmd,
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

showPatchIndexAnnotate :: DarcsCommand
showPatchIndexAnnotate = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-annotate",
  commandHelp =
      "The `darcs show patch-index-annotate FILE' command annotate's FILE's using patch index",
  commandDescription = "run patch index annotate.",
  commandExtraArgs = -1,
  commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
  commandCommand = showPatchIndexAnnotate',
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = listRegisteredFiles,
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [summary,unified,
                           machineReadable,
                           xmloutput,
                           matchOne, creatorhash,
                           workingRepoDir] }

showPatchIndexStatus :: DarcsCommand
showPatchIndexStatus = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-status",
  commandDescription = " Report patch-index status",
  commandHelp =
      "The `darcs show patch-index-status' reports if the patch index is in sync, out of sync, or does not exist",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexStatus',
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

showPatchIndexAllCmd :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexAllCmd opts _ = do
  withRepository (useCache opts) $ RepoJob $ \(Repo repodir _ _) -> do
    dumpPatchIndex repodir

showPatchIndexAnnotate' :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexAnnotate' opts args = do
  let matchFlags = toMatchFlags opts
  withRepository (useCache opts) $ RepoJob $ \repository -> do
  r <- readRepo repository
  (origpath:_) <- fixSubPaths opts args
  recorded <- readRecorded repository

  (patch_id, initial, path) <-
    if haveNonrangeMatch matchFlags
       then do Sealed x <- getOnePatchset repository matchFlags
               let fn = [fp2fn $ toFilePath origpath]
                   nonRangeMatch = getNonrangeMatchS matchFlags r
                   (_, [path], _) = withFileNames Nothing fn nonRangeMatch
               initial <- snd `fmap` virtualTreeIO (getNonrangeMatchS matchFlags r) recorded
               return $ (Just $ PID $ BC.pack $ (\((PIAP patchinfo _):<:_) -> makePatchname patchinfo) $ newset2RL x, initial, toFilePath path)
       else return $ (Nothing, recorded, "./" ++ toFilePath origpath)

  let fmt = if MachineReadable `elem` opts then A.machineFormat else A.format
  found <- findM initial (floatPath $ toFilePath path)
  case found of
    Just (SubTree s) -> do
      s' <- expand s
      let subs = map ((path </>) . anchorPath "" . fst) $ list s'
          showPath (n, File _) = BC.pack (path </> n)
          showPath (n, _) = BC.concat [BC.pack (path </> n), BC.pack "/"]
      ans_patches <- (map (\(Sealed2 p) -> seal2 $ invert p)) `fmap` getPatchesMultipleFiles repository subs patch_id
      putStrLn $ fmt (BC.intercalate (BC.pack "\n") $ map showPath $
                       map (\(x,y) -> (anchorPath "" x, y)) $ list s') $
        A.annotateDirectoryPI (ans_patches) (fp2fn path) (map fp2fn subs)
      return ()
    Just (File b) -> do
      ans_patches <- (map (\(Sealed2 p) -> seal2 $ invert p)) `fmap` getPatches repository path patch_id
      con <- BC.concat `fmap` toChunks `fmap` readBlob b
      putStrLn $ fmt con $ A.annotatePI ans_patches (fp2fn path) con

showPatchIndexFilesCmd :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexFilesCmd opts _ = withRepository (useCache opts) $ RepoJob $ \(Repo repodir _ _) -> do
  dumpPatchIndexFiles repodir

showPatchIndexStatus' :: [DarcsFlag] -> [String] -> IO ()
showPatchIndexStatus' opts _ = withRepository (useCache opts) $ RepoJob $ \(repo@(Repo repodir _ _)) -> do
  ex <- doesPatchIndexExist repodir
  if ex 
   then do
          sy <- isPatchIndexInSync repo 
          if sy
            then putStrLn "Patch Index is in sync with repo."
            else putStrLn "Patch Index is outdated. Run darcs optimize --patch-index"
   else putStrLn "Patch Index is not yet created. Run darcs optimize --patch-index"

getPatchPositions :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> FileName -> Maybe PatchId -> IO [Int]
getPatchPositions repo fn pid = do
  (pids, inv_hash, pid2idx, pi@(PatchIndex fidspans fpspans infom)) <- loadSafePatchIndex repo
  let pos = (\x -> (pid2idx M.! x) - 1) `fmap` pid
      suffixPatches = (\x -> drop x pids) `fmap` pos
      use_pi = fromMaybe pi $ (\sps -> removePidSuffix pid2idx sps pi) `fmap` suffixPatches
      fid = evalState (lookupFid fn "") use_pi
      Just (FileInfo _ ans_pids) = M.lookup fid infom
      ans =  Data.Foldable.foldl (\ls px -> filter (<=px) ls) (S.toAscList $ S.map (\x -> ((pid2idx M.! x) - 1)) ans_pids) pos
  return ans

getPatches :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd p) => Repository p wR wU wT -> FilePath -> Maybe PatchId -> IO [Sealed2 a]
getPatches repository fp pid = do
  ids <- getPatchPositions repository (fp2fn fp) pid
  pxes <- (mapFL seal2 . newset2FL) `fmap` readRepo repository
  return $ getElems ids pxes

getElems :: [Int] -> [a] -> [a]
getElems idxs elems = (\(ans,_,_) -> ans) $ foldl f ([], 0, elems) idxs
 where f :: ([a], Int, [a]) -> Int -> ([a], Int, [a])
       f (ans, ed, el) np = (new:ans, np+1, left)
          where new:left = drop (np-ed) el

getPatchesMultipleFiles :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd p) => Repository p wR wU wT -> [FilePath] -> Maybe PatchId -> IO [Sealed2 a]
getPatchesMultipleFiles repository fps pid = do
   (pids, inv_hash, pid2idx, pi@(PatchIndex fidspans fpspans infom)) <- loadSafePatchIndex repository
   let fids = map ((\fn -> evalState (lookupFid fn "") pi). fp2fn) fps
       ans_pidss = map ((\(FileInfo _ a) -> a).fromJust.(\fid -> M.lookup fid infom)) fids
       idss = map (\ans_pids -> S.toAscList $ S.map (\x -> ((pid2idx M.! x) - 1)) ans_pids) ans_pidss
       pos = (\x -> (pid2idx M.! x) - 1) `fmap` pid
       ids =  Data.Foldable.foldl (\ls px -> filter (<=px) ls) (foldl merge [] idss) pos
   pxes <- (mapFL seal2 . newset2FL) `fmap` readRepo repository
   return $ getElems ids pxes
  where merge :: Ord a => [a] -> [a] -> [a]
        merge [] bs = bs
        merge as [] = as
        merge (a:as) (b:bs)  | a < b = a:merge as (b:bs)
                             | a == b = a:merge as bs
                             | otherwise = b:merge (a:as) bs


