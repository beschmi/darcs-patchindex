--  Copyright (C) 2003-2005 David Roundy
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

module Darcs.UI.Commands.Optimize ( optimize ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( finally )
import Control.Monad ( when, unless )
import Data.Maybe ( isJust )
import Data.List ( sort )
import System.Directory
    ( getDirectoryContents
    , doesDirectoryExist
    , doesFileExist
    , renameFile
    , getModificationTime
    , createDirectoryIfMissing
    , removeFile
    )
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL


import Darcs.Patch.PatchInfoAnd ( extractHash )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInRepository )
import Darcs.UI.Arguments
    ( DarcsFlag( UpgradeFormat
               , Compress
               , UnCompress
               , NoCompress
               , Reorder
               , Relink
               , OptimizePristine
               , OptimizeHTTP
               )
     , reorderPatches
     , uncompressNocompress
     , relink
     , sibling
     , flagsToSiblings
     , upgradeFormat
     , workingRepoDir
     , umaskOption
     , optimizePristine
     , optimizeHTTP
     )
import Darcs.Repository.Prefs ( getPreflist, getCaches )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , readRepo
    , reorderInventory
    , cleanRepository
    , replacePristine
    )
import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Patch.Witnesses.Ordered
     ( mapRL
     , mapFL
     , bunchFL
     , lengthRL
     )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set
    ( newset2RL
    , newset2FL
    , progressPatchSet
    )
import Darcs.Patch.Apply( ApplyState )
import ByteStringUtils ( gzReadFilePS )
import Darcs.Repository.Lock
    ( maybeRelink
    , gzWriteAtomicFilePS
    , writeAtomicFilePS
    , rmRecursive
    )
import Darcs.Utils ( withCurrentDirectory )
import Darcs.UI.External ( catchall )
import Progress
    ( beginTedious
    , endTedious
    , tediousSize
    , debugMessage
    )
import Darcs.Global ( darcsdir )

import System.FilePath.Posix
    ( takeExtension
    , (</>)
    , (<.>)
    , takeFileName
    )
import Darcs.UI.Flags
    ( compression, verbosity, dryRun, useCache, umask )
import Darcs.Repository.Flags
    ( UpdateWorking (..), DryRun (..), UseCache (..), UMask (..) )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Repository.Cache ( hashedDir, HashedDir(HashedPristineDir) )
import Darcs.Repository.Format
    ( identifyRepoFormat
    , createRepoFormat
    , writeRepoFormat
    , formatHas
    , RepoProperty ( HashedInventory )
    )
import qualified Darcs.Repository.HashedRepo as HashedRepo
import Darcs.Repository.State ( readRecorded )

import Storage.Hashed.Tree
    ( Tree
    , TreeItem(..)
    , list
    , expand
    , emptyTree
    )
import Darcs.Path( anchorPath, toFilePath )
import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.Darcs
    ( writeDarcsHashed
    , decodeDarcsSize
    )
import Codec.Archive.Tar ( write )
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath )
import Codec.Compression.GZip ( compress )


optimizeDescription :: String
optimizeDescription = "Optimize the repository."

optimizeHelp :: String
optimizeHelp =
 "The `darcs optimize' command modifies the current repository in an\n" ++
 "attempt to reduce its resource requirements.  By default a single\n" ++
 "fast, safe optimization is performed; additional optimization\n" ++
 "techniques can be enabled by passing options to `darcs optimize'.\n" ++
 "\n" ++
 "--reorder moves recent patches (those not included in\n" ++
 "the latest tag) to the `front', reducing the amount that a typical\n" ++
 "remote command needs to download.  It should also reduce the CPU time\n" ++
 "needed for some operations.\n" ++
 "\n" ++ optimizeHelpRelink ++
 -- uncompression is least useful, so it is last.
 "\n" ++ optimizeHelpCompression ++
 "\n" ++
 "There is one more optimization which CAN NOT be performed by this\n" ++
 "command.  Every time your record a patch, a new inventory file is\n" ++
 "written to _darcs/inventories/, and old inventories are never reaped.\n" ++
 "\n" ++
 "If _darcs/inventories/ is consuming a relatively large amount of\n" ++
 "space, you can safely reclaim it by using `darcs get' to make a\n" ++
 "complete copy of the repo.  When doing so, don't forget to copy over\n" ++
 "any unsaved changes you have made to the working tree or to\n" ++
 "unversioned files in _darcs/prefs/ (such as _darcs/prefs/author).\n"

optimize :: DarcsCommand
optimize = DarcsCommand {
      commandProgramName = "darcs"
    , commandName = "optimize"
    , commandHelp = optimizeHelp
    , commandDescription = optimizeDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = optimizeCmd
    , commandPrereq = amInRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = [ uncompressNocompress
                               , umaskOption
                               ]
    , commandBasicOptions = [ workingRepoDir
                            , reorderPatches
                            , sibling
                            , relink
                            , upgradeFormat
                            , optimizePristine
                            , optimizeHTTP
                            ]
    }
optimizeCmd :: [DarcsFlag] -> [String] -> IO ()
optimizeCmd origopts _ = do
    when (UpgradeFormat `elem` origopts) optimizeUpgradeFormat
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
    cleanRepository repository -- garbage collect pristine.hashed directory
    when (OptimizeHTTP `elem` origopts) $ doOptimizeHTTP repository
    if OptimizePristine `elem` opts
       then doOptimizePristine repository
       else do when (Reorder `elem` opts) $ reorderInventory repository (compression opts) YesUpdateWorking (verbosity opts)
               when (Compress `elem` opts || UnCompress `elem` opts) $
                    optimizeCompression opts
               when (Relink `elem` opts) $ doRelink opts
    putStrLn "Done optimizing!"
  where opts = if UnCompress `elem` origopts then NoCompress:origopts else origopts

optimizeHelpCompression :: String
optimizeHelpCompression =
 "By default patches are compressed with zlib (RFC 1951) to reduce\n" ++
 "storage (and download) size.  In exceptional circumstances, it may be\n" ++
 "preferable to avoid compression.  In this case the `--dont-compress'\n" ++
 "option can be used (e.g. with `darcs record') to avoid compression.\n" ++
 "\n" ++
 "The `darcs optimize --uncompress' and `darcs optimize --compress'\n" ++
 "commands can be used to ensure existing patches in the current\n" ++
 "repository are respectively uncompressed or compressed.  Note that\n" ++
 "repositories in the legacy `old-fashioned-inventory' format have a .gz\n" ++
 "extension on patch files even when uncompressed.\n"

optimizeCompression :: [DarcsFlag] -> IO ()
optimizeCompression opts = do
    putStrLn "Optimizing (un)compression of patches..."
    do_compress (darcsdir++"/patches")
    putStrLn "Optimizing (un)compression of inventories..."
    do_compress (darcsdir++"/inventories")
    where do_compress f =
              do isd <- doesDirectoryExist f
                 if isd then withCurrentDirectory f $
                             do fs <- filter notdot `fmap` getDirectoryContents "."
                                mapM_ do_compress fs
                        else if Compress `elem` opts
                             then gzReadFilePS f >>= gzWriteAtomicFilePS f
                             else gzReadFilePS f >>= writeAtomicFilePS f
          notdot ('.':_) = False
          notdot _ = True

optimizeHelpRelink :: String
optimizeHelpRelink =
 "The `darcs optimize --relink' command hard-links patches that the\n" ++
 "current repository has in common with its peers.  Peers are those\n" ++
 "repositories listed in _darcs/prefs/sources, or defined with the\n" ++
 "`--sibling' option (which can be used multiple times).\n" ++
 "\n" ++
 "Darcs uses hard-links automatically, so this command is rarely needed.\n" ++
 "It is most useful if you used `cp -r' instead of `darcs get' to copy a\n" ++
 "repository, or if you pulled the same patch from a remote repository\n" ++
 "into multiple local repositories.\n"

doOptimizePristine :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
doOptimizePristine repo = do
  hashed <- doesFileExist $ darcsdir </> "hashed_inventory"
  when hashed $ do
    inv <- BS.readFile (darcsdir </> "hashed_inventory")
    let linesInv = BS.split '\n' inv
    case linesInv of
      [] -> return ()
      (pris_line:_) ->
          let size = decodeDarcsSize $ BS.drop 9 pris_line
           in when (isJust size) $ do putStrLn "Optimizing hashed pristine..."
                                      readRecorded repo >>= replacePristine repo
                                      cleanRepository repo

doRelink :: [DarcsFlag] -> IO ()
doRelink opts =
    do let some_siblings = flagsToSiblings opts
       defrepolist <- getPreflist "defaultrepo"
       let siblings = map toFilePath some_siblings ++ defrepolist
       if null siblings
          then putStrLn "No siblings -- no relinking done."
          else do debugMessage "Relinking patches..."
                  patch_tree <- expand =<< readPlainTree "_darcs/patches"
                  let patches = [ anchorPath "" p | (p, File _) <- list patch_tree ]
                  maybeRelinkFiles siblings patches "_darcs/patches"
                  debugMessage "Done relinking."

maybeRelinkFiles :: [String] -> [String] -> String -> IO ()
maybeRelinkFiles src dst dir =
    mapM_ (maybeRelinkFile src) (map ((dir ++ "/") ++) dst)

maybeRelinkFile :: [String] -> String -> IO ()
maybeRelinkFile [] _ = return ()
maybeRelinkFile (h:t) f =
    do done <- maybeRelink (h ++ "/" ++ f) f
       unless done $
           maybeRelinkFile t f

optimizeUpgradeFormat :: IO ()
optimizeUpgradeFormat = do
  debugMessage "Upgrading to hashed..."
  rf <- identifyRepoFormat "."
  debugMessage "Found our format"
  if formatHas HashedInventory rf
     then putStrLn "No action taken because this repository already is hashed."
     else do putStrLn "Checking repository in case of corruption..."
             withRepoLock NoDryRun YesUseCache YesUpdateWorking NoUMask $ RepoJob $ \repository ->
               actuallyUpgradeFormat repository

actuallyUpgradeFormat :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
actuallyUpgradeFormat repository = do
  -- convert patches/inventory
  patches <- readRepo repository
  let k = "Hashing patch"
  beginTedious k
  tediousSize k (lengthRL $ newset2RL patches)
  let patches' = progressPatchSet k patches
  cache <- getCaches YesUseCache "."
  let compr = compression [] -- default compression
  HashedRepo.writeTentativeInventory cache compr patches'
  endTedious k
  -- convert pristine by applying patches
  -- the faster alternative would be to copy pristine, but the apply method is more reliable
  let patchesToApply = progressFL "Applying patch" $ newset2FL patches'
  createDirectoryIfMissing False $ darcsdir </> hashedDir HashedPristineDir
  -- We ignore the returned root hash, we don't use it.
  _ <- writeDarcsHashed emptyTree "_darcs/pristine.hashed"
  sequence_ $ mapFL HashedRepo.applyToTentativePristine $ bunchFL 100 patchesToApply
  -- now make it official
  HashedRepo.finalizeTentativeChanges repository compr
  writeRepoFormat (createRepoFormat True False) (darcsdir </> "format")
  -- clean out old-fashioned junk
  debugMessage "Cleaning out old-fashioned repository files..."
  removeFile   $ darcsdir </> "inventory"
  removeFile   $ darcsdir </> "tentative_inventory"
  rmRecursive (darcsdir </> "pristine") `catchall` rmRecursive (darcsdir </> "current")
  rmGzsIn (darcsdir </> "patches")
  rmGzsIn (darcsdir </> "inventories")
  let checkpointDir = darcsdir </> "checkpoints"
  hasCheckPoints <- doesDirectoryExist checkpointDir
  when hasCheckPoints $ rmRecursive checkpointDir
  putStrLn "Done upgrading!"
 where
  rmGzsIn dir =
    withCurrentDirectory dir $ do
      gzs <- filter ((== ".gz") . takeExtension) `fmap` getDirectoryContents "."
      mapM_ removeFile gzs

doOptimizeHTTP :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
doOptimizeHTTP repo = flip finally (mapM_ removeFileIfExists
  [ darcsdir </> "meta-filelist-inventories"
  , darcsdir </> "meta-filelist-pristine"
  , basicTar <.> "part"
  , patchesTar <.> "part"
  ]) $ do
  rf <- identifyRepoFormat "."
  unless (formatHas HashedInventory rf) $ fail oldRepoFailMsg
  createDirectoryIfMissing False packsDir
  -- pack patchesTar
  ps <- mapRL hashedPatchFileName . newset2RL <$> readRepo repo
  is <- map ((darcsdir </> "inventories") </>) <$> HashedRepo.listInventories
  writeFile (darcsdir </> "meta-filelist-inventories") . unlines $
    map takeFileName is
  BL.writeFile (patchesTar <.> "part") . compress . write =<<
    mapM fileEntry' ((darcsdir </> "meta-filelist-inventories") : ps ++
    reverse is)
  renameFile (patchesTar <.> "part") patchesTar
  -- pack basicTar
  pr <- sortByMTime =<< dirContents "pristine.hashed"
  writeFile (darcsdir </> "meta-filelist-pristine") . unlines $
    map takeFileName pr
  BL.writeFile (basicTar <.> "part") . compress . write =<< mapM fileEntry' (
    [ darcsdir </> "meta-filelist-pristine"
    , darcsdir </> "hashed_inventory"
    ] ++ reverse pr)
  renameFile (basicTar <.> "part") basicTar
 where
  packsDir = darcsdir </> "packs"
  basicTar = packsDir </> "basic.tar.gz"
  patchesTar = packsDir </> "patches.tar.gz"
  fileEntry' x = unsafeInterleaveIO $ do
    content <- BL.fromChunks . return <$> gzReadFilePS x
    tp <- either fail return $ toTarPath False x
    return $ fileEntry tp content
  dirContents d = dirContents' d $ const True
  dirContents' d f = map ((darcsdir </> d) </>) . filter (\x ->
    head x /= '.' && f x) <$> getDirectoryContents (darcsdir </> d)
  hashedPatchFileName x = case extractHash x of
    Left _ -> fail "unexpected unhashed patch"
    Right h -> darcsdir </> "patches" </> h
  sortByMTime xs = map snd . sort <$> mapM (\x -> (\t -> (t, x)) <$>
    getModificationTime x) xs
  removeFileIfExists x = do
    ex <- doesFileExist x
    when ex $ removeFile x
