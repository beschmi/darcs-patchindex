--  Copyright (C) 2002-2005 David Roundy
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

{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using Prelude.catch
{-# LANGUAGE CPP #-}

module Darcs.UI.Commands.Get
    ( get
    , clone
    ) where

import System.Directory ( setCurrentDirectory
                        , doesDirectoryExist
                        , doesFileExist
                        , createDirectory )
import Workaround ( getCurrentDirectory )
import Control.Monad ( when, unless )

import Storage.Hashed.Tree( Tree )

import Darcs.UI.Commands ( DarcsCommand(..)
                      , nodefaults
                      , commandAlias
                      , putInfo
                      )
import Darcs.UI.Flags( compression, toMatchFlags, useCache, dryRun, umask, remoteRepos, setDefault , DarcsFlag(Quiet, XMLOutput, NoPatchIndexFlag), usePacks, remoteDarcs, getKind, verbosity )
import Darcs.Repository.Flags ( UpdateWorking (..), UseCache(..) )
import Darcs.UI.Arguments ( DarcsFlag( NewRepo
                                  , Lazy
                                  , UseNoWorkingDir
                                  , SetScriptsExecutable
                                  , OnePattern
                                  )
                       , getContext
                       , useWorkingDir
                       , partial
                       , reponame
                       , matchOneContext
                       , setScriptsExecutableOption
                       , networkOptions
                       , patchIndex
                       , noPatchIndex
                       )
import qualified Darcs.UI.Arguments as A ( setDefault, usePacks )
import Darcs.Repository ( Repository
                        , withRepository
                        , RepoJob(..)
                        , withRepoLock
                        , identifyRepositoryFor
                        , readRepo
                        , tentativelyRemovePatches
                        , patchSetToRepository
                        , copyRepository
                        , tentativelyAddToPending
                        , finalizeRepositoryChanges
                        , setScriptsExecutable
                        , invalidateIndex
                        , createRepository
                        , setScriptsExecutablePatches
                        )
import Darcs.Repository.Format ( identifyRepoFormat
                               , RepoFormat
                               , RepoProperty ( Darcs2
                                              , HashedInventory
                                              )
                               , formatHas
                               )
import Darcs.Patch ( RepoPatch
                   , apply
                   , invert
                   , effect
                   , PrimOf
                   )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered ( lengthFL, mapFL_FL, (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Repository.Prefs ( setDefaultrepo )
import Darcs.Repository.Motd ( showMotd )
import Darcs.Patch.Match ( havePatchsetMatch )
import Darcs.Repository.Match ( getOnePatchset )
import Progress ( debugMessage )
import Printer ( text, errorDoc, ($$) )
import Darcs.Path ( toFilePath, toPath, ioAbsoluteOrRemote)
import English ( englishNum, Noun(..) )
import Darcs.Repository.FileMod( createOrUpdatePatchIndexDisk )

getDescription :: String
getDescription = "Create a local copy of a repository."

getHelp :: String
getHelp =
 unlines
  [ "Get creates a local copy of a repository.  The optional second"
  , "argument specifies a destination directory for the new copy; if"
  , "omitted, it is inferred from the source location."
  , ""
  , "By default Darcs will copy every patch from the original repository."
  , "This means the copy is completely independent of the original; you can"
  , "operate on the new repository even when the original is inaccessible."
  , "If you expect the original repository to remain accessible, you can"
  , "use --lazy to avoid copying patches until they are needed (`copy on"
  , "demand').  This is particularly useful when copying a remote"
  , "repository with a long history that you don't care about."
  , ""
  , "The --lazy option isn't as useful for local copies, because Darcs will"
  , "automatically use `hard linking' where possible.  As well as saving"
  , "time and space, you can move or delete the original repository without"
  , "affecting a complete, hard-linked copy.  Hard linking requires that"
  , "the copy be on the same filesystem and the original repository, and"
  , "that the filesystem support hard linking.  This includes NTFS, HFS+"
  , "and all general-purpose Unix filesystems (such as ext3, UFS and ZFS)."
  , "FAT does not support hard links."
  , ""
  , "Darcs get will not copy unrecorded changes to the source repository's"
  , "working tree."
  , ""
  ] ++ getHelpTag

get :: DarcsCommand
get = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "get"
    , commandHelp = getHelp
    , commandDescription = getDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<REPOSITORY>", "[<DIRECTORY>]"]
    , commandCommand = getCmd
    , commandPrereq = contextExists
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = [A.usePacks, patchIndex, noPatchIndex] ++ networkOptions
    , commandBasicOptions =
         [
           reponame
         , partial
         , matchOneContext
         , A.setDefault True
         , setScriptsExecutableOption
         , useWorkingDir
         ]
    }

clone :: DarcsCommand
clone = commandAlias "clone" Nothing get

getCmd :: [DarcsFlag] -> [String] -> IO ()
getCmd opts [inrepodir, outname] = getCmd (NewRepo outname:opts) [inrepodir]
getCmd opts [inrepodir] = do
  debugMessage "Starting work on get..."
  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir
  unless (Quiet `elem` opts || XMLOutput `elem` opts) $ showMotd repodir
  rfsource <- identifyRepoFormat repodir
  debugMessage $ "Found the format of "++repodir++"..."
  mysimplename <- makeRepoName opts repodir
  createDirectory mysimplename
  setCurrentDirectory mysimplename
  createRepository (not $ formatHas Darcs2 rfsource) (UseNoWorkingDir `elem` opts) (not $ NoPatchIndexFlag `elem` opts)
  debugMessage "Finished initializing new directory."
  setDefaultrepo repodir (dryRun opts) (remoteRepos opts) (setDefault opts)

  if not (null [p | OnePattern p <- opts]) -- --to-match given
     && Lazy `notElem` opts
    then withRepository (useCache opts) $ RepoJob $ \repository -> do
      debugMessage "Using economical get --to-match handling"
      fromrepo <- identifyRepositoryFor repository (useCache opts) repodir
      Sealed patches_to_get <- getOnePatchset fromrepo (toMatchFlags opts)
      patchSetToRepository fromrepo patches_to_get (useCache opts) (compression opts) (remoteDarcs opts)
      debugMessage "Finished converting selected patch set to new repository"
    else copyRepoAndGoToChosenVersion opts repodir rfsource
  unless (NoPatchIndexFlag `elem` opts) $ withRepository NoUseCache $ RepoJob $ createOrUpdatePatchIndexDisk 
getCmd _ _ = fail "You must provide 'get' with either one or two arguments."

-- | called by getCmd
--   assumes that the target repo of the get is the current directory,
--   and that an inventory in the right format has already been created.
copyRepoAndGoToChosenVersion :: [DarcsFlag]
                             -> String
                             -> RepoFormat
                             -> IO ()
copyRepoAndGoToChosenVersion opts repodir rfsource =
  withRepository (useCache opts) $ RepoJob $ \repository -> do
     debugMessage "Identifying and copying repository..."
     unless (formatHas HashedInventory rfsource) $ putInfo opts $
          text "***********************************************************************"
       $$ text "  _______   Sorry for the wait! The repository you are fetching is"
       $$ text " |       |  using the DEPRECATED 'old-fashioned' format. I'm getting a"
       $$ text " | O   O |  hashed copy instead, but this may take a while."
       $$ text " |  ___  |"
       $$ text " | /   \\ |  We recommend that the maintainer upgrade the remote copy"
       $$ text " |_______|  as well. See http://wiki.darcs.net/OF for more information."
       $$ text ""
       $$ text "***********************************************************************"
     r <- identifyRepositoryFor repository (useCache opts) repodir
     copyRepository r (verbosity opts) (useCache opts) (getKind opts)
                      (compression opts) (dryRun opts)
                      (umask opts) (remoteDarcs opts)
                      (UseNoWorkingDir `notElem` opts) (usePacks opts) (not $ NoPatchIndexFlag `elem` opts)
     when (SetScriptsExecutable `elem` opts) setScriptsExecutable
     goToChosenVersion repository opts
     putInfo opts $ text "Finished getting."

makeRepoName :: [DarcsFlag] -> FilePath -> IO String
makeRepoName dfs d =
 case [ n | NewRepo n <- dfs] of
  (n:_) ->
    do exists <- doesDirectoryExist n
       file_exists <- doesFileExist n
       if exists || file_exists
          then fail $ "Directory or file named '" ++ n ++ "' already exists."
          else return n
  [] ->
    case dropWhile (=='.') $ reverse $
         takeWhile (\c -> c /= '/' && c /= ':') $
         dropWhile (=='/') $ reverse d of
      ""   -> modifyRepoName "anonymous_repo"
      base -> modifyRepoName base

modifyRepoName :: String -> IO String
modifyRepoName name =
    if head name == '/'
    then mrn name (-1)
    else do cwd <- getCurrentDirectory
            mrn (cwd ++ "/" ++ name) (-1)
 where
  mrn :: String -> Int -> IO String
  mrn n i = do
    exists <- doesDirectoryExist thename
    file_exists <- doesFileExist thename
    if not exists && not file_exists
       then do when (i /= -1) $
                    putStrLn $ "Directory '"++ n ++
                               "' already exists, creating repository as '"++
                               thename ++"'"
               return thename
       else mrn n $ i+1
    where thename = if i == -1 then n else n++"_"++show i

getHelpTag :: String
getHelpTag =
 unlines
  [ "It is often desirable to make a copy of a repository that excludes"
  , "some patches.  For example, if releases are tagged then `darcs get"
  , "--tag .' would make a copy of the repository as at the latest release."
  , ""
  , "An untagged repository state can still be identified unambiguously by"
  , "a context file, as generated by `darcs changes --context'.  Given the"
  , "name of such a file, the --context option will create a repository"
  , "that includes only the patches from that context.  When a user reports"
  , "a bug in an unreleased version of your project, the recommended way to"
  , "find out exactly what version they were running is to have them" 
  , "include a context file in the bug report."
  , ""
  , "You can also make a copy of an untagged state using the --to-patch or"
  , "--to-match options, which exclude patches `after' the first matching"
  , "patch.  Because these options treat the set of patches as an ordered"
  , "sequence, you may get different results after reordering with `darcs"
  , "optimize', so tagging is preferred."
  ]

contextExists :: [DarcsFlag] -> IO (Either String ())
contextExists opts =
   case getContext opts of
     Nothing -> return $ Right ()
     Just f  -> do exists <- doesFileExist $ toFilePath f
                   return $ if exists
                            then Right ()
                            else Left $ "Context file "++toFilePath f++" does not exist"

goToChosenVersion :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
                  => Repository p wR wU wR
                  -> [DarcsFlag] -> IO ()
goToChosenVersion repository opts =
    let matchFlags = toMatchFlags opts
    in
    when (havePatchsetMatch matchFlags) $ do
       debugMessage "Going to specified version..."
       patches <- readRepo repository
       Sealed context <- getOnePatchset repository matchFlags
       when (snd (countUsThem patches context) > 0) $
            errorDoc $ text "Missing patches from context!" -- FIXME : - (
       _ :> us' <- return $ findCommonWithThem patches context
       let ps = mapFL_FL hopefully us'
       putInfo opts $ text $ "Unapplying " ++ show (lengthFL ps) ++ " " ++
                   englishNum (lengthFL ps) (Noun "patch") ""
       invalidateIndex repository
       withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \_ ->
           do _ <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking us'
              tentativelyAddToPending repository (dryRun opts) YesUpdateWorking $ invert $ effect us'
              finalizeRepositoryChanges repository (dryRun opts) YesUpdateWorking (compression opts) (not $ NoPatchIndexFlag `elem` opts)
              apply (invert $ effect ps) `catch` \e ->
                  fail ("Couldn't undo patch in working dir.\n" ++ show e)
              when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches (invert $ effect ps)

