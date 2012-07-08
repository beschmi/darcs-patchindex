-- Copyright (C) 2002-2004,2007-2008 David Roundy
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
{-# LANGUAGE CPP, ScopedTypeVariables, Rank2Types, RankNTypes, PatternGuards #-}


module Darcs.Repository.Internal
    ( Repository(..)
    , RepoType(..)
    , RepoJob(..)
    , maybeIdentifyRepository
    , identifyDarcsRepository
    , identifyRepositoryFor
    , IdentifyRepo(..)
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , revertRepositoryChanges
    , announceMergeConflicts
    , setTentativePending
    , checkUnrecordedConflicts
    , readRepo
    , readTentativeRepo
    , readRepoUsingSpecificInventory
    , prefsUrl
    , makePatchLazy
    , withRecorded
    , withTentative
    , withRepoLock
    , withRepoReadLock
    , withRepository
    , withRepositoryDirectory
    , withGutsOf
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyAddToPending
    , tentativelyAddPatch_
    , tentativelyReplacePatches
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
    , UpdatePristine(..)
    , MakeChanges(..)
    , applyToTentativePristine
    , makeNewPending
    , seekRepo
    ) where

import Printer ( putDocLn
               , (<+>)
               , text
               , ($$)
               , redText
               , putDocLnWith
               , ($$)
               )
import Darcs.ColorPrinter (fancyPrinters)

import Darcs.Repository.State ( readRecorded
                              , readWorking
                              )
import Darcs.Repository.LowLevel
    ( readPending
    , readTentativePending
    , writeTentativePending
    , readNewPending
    , writeNewPending
    , pendingName
    )
import System.Exit ( ExitCode(..)
                   , exitWith
                   )
import Darcs.Repository.ApplyPatches
    ( runTolerantly
    , runSilently
    )

import Darcs.SignalHandler ( withSignalsBlocked )
import Darcs.Repository.Format ( RepoFormat
                               , RepoProperty( Darcs2
                                             , HashedInventory
                                             , NoWorkingDir
                                             )
                               , tryIdentifyRepoFormat
                               , formatHas
                               , writeProblem
                               , readProblem
                               , readfromAndWritetoProblem
                               )
import System.Directory ( doesDirectoryExist
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        )
import Control.Monad ( when
                     , unless
                     , filterM
                     , void
                     )
import Control.Applicative ( (<$>) )
import Workaround ( getCurrentDirectory
                  , renameFile
                  , setExecutable
                  )

import qualified Data.ByteString as B ( readFile
                                      , isPrefixOf
                                      )
import qualified Data.ByteString.Char8 as BC (pack)

import Darcs.Patch ( Effect
                   , primIsHunk
                   , primIsBinary
                   , description
                   , tryToShrink
                   , commuteFLorComplain
                   , commute
                   , fromPrim
                   )

import Darcs.Patch.Dummy ( DummyPatch )

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.V1 ( Patch )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.Prim.V1 ( Prim )

import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Prim ( PrimPatchBase
                        , PrimOf
                        , tryShrinkingInverse
                        , PrimPatch
                        )
import Darcs.Patch.Bundle ( scanBundle
                          , makeBundleN
                          )
import Darcs.Patch.Info ( isTag )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , hopefully
    , info
    )
import qualified Darcs.Repository.HashedRepo as HashedRepo
                            ( revertTentativeChanges
                            , finalizeTentativeChanges
                            , removeFromTentativeInventory
                            , copyPristine
                            , copyPartialsPristine
                            , applyToTentativePristine
                            , writeAndReadPatch
                            , addToTentativeInventory
                            , readRepo
                            , readTentativeRepo
                            , readRepoUsingSpecificInventory
                            , cleanPristine
                            )
import qualified Darcs.Repository.Old as Old
                            ( readOldRepo
                            , revertTentativeChanges
                            , oldRepoFailMsg
                            )
import Darcs.Repository.Flags
    ( Compression, Verbosity(..), UseCache(..), UpdateWorking (..), DryRun(..), UMask (..), AllowConflicts (..), ExternalMerge (..), WorkRepo (..)  )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Unsafe
    ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , (:\/:)(..)
    , (:/\:)(..)
    , (:>)(..)
    , (+>+)
    , (+<+)
    , lengthFL
    , allFL
    , filterFLFL
    , reverseFL
    , mapFL_FL
    , concatFL
    , reverseRL
    , mapRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(Sealed)
    , seal
    , FlippedSeal(FlippedSeal)
    , flipSeal
    , mapSeal
    )
import Darcs.Patch ( RepoPatch
                   , Patchy
                   , merge
                   , listConflictedFiles
                   , listTouchedFiles
                   , Named
                   , commuteRL
                   , fromPrims
                   , readPatch
                   , effect
                   , invert
                   , primIsAddfile
                   , primIsAdddir
                   , primIsSetpref
                   , apply
                   , applyToTree
                   )
import Darcs.Patch.Permutations ( commuteWhatWeCanFL
                                , removeFL
                                )
import Darcs.Patch.Set ( PatchSet(..)
                       , SealedPatchSet
                       , newset2FL
                       , newset2RL
                       , Origin
                       )
import Darcs.Patch.Depends ( removeFromPatchSet
                           , mergeThem
                           , splitOnTag
                           )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Path
    ( FilePathLike
    , AbsolutePath
    , toFilePath
    , ioAbsoluteOrRemote
    , toPath
    , anchorPath
    )
import Darcs.Utils ( promptYorn
                   , catchall
                   , withCurrentDirectory
                   , withUMask
                   , nubsort
                   )
import Progress ( debugMessage )
import Darcs.Patch.Progress (progressFL)
import Darcs.URL ( isFile )
import Darcs.Repository.Prefs ( getCaches )
import Darcs.Repository.Lock
    ( withLock
    , writeDocBinFile
    , removeFileMayNotExist
    )
import Darcs.Repository.InternalTypes( Repository(..)
                                     , RepoType(..)
                                     , Pristine(..)
                                     )
import Darcs.Global ( darcsdir )

import System.Mem( performGC )

import qualified Storage.Hashed.Tree as Tree
import Storage.Hashed.Tree ( Tree )

#include "impossible.h"

-- | The status of a given directory: is it a darcs repository?
data IdentifyRepo p wR wU wT = BadRepository String -- ^ looks like a repository with some error
                             | NonRepository String -- ^ safest guess
                             | GoodRepository (Repository p wR wU wT)

-- | Tries to identify the repository in a given directory
maybeIdentifyRepository :: UseCache -> String -> IO (IdentifyRepo p wR wU wT)
maybeIdentifyRepository useCache "." =
    do darcs <- doesDirectoryExist darcsdir
       repoFormatOrError <- tryIdentifyRepoFormat "."
       here <- toPath `fmap` ioAbsoluteOrRemote "."
       case repoFormatOrError of
         Left err -> return $ NonRepository err
         Right rf ->
             case readProblem rf of
             Just err -> return $ BadRepository err
             Nothing -> if darcs then do pris <- identifyPristine
                                         cs <- getCaches useCache here
                                         return $ GoodRepository $ Repo here rf (DarcsRepository pris cs)
                                 else return (NonRepository "Not a repository")
maybeIdentifyRepository useCache url' =
 do url <- toPath `fmap` ioAbsoluteOrRemote url'
    repoFormatOrError <- tryIdentifyRepoFormat url
    case repoFormatOrError of
      Left e -> return $ NonRepository e
      Right rf -> case readProblem rf of
                  Just err -> return $ BadRepository err
                  Nothing ->  do cs <- getCaches useCache url
                                 return $ GoodRepository $ Repo url rf (DarcsRepository NoPristine cs)

identifyPristine :: IO Pristine
identifyPristine =
    do pristine <- doesDirectoryExist $ darcsdir++"/pristine"
       current  <- doesDirectoryExist $ darcsdir++"/current"
       hashinv  <- doesFileExist      $ darcsdir++"/hashed_inventory"
       case (pristine || current, hashinv) of
           (False, False) -> return NoPristine
           (True,  False) -> return PlainPristine
           (False, True ) -> return HashedPristine
           _ -> fail "Multiple pristine trees."

-- | identifyDarcsRepository identifies the repo at 'url'. Warning:
-- you have to know what kind of patches are found in that repo.
identifyDarcsRepository :: forall p wR wU wT. UseCache -> String
                           -> IO (Repository p wR wU wT)
identifyDarcsRepository useCache url =
    do er <- maybeIdentifyRepository useCache url
       case er of
         BadRepository s -> fail s
         NonRepository s -> fail s
         GoodRepository r -> return r

-- TODO: Is this type really right? It seems to be getting a different repository to the one passed in,
-- so surely it shouldn't have the same witnesses?
-- | @identifyRepositoryFor repo url@ identifies (and returns) the repo at 'url',
-- but fails if it is not compatible for reading from and writing to.
identifyRepositoryFor :: forall p wR wU wT. RepoPatch p
                      => Repository p wR wU wT
                      -> UseCache
                      -> String
                      -> IO (Repository p wR wU wT)
identifyRepositoryFor (Repo _ rf _) useCache url =
    do Repo absurl rf_ t <- identifyDarcsRepository useCache url
       let t' = case t of DarcsRepository x c -> DarcsRepository x c
       case readfromAndWritetoProblem rf_ rf of
         Just e -> fail $ "Incompatibility with repository " ++ url ++ ":\n" ++ e
         Nothing -> return $ Repo absurl rf_ t'

amInRepository :: WorkRepo -> IO (Either String ())
amInRepository (WorkRepoDir d) = do
       setCurrentDirectory d `catchall` (fail $ "can't set directory to "++d)
       status <- maybeIdentifyRepository YesUseCache "."
       case status of
         GoodRepository _ -> return (Right ())
         BadRepository  e -> return (Left $ "While " ++ d ++ " looks like a repository directory, we have a problem with it:\n" ++ e)
         NonRepository  _ -> return (Left "You need to be in a repository directory to run this command.")
amInRepository _ =
       maybe (Left "You need to be in a repository directory to run this command.") id <$> seekRepo

amInHashedRepository :: WorkRepo -> IO (Either String ())
amInHashedRepository wd
 = do inrepo <- amInRepository wd
      case inrepo of
       Right _ -> do pristine <- identifyPristine
                     case pristine of
                       HashedPristine -> return (Right ())
                       _ -> return (Left Old.oldRepoFailMsg)
       left    -> return left

-- | hunt upwards for the darcs repository
-- This keeps changing up one parent directory, testing at each
-- step if the current directory is a repository or not.  $
-- The result is:
--   Nothing, if no repository found
--   Just (Left errorMessage), if bad repository found
--   Just (Right ()), if good repository found.
-- WARNING this changes the current directory for good if matchFn succeeds
seekRepo :: IO (Maybe (Either String ()))
seekRepo = getCurrentDirectory >>= helper where
   helper startpwd = do
    status <- maybeIdentifyRepository YesUseCache "."
    case status of
      GoodRepository _ -> return . Just $ Right ()
      BadRepository e  -> return . Just $ Left e
      NonRepository _ ->
            do cd <- toFilePath `fmap` getCurrentDirectory
               setCurrentDirectory ".."
               cd' <- toFilePath `fmap` getCurrentDirectory
               if cd' /= cd
                  then helper startpwd
                  else do setCurrentDirectory startpwd
                          return Nothing

-- The performGC in this function is a workaround for a library/GHC bug,
-- http://hackage.haskell.org/trac/ghc/ticket/2924 -- (doesn't seem to be a
-- problem on fast machines, but virtual ones trip this from time to time)
amNotInRepository :: WorkRepo -> IO (Either String ())
amNotInRepository (WorkRepoDir d) = do
    createDirectoryIfMissing False d
       `catchall` (performGC >> createDirectoryIfMissing False d)
    -- note that the above could always fail
    setCurrentDirectory d
    amNotInRepository WorkRepoCurrentDir
amNotInRepository _ = do
       status <- maybeIdentifyRepository YesUseCache "."
       case status of
         GoodRepository _ -> return (Left $ "You may not run this command in a repository.")
         BadRepository e  -> return (Left $ "You may not run this command in a repository.\nBy the way, we have a problem with it:\n" ++ e)
         NonRepository _  -> return (Right ())

findRepository :: WorkRepo -> IO (Either String ())
findRepository (WorkRepoURL d) | isFile d =
    do setCurrentDirectory d `catchall` (fail $ "can't set directory to "++d)
       findRepository WorkRepoCurrentDir
findRepository (WorkRepoDir d) =
    do setCurrentDirectory d `catchall` (fail $ "can't set directory to "++d)
       findRepository WorkRepoCurrentDir
findRepository _ = maybe (Right ()) id <$> seekRepo

-- TODO: see also Repository.State.readPendingLL ... to be removed after GHC 7.2
readNewPendingLL :: (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT -> IO (Sealed ((FL p) wT))
readNewPendingLL repo = mapSeal (mapFL_FL fromPrim) `fmap` readNewPending repo

makeNewPending :: forall p wR wU wT wY. (RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wT
                 -> UpdateWorking
                 -> FL (PrimOf p) wT wY
                 -> IO ()
makeNewPending _ NoUpdateWorking _ = return ()
makeNewPending repo@(Repo r _ tp) _ origp =
    withCurrentDirectory r $
    do let newname = pendingName tp ++ ".new"
       debugMessage $ "Writing new pending:  " ++ newname
       Sealed sfp <- return $ siftForPending origp
       writeNewPending repo sfp
       cur <- readRecorded repo
       Sealed p <- readNewPendingLL repo -- :: IO (Sealed (FL (PrimOf p) wT))
       -- We don't ever use the resulting tree.
       _ <- catch (applyToTree p cur) $ \err -> do
         let buggyname = pendingName tp ++ "_buggy"
         renameFile newname buggyname
         bugDoc $ text ("There was an attempt to write an invalid pending! " ++ show err)
                    $$ text "If possible, please send the contents of"
                    <+> text buggyname
                    $$ text "along with a bug report."
       renameFile newname (pendingName tp)
       debugMessage $ "Finished writing new pending:  " ++ newname

siftForPending :: forall prim wX wY . PrimPatch prim => FL prim wX wY -> Sealed (FL prim wX)
siftForPending simple_ps =
 let oldps = maybe simple_ps id $ tryShrinkingInverse $ crudeSift simple_ps
 in if allFL (\p -> primIsAddfile p || primIsAdddir p) $ oldps
    then seal oldps
    else fromJust $ do
      Sealed x <- return $ sfp NilFL $ reverseFL oldps
      return (case tryToShrink x of
              ps | lengthFL ps < lengthFL oldps -> siftForPending ps
                 | otherwise -> seal ps)
      where sfp :: FL prim wA wB -> RL prim wC wA -> Sealed (FL prim wC)
            sfp sofar NilRL = seal sofar
            sfp sofar (p:<:ps)
                | primIsHunk p || primIsBinary p
                    = case commuteFLorComplain (p :> sofar) of
                      Right (sofar' :> _) -> sfp sofar' ps
                      Left _ -> sfp (p:>:sofar) ps
            sfp sofar (p:<:ps) = sfp (p:>:sofar) ps

-- @todo: we should not have to open the result of HashedRepo and
-- seal it.  Instead, update this function to work with type witnesses
-- by fixing DarcsRepo to match HashedRepo in the handling of
-- Repository state.
readRepo :: (RepoPatch p, ApplyState p ~ Tree)
         => Repository p wR wU wT
         -> IO (PatchSet p Origin wR)
readRepo repo@(Repo r rf _)
    | formatHas HashedInventory rf = HashedRepo.readRepo repo r
    | otherwise = do Sealed ps <- Old.readOldRepo r
                     return $ unsafeCoerceP ps

readTentativeRepo :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository p wR wU wT
                  -> IO (PatchSet p Origin wT)
readTentativeRepo repo@(Repo r rf _)
    | formatHas HashedInventory rf = HashedRepo.readTentativeRepo repo r
    | otherwise = fail Old.oldRepoFailMsg

readRepoUsingSpecificInventory :: (RepoPatch p, ApplyState p ~ Tree)
                               => String
                               -> Repository p wR wU wT
                               -> IO (PatchSet p Origin wT)
readRepoUsingSpecificInventory invPath repo@(Repo r rf _)
    | formatHas HashedInventory rf =
        HashedRepo.readRepoUsingSpecificInventory invPath repo r
    | otherwise = fail Old.oldRepoFailMsg

makePatchLazy :: RepoPatch p
              => Repository p wR wU wT
              -> Compression
              -> PatchInfoAnd p wX wY
              -> IO (PatchInfoAnd p wX wY)
makePatchLazy (Repo r rf (DarcsRepository _ c)) compr p
    | formatHas HashedInventory rf =
         withCurrentDirectory r $ HashedRepo.writeAndReadPatch c compr p
    | otherwise = fail Old.oldRepoFailMsg

prefsUrl :: Repository p wR wU wT -> String
prefsUrl (Repo r _ (DarcsRepository _ _)) = r ++ "/"++darcsdir++"/prefs"

unrevertUrl :: Repository p wR wU wT -> String
unrevertUrl (Repo r _ (DarcsRepository _ _)) = r ++ "/"++darcsdir++"/patches/unrevert"

applyToWorking :: (ApplyState (PrimOf p) ~ Tree, RepoPatch p)
               => Repository p wR wU wT -> Verbosity -> FL (PrimOf p) wU wY
               -> IO (Repository p wR wY wT)
applyToWorking (Repo r rf (DarcsRepository t c)) verb patch =
  do
    unless (formatHas NoWorkingDir rf) $
      withCurrentDirectory r $ if verb == Quiet
                               then runSilently $ apply patch
                               else runTolerantly $ apply patch
    return (Repo r rf (DarcsRepository t c))

handlePendForAdd :: forall p wR wU wT wX wY. (RepoPatch p)
                    => Repository p wR wU wT
                    -> UpdateWorking
                    -> PatchInfoAnd p wX wY
                    -> IO ()
handlePendForAdd (Repo _ _ _) NoUpdateWorking _ = return ()
handlePendForAdd repo _ p =
    do
       Sealed pend <- readTentativePending repo
       let effectp = if allFL isSimple pend then crudeSift $ effect p
                                             else effect p
       Sealed newpend <- return $ rmpend (progressFL "Removing from pending:" effectp) (unsafeCoercePStart pend)
       writeTentativePending repo (unsafeCoercePStart newpend)
    where rmpend :: FL (PrimOf p) wA wB -> FL (PrimOf p) wA wC -> Sealed (FL (PrimOf p) wB)
          rmpend NilFL x = Sealed x
          rmpend _ NilFL = Sealed NilFL
          rmpend (x:>:xs) xys | Just ys <- removeFL x xys = rmpend xs ys
          rmpend (x:>:xs) ys =
              case commuteWhatWeCanFL (x:>xs) of
              a:>x':>b -> case rmpend a ys of
                          Sealed ys' -> case commute (invert (x':>:b) :> ys') of
                                        Just (ys'' :> _) -> seal ys''
                                        Nothing -> seal $ invert (x':>:b)+>+ys'
                                        -- DJR: I don't think this
                                        -- last case should be
                                        -- reached, but it also
                                        -- shouldn't lead to
                                        -- corruption.

isSimple :: PrimPatch prim => prim wX wY -> Bool
isSimple x = primIsHunk x || primIsBinary x || primIsSetpref x

crudeSift :: forall prim wX wY . PrimPatch prim => FL prim wX wY -> FL prim wX wY
crudeSift xs = if allFL isSimple xs then filterFLFL ishunkbinary xs else xs
    where ishunkbinary :: prim wA wB -> EqCheck wA wB
          ishunkbinary x | primIsHunk x || primIsBinary x = unsafeCoerceP IsEq
                         | otherwise = NotEq

data HashedVsOld a = HvsO { old, hashed :: a }

decideHashedOrNormal :: Monad m => RepoFormat -> HashedVsOld (m a) -> m a
decideHashedOrNormal rf (HvsO { hashed = h, old = o })
    | formatHas HashedInventory rf = h
    | otherwise = o

data MakeChanges = MakeChanges | DontMakeChanges deriving ( Eq )

announceMergeConflicts :: (PrimPatch p, PatchInspect p)
                       => String
                       -> AllowConflicts
                       -> ExternalMerge
                       -> FL p wX wY
                       -> IO Bool
announceMergeConflicts cmd allowConflicts externalMerge resolved_pw =
  case nubsort $ listTouchedFiles resolved_pw of
    [] -> return False
    cfs -> if allowConflicts `elem` [YesAllowConflicts,YesAllowConflictsAndMark]
              || externalMerge /= NoExternalMerge
           then do putDocLnWith fancyPrinters $ 
                     redText "We have conflicts in the following files:" $$ text (unlines cfs)
                   return True
           else do putDocLnWith fancyPrinters $
                     redText "There are conflicts in the following files:" $$ text (unlines cfs)
                   fail $ "Refusing to "++cmd++" patches leading to conflicts.\n"++
                          "If you would rather apply the patch and mark the conflicts,\n"++
                          "use the --mark-conflicts or --allow-conflicts options to "++cmd++"\n"++
                          "These can set as defaults by adding\n"++
                          " "++cmd++" mark-conflicts\n"++
                          "to "++darcsdir++"/prefs/defaults in the target repo. "

checkUnrecordedConflicts :: forall p wT wY. RepoPatch p
                         => UpdateWorking
                         -> UseCache
                         -> FL (Named p) wT wY
                         -> IO Bool
checkUnrecordedConflicts NoUpdateWorking _ _ = return False
checkUnrecordedConflicts _ useCache pc =
    do repository <- identifyDarcsRepository useCache "."
       cuc repository
    where cuc :: Repository p wR wU wT -> IO Bool
          cuc r = do Sealed (mpend :: FL (PrimOf p) wT wX) <- readPending r :: IO (Sealed (FL (PrimOf p) wT))
                     case mpend of
                       NilFL -> return False
                       pend ->
                           case merge (fromPrims_ pend :\/: fromPrims_ (concatFL $ mapFL_FL effect pc)) of
                           _ :/\: pend' ->
                               case listConflictedFiles pend' of
                               [] -> return False
                               fs -> do putStrLn ("You have conflicting local changes to:\n"
                                                 ++ unwords fs)
                                        confirmed <- promptYorn "Proceed?"
                                        unless confirmed $
                                             do putStrLn "Cancelled."
                                                exitWith ExitSuccess
                                        return True
          fromPrims_ :: FL (PrimOf p) wA wB -> FL p wA wB
          fromPrims_ = fromPrims

tentativelyAddPatch :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository p wR wU wT
                    -> Compression
                    -> Verbosity
                    -> UpdateWorking
                    -> PatchInfoAnd p wT wY
                    -> IO (Repository p wR wU wY)
tentativelyAddPatch = tentativelyAddPatch_ UpdatePristine

data UpdatePristine = UpdatePristine | DontUpdatePristine deriving Eq

-- TODO re-add a safety catch for --dry-run? Maybe using a global, like dryRun
-- :: Bool, with dryRun = unsafePerformIO $ readIORef ...
tentativelyAddPatch_ :: (RepoPatch p, ApplyState p ~ Tree)
                     => UpdatePristine
                     -> Repository p wR wU wT
                     -> Compression
                     -> Verbosity
                     -> UpdateWorking
                     -> PatchInfoAnd p wT wY
                     -> IO (Repository p wR wU wY)
tentativelyAddPatch_ up r@(Repo dir rf (DarcsRepository t c)) compr verb uw p =
    withCurrentDirectory dir $ do
       decideHashedOrNormal rf $ HvsO {
          hashed = void $ HashedRepo.addToTentativeInventory c compr p,
          old = fail Old.oldRepoFailMsg}
       when (up == UpdatePristine) $ do debugMessage "Applying to pristine cache..."
                                        applyToTentativePristine r verb p
                                        debugMessage "Updating pending..."
                                        handlePendForAdd r uw p
       return (Repo dir rf (DarcsRepository t c))

applyToTentativePristine :: (ApplyState q ~ Tree, Effect q, Patchy q, ShowPatch q, PrimPatchBase q)
                         => Repository p wR wU wT
                         -> Verbosity
                         -> q wT wY
                         -> IO ()
applyToTentativePristine (Repo dir rf (DarcsRepository _ _)) verb p =
    withCurrentDirectory dir $
    do when (verb == Verbose) $ putDocLn $ text "Applying to pristine..." <+> description p
       decideHashedOrNormal rf $ HvsO {hashed = HashedRepo.applyToTentativePristine p,
                                       old = fail Old.oldRepoFailMsg}

-- | This fuction is unsafe because it accepts a patch that works on the tentative
-- pending and we don't currently track the state of the tentative pending.
tentativelyAddToPending :: forall p wR wU wT wX wY. RepoPatch p
                        => Repository p wR wU wT
                        -> DryRun
                        -> UpdateWorking
                        -> FL (PrimOf p) wX wY
                        -> IO ()
tentativelyAddToPending _ YesDryRun _ _
    = bug "tentativelyAddToPending called when --dry-run is specified"
tentativelyAddToPending _ _ NoUpdateWorking _ = return ()
tentativelyAddToPending repo@(Repo dir _ _) _ _ patch =
    withCurrentDirectory dir $ do
      Sealed pend <- readTentativePending repo
      FlippedSeal newpend_ <- return $ newpend (unsafeCoerceP pend :: FL (PrimOf p) wA wX) patch
      writeTentativePending repo (unsafeCoercePStart newpend_)
      where newpend :: FL prim wA wB -> FL prim wB wC -> FlippedSeal (FL prim) wC
            newpend NilFL patch_ = flipSeal patch_
            newpend p     patch_ = flipSeal $ p +>+ patch_

-- | setTentativePending is basically unsafe.  It overwrites the pending
--   state with a new one, not related to the repository state.
setTentativePending :: forall p wR wU wT wX wY. RepoPatch p
                    => Repository p wR wU wT
                    -> UpdateWorking
                    -> FL (PrimOf p) wX wY
                    -> IO ()
setTentativePending _ NoUpdateWorking _ = return ()
setTentativePending repo@(Repo dir _ _) _ patch = do
    Sealed prims <- return $ siftForPending patch
    withCurrentDirectory dir $ writeTentativePending repo (unsafeCoercePStart prims)

-- | prepend is basically unsafe.  It overwrites the pending state
-- with a new one, not related to the repository state.
prepend :: forall p wR wU wT wX wY. RepoPatch p
        => Repository p wR wU wT
        -> UpdateWorking
        -> FL (PrimOf p) wX wY
        -> IO ()
prepend (Repo _ _ _) NoUpdateWorking _ = return ()
prepend repo@(Repo _ _ _) _ patch =
    do
       Sealed pend <- readTentativePending repo
       Sealed newpend_ <- return $ newpend (unsafeCoerceP pend) patch
       writeTentativePending repo (unsafeCoercePStart $ crudeSift newpend_)
      where newpend :: FL prim wB wC -> FL prim wA wB -> Sealed (FL prim wA)
            newpend NilFL patch_ = seal patch_
            newpend p     patch_ = seal $ patch_ +>+ p

tentativelyRemovePatches :: (RepoPatch p, ApplyState p ~ Tree)
                         => Repository p wR wU wT
                         -> Compression
                         -> UpdateWorking
                         -> FL (PatchInfoAnd p) wX wT
                         -> IO (Repository p wR wU wX)
tentativelyRemovePatches = tentativelyRemovePatches_ UpdatePristine

tentativelyRemovePatches_ :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree)
                          => UpdatePristine
                          -> Repository p wR wU wT
                          -> Compression
                          -> UpdateWorking
                          -> FL (PatchInfoAnd p) wX wT
                          -> IO (Repository p wR wU wX)
tentativelyRemovePatches_ up repository@(Repo dir rf (DarcsRepository t c)) compr uw ps =
    withCurrentDirectory dir $ do
      when (up == UpdatePristine) $ do debugMessage "Adding changes to pending..."
                                       prepend repository uw $ effect ps
      removeFromUnrevertContext repository ps
      debugMessage "Removing changes from tentative inventory..."
      if formatHas HashedInventory rf
        then do HashedRepo.removeFromTentativeInventory repository compr ps
                when (up == UpdatePristine) $
                     HashedRepo.applyToTentativePristine $
                     progressFL "Applying inverse to pristine" $ invert ps
        else fail Old.oldRepoFailMsg
      return (Repo dir rf (DarcsRepository t c))

tentativelyReplacePatches :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> Compression
                          -> UpdateWorking
                          -> Verbosity
                          -> FL (PatchInfoAnd p) wX wT
                          -> IO ()
tentativelyReplacePatches repository compr uw verb ps =
    do repository' <- tentativelyRemovePatches_ DontUpdatePristine repository compr uw ps
       mapAdd repository' ps
  where mapAdd :: Repository p wM wL wI
               -> FL (PatchInfoAnd p) wI wJ
               -> IO ()
        mapAdd _ NilFL = return ()
        mapAdd r (a:>:as) =
               do r' <- tentativelyAddPatch_ DontUpdatePristine r compr verb uw a
                  mapAdd r' as

finalizePending :: (RepoPatch p, ApplyState p ~ Tree)
                => Repository p wR wU wT
                -> UpdateWorking
                -> IO ()
finalizePending (Repo dir _ rt) NoUpdateWorking =
        withCurrentDirectory dir $ removeFileMayNotExist $ (pendingName rt)
finalizePending repository@(Repo dir _ _) updateWorking = do
  withCurrentDirectory dir $ do
      Sealed tpend <- readTentativePending repository
      Sealed new_pending <- return $ siftForPending tpend
      makeNewPending repository updateWorking new_pending

finalizeRepositoryChanges :: (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> DryRun
                          -> UpdateWorking
                          -> Compression
                          -> IO ()
finalizeRepositoryChanges (Repo _ _ _) YesDryRun _ _
    = bug "finalizeRepositoryChanges called when --dry-run specified"
finalizeRepositoryChanges repository@(Repo dir rf _) _ updateWorking compr
    | formatHas HashedInventory rf =
        withCurrentDirectory dir $ do
            debugMessage "Finalizing changes..."
            withSignalsBlocked $ do
                 HashedRepo.finalizeTentativeChanges repository compr
                 finalizePending repository updateWorking
            debugMessage "Done finalizing changes..."
    | otherwise = fail Old.oldRepoFailMsg

revertRepositoryChanges :: RepoPatch p
                        => Repository p wR wU wT
                        -> DryRun
                        -> UpdateWorking
                        -> IO ()
revertRepositoryChanges (Repo _ _ _) YesDryRun _
    = bug "revertRepositoryChanges called when --dry-run is specified"
revertRepositoryChanges r@(Repo dir rf dr@(DarcsRepository _ _)) _ uw =
    withCurrentDirectory dir $
    do removeFileMayNotExist (pendingName dr ++ ".tentative")
       Sealed x <- readPending r
       setTentativePending r uw x
       when (uw == NoUpdateWorking) $ removeFileMayNotExist $ pendingName dr
       decideHashedOrNormal rf $ HvsO { hashed = HashedRepo.revertTentativeChanges,
                                        old = Old.revertTentativeChanges }

patchSetToPatches :: RepoPatch p => PatchSet p wX wY -> FL (Named p) wX wY
patchSetToPatches patchSet = mapFL_FL hopefully $ newset2FL patchSet

getUMask :: UMask -> Maybe String
getUMask (YesUMask s) = Just s
getUMask NoUMask = Nothing

withUMaskFlag :: UMask -> IO a -> IO a
withUMaskFlag = maybe id withUMask . getUMask

-- | withGutsOf makes darcs more interrumptable with hashed repositories
withGutsOf :: Repository p wR wU wT -> IO a -> IO a
withGutsOf (Repo _ rf _) | formatHas HashedInventory rf = id
                           | otherwise = withSignalsBlocked

data RepoJob a
    -- = RepoJob (forall p wR wU . RepoPatch p => Repository p wR wU wR -> IO a)
    -- TODO: Unbind Tree from RepoJob, possibly renaming existing RepoJob
    = RepoJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
               => Repository p wR wU wR -> IO a)
    | V1Job (forall wR wU . Repository (Patch Prim) wR wU wR -> IO a)
    | V2Job (forall wR wU . Repository (RealPatch Prim) wR wU wR -> IO a)

onRepoJob :: RepoJob a
          -> (forall p wR wU . RepoPatch p => (Repository p wR wU wR -> IO a) -> (Repository p wR wU wR -> IO a))
          -> RepoJob a
onRepoJob (RepoJob job) f = RepoJob (f job)
-- onRepoJob (TreeJob job) f = TreeJob (f job)
onRepoJob (V1Job job) f = V1Job (f job)
onRepoJob (V2Job job) f = V2Job (f job)

-- | apply a given RepoJob to a repository in the current working directory
withRepository :: UseCache -> RepoJob a -> IO a
withRepository useCache = withRepositoryDirectory useCache "."

-- | apply a given RepoJob to a repository in a given url
withRepositoryDirectory :: UseCache -> String -> RepoJob a -> IO a
withRepositoryDirectory useCache url repojob = do
    Repo dir rf (DarcsRepository t c) <- identifyDarcsRepository useCache url
    if formatHas Darcs2 rf
      then do
         debugMessage $ "Identified darcs-2 repo: " ++ dir
         let therepo = Repo dir rf (DarcsRepository t c)
                           :: Repository (RealPatch Prim) wR wU wR
         case repojob of
           RepoJob job -> job therepo
           -- TreeJob job -> job therepo
           V2Job job -> job therepo
           V1Job _ -> fail $    "This repository contains darcs v1 patches,"
                             ++ " but the command requires darcs v2 patches."
      else do
         debugMessage $ "Identified darcs-1 repo: " ++ dir
         let therepo = Repo dir rf (DarcsRepository t c)
                           :: Repository (Patch Prim) wR wU wR
         case repojob of
           RepoJob job -> job therepo
           -- TreeJob job -> job therepo
           V1Job job -> job therepo
           V2Job _ -> fail $    "This repository contains darcs v2 patches,"
                             ++ " but the command requires darcs v1 patches."

-- | apply a given RepoJob to a repository in the current working directory,
--   taking a lock
withRepoLock :: DryRun -> UseCache -> UpdateWorking -> UMask -> RepoJob a -> IO a
withRepoLock dry useCache uw um repojob =
    withRepository useCache $ onRepoJob repojob $ \job repository@(Repo _ rf _) ->
    do maybe (return ()) fail $ writeProblem rf
       let name = "./"++darcsdir++"/lock"
       withUMaskFlag um $
         if dry == YesDryRun
           then job repository
           else withLock name (revertRepositoryChanges repository dry uw >> job repository)

-- | apply a given RepoJob to a repository in the current working directory,
--   taking a lock only if the repository is old-fashioned
withRepoReadLock :: DryRun -> UseCache -> UMask -> RepoJob a -> IO a
withRepoReadLock dry useCache um repojob =
    withRepository useCache $ onRepoJob repojob $ \job repository@(Repo _ rf _) ->
    do maybe (return ()) fail $ writeProblem rf
       let name = "./"++darcsdir++"/lock"
       withUMaskFlag um $ if formatHas HashedInventory rf || dry == YesDryRun
           then job repository
           else withLock name (revertRepositoryChanges repository dry YesUpdateWorking >> job repository)

removeFromUnrevertContext :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> FL (PatchInfoAnd p) wX wT
                          -> IO ()
removeFromUnrevertContext repository ps = do
  Sealed bundle <- unrevert_patch_bundle `catchall` (return $ seal (PatchSet NilRL NilRL))
  remove_from_unrevert_context_ bundle
  where unrevert_impossible =
            do confirmed <- promptYorn "This operation will make unrevert impossible!\nProceed?"
               if confirmed then removeFileMayNotExist (unrevertUrl repository)
                            else fail "Cancelled."
        unrevert_patch_bundle :: IO (SealedPatchSet p Origin)
        unrevert_patch_bundle = do pf <- B.readFile (unrevertUrl repository)
                                   case scanBundle pf of
                                     Right foo -> return foo
                                     Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
        remove_from_unrevert_context_ :: PatchSet p Origin wZ -> IO ()
        remove_from_unrevert_context_ (PatchSet NilRL NilRL) = return ()
        remove_from_unrevert_context_ bundle =
         do debugMessage "Adjusting the context of the unrevert changes..."
            debugMessage $ "Removing "++ show (lengthFL ps) ++
                                  " patches in removeFromUnrevertContext!"
            ref <- readTentativeRepo repository
            let withSinglet :: Sealed (FL ppp wXxx)
                            -> (forall wYyy . ppp wXxx wYyy -> IO ()) -> IO ()
                withSinglet (Sealed (x :>: NilFL)) j = j x
                withSinglet _ _ = return ()
            withSinglet (mergeThem ref bundle) $ \h_us ->
                  case commuteRL (reverseFL ps :> h_us) of
                    Nothing -> unrevert_impossible
                    Just (us' :> _) ->
                      case removeFromPatchSet ps ref of
                      Nothing -> unrevert_impossible
                      Just common ->
                          do debugMessage "Have now found the new context..."
                             bundle' <- makeBundleN Nothing common (hopefully us':>:NilFL)
                             writeDocBinFile (unrevertUrl repository) bundle'
            debugMessage "Done adjusting the context of the unrevert changes!"

cleanRepository :: RepoPatch p => Repository p wR wU wT -> IO ()
cleanRepository repository@(Repo _ rf _) =
    decideHashedOrNormal rf $
    HvsO { hashed = HashedRepo.cleanPristine repository,
           old = fail Old.oldRepoFailMsg}

createPristineDirectoryTree :: RepoPatch p => Repository p wR wU wT -> Compression -> FilePath -> IO ()
createPristineDirectoryTree (Repo r rf (DarcsRepository _ c)) compr reldir
    | formatHas HashedInventory rf =
        do createDirectoryIfMissing True reldir
           withCurrentDirectory reldir $ HashedRepo.copyPristine c compr r (darcsdir++"/hashed_inventory")
    | otherwise = fail Old.oldRepoFailMsg

-- fp below really should be FileName
-- | Used by the commands dist and diff
createPartialsPristineDirectoryTree :: (FilePathLike fp, RepoPatch p)
                                    => Repository p wR wU wT
                                    -> Compression
                                    -> [fp]
                                    -> FilePath
                                    -> IO ()
createPartialsPristineDirectoryTree (Repo r rf (DarcsRepository _ c)) compr prefs dir
    | formatHas HashedInventory rf =
        do createDirectoryIfMissing True dir
           withCurrentDirectory dir $
               HashedRepo.copyPartialsPristine c compr r (darcsdir++"/hashed_inventory") prefs
    | otherwise = fail Old.oldRepoFailMsg

withRecorded :: RepoPatch p
             => Repository p wR wU wT
             -> Compression
             -> ((AbsolutePath -> IO a) -> IO a)
             -> (AbsolutePath -> IO a)
             -> IO a
withRecorded repository compr mk_dir f
    = mk_dir $ \d -> do createPristineDirectoryTree repository compr (toFilePath d)
                        f d

withTentative :: forall p a wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT
              -> Compression
              -> ((AbsolutePath -> IO a) -> IO a)
              -> (AbsolutePath -> IO a)
              -> IO a
withTentative (Repo dir rf (DarcsRepository _ c)) compr mk_dir f
    | formatHas HashedInventory rf =
        mk_dir $ \d -> do HashedRepo.copyPristine
                              c
                              compr
                              dir
                              (darcsdir++"/tentative_pristine")
                          f d
withTentative repository@(Repo dir _ _) compr mk_dir f =
    withRecorded repository compr mk_dir $ \d ->
    do Sealed ps <- read_patches (dir ++ "/"++darcsdir++"/tentative_pristine")
       apply ps
       f d
    where read_patches :: FilePath -> IO (Sealed (FL p wX))
          read_patches fil = do ps <- B.readFile fil
                                return $ maybe (seal NilFL) id $ readPatch ps

-- | Sets scripts in or below the current directory executable.
--   A script is any file that starts with the bytes '#!'.
--   This is used for --set-scripts-executable.
setScriptsExecutable_ :: PatchInspect p => Maybe (p wX wY) -> IO ()
setScriptsExecutable_ pw = do
    debugMessage "Making scripts executable"
    tree <- readWorking
    paths <- case pw of
          Just ps -> filterM doesFileExist $ listTouchedFiles ps
          Nothing -> return [ anchorPath "." p | (p, Tree.File _) <- Tree.list tree ]
    let setExecutableIfScript f =
              do contents <- B.readFile f
                 when (BC.pack "#!" `B.isPrefixOf` contents) $ do
                   debugMessage ("Making executable: " ++ f)
                   setExecutable f True
    mapM_ setExecutableIfScript paths

setScriptsExecutable :: IO ()
setScriptsExecutable = setScriptsExecutable_ (Nothing :: Maybe (FL DummyPatch wX wY))

setScriptsExecutablePatches :: PatchInspect p => p wX wY -> IO ()
setScriptsExecutablePatches = setScriptsExecutable_ . Just


-- | Writes out a fresh copy of the inventory that minimizes the
-- amount of inventory that need be downloaded when people pull from
-- the repository.
--
-- Specifically, it breaks up the inventory on the most recent tag.
-- This speeds up most commands when run remotely, both because a
-- smaller file needs to be transfered (only the most recent
-- inventory).  It also gives a guarantee that all the patches prior
-- to a given tag are included in that tag, so less commutation and
-- history traversal is needed.  This latter issue can become very
-- important in large repositories.
reorderInventory :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wR
                 -> Compression
                 -> UpdateWorking
                 -> Verbosity
                 -> IO ()
reorderInventory repository@(Repo _ rf _) compr uw verb = do
    decideHashedOrNormal rf $ HvsO {
    hashed = do
        debugMessage "Reordering the inventory."
        PatchSet ps _ <- chooseOrder `fmap` readRepo repository
        tentativelyReplacePatches repository compr uw verb $ reverseRL ps
        HashedRepo.finalizeTentativeChanges repository compr
        debugMessage "Done reordering the inventory.",
    old = fail Old.oldRepoFailMsg }

chooseOrder :: forall p wS wX . RepoPatch p
            => PatchSet p wS wX
            -> PatchSet p wS wX
chooseOrder ps = case filter isTag $ mapRL info $ newset2RL ps of
                  [] -> ps
                  (lt:_) -> case splitOnTag lt ps of
                            Just (PatchSet xs ts :> r) ->
                                PatchSet (r+<+xs) ts
                            _ -> impossible


