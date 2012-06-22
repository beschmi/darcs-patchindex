--  Copyright (C) 2002-2004 David Roundy
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
{-# LANGUAGE CPP, TypeOperators #-}

module Darcs.UI.Commands.Send ( send ) where

import System.Exit ( exitWith, ExitCode( ExitSuccess ) )
#ifndef HAVE_MAPI
import System.Exit ( ExitCode( ExitFailure ) )
#endif
import System.IO.Error ( ioeGetErrorString )
import System.IO ( hClose )
import Control.Monad ( when, unless, forM_ )
import Storage.Hashed.Tree ( Tree )
import Data.List ( intercalate, isPrefixOf, stripPrefix )
import Data.Maybe ( isNothing, fromMaybe )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , putInfo
    , putVerbose
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , formatPath
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Arguments
    ( DarcsFlag( EditDescription
               , LogFile
               , Target
               , Context
               , DryRun 
               , Quiet
               , AllowUnrelatedRepos
               )
    , fixUrl
    , getCc
    , getAuthor
    , workingRepoDir
    , editDescription
    , logfile
    , rmlogfile
    , sign
    , getSubject
    , depsSel
    , getInReplyTo
    , matchSeveral
    , outputAutoName
    , output
    , ccSend
    , subject
    , target
    , author
    , sendmailCmd
    , inReplyTo
    , remoteRepo
    , networkOptions
    , allInteractive
    , getSendmailCmd
    , summary
    , allowUnrelatedRepos
    , fromOpt
    , sendToContext
    , getOutput
    , changesReverse
    , charset
    , getCharset
    )
import qualified Darcs.UI.Arguments as A ( dryRun, setDefault )

import Darcs.UI.Flags ( willRemoveLogFile, doReverse, dryRun, useCache, umask, remoteRepos, setDefault )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, patchDesc )
import Darcs.Repository ( PatchSet, Repository,
                          identifyRepositoryFor, withRepoReadLock, RepoJob(..),
                          readRepo, readRecorded, prefsUrl, checkUnrelatedRepos )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch ( RepoPatch, description, applyToTree, invert )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), (:\/:)(..), (:>)(..),
                       mapFL, mapFL_FL, lengthFL, nullFL )
import Darcs.Patch.Bundle ( makeBundleN, scanContext, patchFilename )
import Darcs.Repository.Prefs ( setDefaultrepo, getPreflist )
import Darcs.Repository.External ( fetchFilePS, Cachable(..) )
import Darcs.UI.External
    ( signString
    , sendEmailDoc
    , generateEmail
    , editFile
    , catchall
    , getSystemEncoding
    , isUTF8Locale
#ifndef HAVE_MAPI
    , haveSendmail
#endif
    )
import ByteStringUtils ( mmapFilePS, isAscii )
import qualified Data.ByteString.Char8 as BC (unpack)
import Darcs.Repository.Lock
    ( withOpenTemp
    , writeDocBinFile
    , readDocBinFile
    , worldReadableTemp
    , removeFileMayNotExist
    )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , runSelection
    )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Utils ( askUser, promptYorn )
import Data.Text.Encoding       ( decodeUtf8' )
import Progress ( debugMessage )
import Darcs.UI.Email ( makeEmail )
import Printer ( Doc, vsep, vcat, text, ($$), (<+>), (<>), putDoc, renderPS )
import Darcs.Path ( FilePathLike, toFilePath, AbsolutePath, AbsolutePathOrStd,
                        getCurrentDirectory, useAbsoluteOrStd )
import URL.HTTP ( postUrl )
#include "impossible.h"


sendDescription :: String
sendDescription =
 "Send by email a bundle of one or more patches."

sendHelp :: String
sendHelp =
 "Send is used to prepare a bundle of patches that can be applied to a target\n"++
 "repository.  Send accepts the URL of the repository as an argument.  When\n"++
 "called without an argument, send will use the most recent repository that\n"++
 "was either pushed to, pulled from or sent to.  By default, the patch bundle\n"++
 "is sent by email, although you may save it to a file.\n"

send :: DarcsCommand
send = DarcsCommand {commandProgramName = "darcs",
                     commandName = "send",
                     commandHelp = sendHelp,
                     commandDescription = sendDescription,
                     commandExtraArgs = 1,
                     commandExtraArgHelp = ["[REPOSITORY]"],
                     commandCommand = sendCmd,
                     commandPrereq = amInHashedRepository,
                     commandGetArgPossibilities = getPreflist "repos",
                     commandArgdefaults = defaultRepo,
                     commandAdvancedOptions = [logfile, rmlogfile,
                                                 remoteRepo,
                                                 sendToContext, changesReverse] ++
                                                networkOptions,
                     commandBasicOptions = [matchSeveral, depsSel,
                                              allInteractive,
                                              fromOpt, author,
                                              target,ccSend,subject, inReplyTo, charset,
                                              output,outputAutoName,sign]
                                              ++A.dryRun++[summary,
                                              editDescription,
                                              A.setDefault False,
                                              workingRepoDir,
                                              sendmailCmd,
                                              allowUnrelatedRepos]}

sendCmd :: [DarcsFlag] -> [String] -> IO ()
sendCmd input_opts [""] = sendCmd input_opts []
sendCmd input_opts [unfixedrepodir] = withRepoReadLock (dryRun input_opts) (useCache input_opts) (umask input_opts) $ RepoJob $
  \(repository :: Repository p wR wU wR) -> do
  context_ps <- the_context input_opts
  case context_ps of
    Just them -> do
        wtds <- decideOnBehavior input_opts (Nothing :: Maybe (Repository p wR wU wR))
        sendToThem repository input_opts wtds "CONTEXT" them
    Nothing -> do
        repodir <- fixUrl input_opts unfixedrepodir
        -- Test to make sure we aren't trying to push to the current repo
        here <- getCurrentDirectory
        when (repodir == toFilePath here) $
           fail "Can't send to current repository! Did you mean send --context?"
        old_default <- getPreflist "defaultrepo"
        when (old_default == [repodir] && Quiet `notElem` input_opts) $
             putStrLn $ "Creating patch to "++formatPath repodir++"..."
        repo <- identifyRepositoryFor repository (useCache input_opts) repodir
        them <- readRepo repo
        setDefaultrepo repodir (dryRun input_opts) (remoteRepos input_opts) (setDefault input_opts)
        wtds <- decideOnBehavior input_opts (Just repo)
        sendToThem repository input_opts wtds repodir them
    where the_context [] = return Nothing
          the_context (Context foo:_)
              = (Just . scanContext )`fmap` mmapFilePS (toFilePath foo)
          the_context (_:fs) = the_context fs
sendCmd _ _ = impossible

sendToThem :: (RepoPatch p, ApplyState p ~ Tree)
           => Repository p wR wU wT -> [DarcsFlag] -> [WhatToDo] -> String
           -> PatchSet p Origin wX -> IO ()
sendToThem repo opts wtds their_name them = do
#ifndef HAVE_MAPI
  -- Check if the user has sendmail or provided a --sendmail-cmd
  -- (unless -o/-O or --dry-run is used)
  sendmail <- haveSendmail
  sm_cmd <- getSendmailCmd opts
  when (isNothing (getOutput opts "") && DryRun `notElem` opts &&
        not sendmail && sm_cmd == "") $ do
      putInfo opts $ text "No working sendmail instance on your machine!"
      exitWith $ ExitFailure 1
#endif
  us <- readRepo repo
  common :> us' <- return $ findCommonWithThem us them
  checkUnrelatedRepos (AllowUnrelatedRepos `elem` opts) us them
  (case us' of
      NilFL -> do putInfo opts $ text "No recorded local changes to send!"
                  exitWith ExitSuccess
      _ -> putVerbose opts $ text "We have the following patches to send:"
                     $$ vcat (mapFL description us')) :: IO ()
  pristine <- readRecorded repo
  let context = selectionContext "send" opts Nothing Nothing
      selector = if doReverse opts
                 then selectChanges FirstReversed
                 else selectChanges First
  (to_be_sent :> _) <- runSelection (selector us') context
  printDryRunMessageAndExit "send" opts to_be_sent
  when (nullFL to_be_sent) $ do
      putInfo opts $ text "You don't want to send any patches, and that's fine with me!"
      exitWith ExitSuccess
  setEnvDarcsPatches to_be_sent
  bundle <- prepareBundle opts common pristine (us':\/:to_be_sent)
  let make_fname (tb:>:_) = patchFilename $ patchDesc tb
      make_fname _ = impossible
      fname = make_fname to_be_sent
      outname = getOutput opts fname
  case outname of
    Just fname' -> writeBundleToFile opts to_be_sent bundle fname' wtds their_name
    Nothing -> sendBundle opts to_be_sent bundle fname wtds their_name

prepareBundle :: forall p wX wY wZ. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet p Origin wZ
              -> Tree IO -> (FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) wX wY
              -> IO Doc
prepareBundle opts common pristine (us' :\/: to_be_sent) = do
  pristine' <- applyToTree (invert $ mapFL_FL hopefully us') pristine
  unsig_bundle <- makeBundleN (Just pristine') (unsafeCoerceP common) (mapFL_FL hopefully to_be_sent)
  signString opts unsig_bundle

sendBundle :: forall p wX wY . (RepoPatch p, ApplyState p ~ Tree)
           => [DarcsFlag] -> FL (PatchInfoAnd p) wX wY
             -> Doc -> String -> [WhatToDo] -> String -> IO ()
sendBundle opts to_be_sent bundle fname wtds their_name=
         let
           auto_subject :: forall pp wA wB . FL (PatchInfoAnd pp) wA wB -> String
           auto_subject (p:>:NilFL)  = "darcs patch: " ++ trim (patchDesc p) 57
           auto_subject (p:>:ps) = "darcs patch: " ++ trim (patchDesc p) 43 ++
                            " (and " ++ show (lengthFL ps) ++ " more)"
           auto_subject _ = error "Tried to get a name from empty patch list."
           trim st n = if length st <= n then st
                       else take (n-3) st ++ "..."
           in do
           thetargets <- getTargets wtds
           from <- getAuthor opts
           let thesubject = fromMaybe (auto_subject to_be_sent) $ getSubject opts
           (mailcontents, mailfile, mailcharset) <- getDescription opts their_name to_be_sent

           let warnMailBody = let msg = "Email body left in " in
                              case mailfile of
                                  Just mf -> putStrLn $ msg++mf++"."
                                  Nothing -> return ()

               warnCharset msg = do
                 confirmed <- promptYorn $ "Warning: " ++ msg ++ "  Send anyway?"
                 unless confirmed $ do
                    putStrLn "Aborted.  You can specify charset with the --charset option."
                    warnMailBody
                    exitWith ExitSuccess

           thecharset <- case getCharset opts of
                              -- Always trust provided charset
                              providedCset@(Just _) -> return providedCset
                              Nothing ->
                                case mailcharset of
                                Nothing -> do
                                  warnCharset "darcs could not guess the charset of your mail."
                                  return mailcharset
                                Just "utf-8" -> do
                                  -- Check the locale encoding for consistency
                                  encoding <- getSystemEncoding
                                  debugMessage $ "Current locale encoding: " ++ encoding
                                  unless (isUTF8Locale encoding) $
                                    warnCharset "your mail is valid UTF-8 but your locale differs."
                                  return mailcharset
                                -- Trust other cases (us-ascii)
                                Just _ -> return mailcharset

           let body = makeEmail their_name
                        (maybe [] (\x -> [("In-Reply-To", x), ("References", x)]) . getInReplyTo $ opts)
                        (Just mailcontents)
                        thecharset
                        bundle
                        (Just fname)
               contentAndBundle = Just (mailcontents, bundle)

               sendmail = do
                 sm_cmd <- getSendmailCmd opts
                 let to = generateEmailToString thetargets
                 sendEmailDoc from to thesubject (getCc opts)
                               sm_cmd contentAndBundle body >>
                  (putInfo opts . text $ ("Successfully sent patch bundle to: "
                            ++ to
                            ++ ccs (getCc opts) ++"."))
                 `catch` \e -> do warnMailBody
                                  fail $ ioeGetErrorString e
               ccs [] = []
               ccs cs  = " and cc'ed " ++ cs

           when (null [ p | Post p <- thetargets]) sendmail
           nbody <- withOpenTemp $ \ (fh,fn) -> do
               let to = generateEmailToString thetargets
               generateEmail fh from to thesubject (getCc opts) body
               hClose fh
               mmapFilePS fn
           forM_ [ p | Post p <- thetargets]
             (\url -> do
                putInfo opts . text $ "Posting patch to " ++ url
                postUrl url (BC.unpack nbody) "message/rfc822")
             `catch` const sendmail
           cleanup opts mailfile

generateEmailToString :: [WhatToDo] -> String
generateEmailToString = intercalate " , " . filter (/= "") . map extractEmail
  where
    extractEmail (SendMail t) = t
    extractEmail _ = ""

cleanup :: (FilePathLike t) => [DarcsFlag] -> Maybe t -> IO ()
cleanup opts (Just mailfile) = when (isNothing (getFileopt opts) || willRemoveLogFile opts) $
                                      removeFileMayNotExist mailfile
cleanup _ Nothing = return ()

writeBundleToFile :: forall p wX wY . (RepoPatch p, ApplyState p ~ Tree)
                  => [DarcsFlag] -> FL (PatchInfoAnd p) wX wY -> Doc ->
                    AbsolutePathOrStd -> [WhatToDo] -> String -> IO ()
writeBundleToFile opts to_be_sent bundle fname wtds their_name =
    do (d,f,_) <- getDescription opts their_name to_be_sent
       let putabs a = do writeDocBinFile a (d $$ bundle)
                         putStrLn $ "Wrote patch to " ++ toFilePath a ++ "."
           putstd = putDoc (d $$ bundle)
       useAbsoluteOrStd putabs putstd fname
       let to = generateEmailToString wtds
       unless (null to) $
           putInfo opts . text $ "The usual recipent for this bundle is: " ++ to
       cleanup opts f

data WhatToDo
    = Post String        -- ^ POST the patch via HTTP
    | SendMail String    -- ^ send patch via email

decideOnBehavior :: RepoPatch p => [DarcsFlag] -> Maybe (Repository p wR wU wT) -> IO [WhatToDo]
decideOnBehavior opts remote_repo =
    case the_targets of
    [] -> do wtds <- case remote_repo of
                     Nothing -> return []
                     Just r -> check_post r
             unless (null wtds) $ announce_recipients wtds
             return wtds
    ts -> do announce_recipients ts
             return ts
    where the_targets = collectTargets opts
#ifdef HAVE_HTTP
          -- the ifdef above is to so that darcs only checks the remote
          -- _darcs/post if we have an implementation of postUrl.  See
          -- our HTTP module for more details
          check_post the_remote_repo =
                       do p <- ((readPost . BC.unpack) `fmap`
                                fetchFilePS (prefsUrl the_remote_repo++"/post")
                                (MaxAge 600)) `catchall` return []
                          emails <- who_to_email the_remote_repo
                          return (p++emails)
          readPost = map parseLine . lines where
              parseLine t = maybe (Post t) SendMail $ stripPrefix "mailto:" t
#else
          check_post = who_to_email
#endif
          who_to_email the_remote_repo =
              do email <- (BC.unpack `fmap`
                           fetchFilePS (prefsUrl the_remote_repo++"/email")
                                       (MaxAge 600))
                          `catchall` return ""
                 if '@' `elem` email then return . map SendMail $ lines email
                                     else return []
          announce_recipients emails =
            let pn (SendMail s) = s
                pn (Post p) = p
            in if DryRun `elem` opts
            then putInfo opts . text $ "Patch bundle would be sent to: "++unwords (map pn emails)
            else when (null the_targets && isNothing (getOutput opts "")) $
                 putInfo opts . text $ "Patch bundle will be sent to: "++unwords (map pn emails)

getTargets :: [WhatToDo] -> IO [WhatToDo]
getTargets [] = fmap ((:[]) . SendMail) $ askUser "What is the target email address? "
getTargets wtds = return wtds

collectTargets :: [DarcsFlag] -> [WhatToDo]
collectTargets flags = [ f t | Target t <- flags ] where
    f url | "http:" `isPrefixOf` url = Post url
    f em = SendMail em

getDescription :: (RepoPatch p, ApplyState p ~ Tree)
               => [DarcsFlag] -> String -> FL (PatchInfoAnd p) wX wY -> IO (Doc, Maybe String, Maybe String)
getDescription opts their_name patches =
    case get_filename of
        Just f -> do file <- f
                     when (EditDescription `elem` opts) $ do
                       when (isNothing $ getFileopt opts) $
                            writeDocBinFile file patchdesc
                       debugMessage $ "About to edit file " ++ file
                       (_, changed) <- editFile file
                       unless changed $ do
                         confirmed <- promptYorn "File content did not change. Continue anyway?"
                         unless confirmed $ do putStrLn "Aborted."
                                               exitWith ExitSuccess
                       return ()
                     doc <- readDocBinFile file
                     return (doc, Just file, tryGetCharset doc)
        Nothing -> return (patchdesc, Nothing, tryGetCharset patchdesc)
    where patchdesc = text (if lengthFL patches == 1
                               then "1 patch"
                               else show (lengthFL patches) ++ " patches")
                      <+> text "for repository" <+> text their_name <> text ":"
                      $$ text ""
                      $$ vsep (mapFL description patches)
          get_filename = case getFileopt opts of
                                Just f -> Just $ return $ toFilePath f
                                Nothing -> if EditDescription `elem` opts
                                              then Just tempfile
                                              else Nothing
          tempfile = worldReadableTemp "darcs-temp-mail"
          tryGetCharset content = let body = renderPS content in
                                  if isAscii body
                                  then Just "us-ascii"
                                  else either (const Nothing)
                                              (const $ Just "utf-8")
                                              (decodeUtf8' body)

getFileopt :: [DarcsFlag] -> Maybe AbsolutePath
getFileopt (LogFile f:_) = Just f
getFileopt (_:flags) = getFileopt flags
getFileopt [] = Nothing
