-- Copyright (C) 2005 David Roundy
--
-- This file is licensed under the GPL, version two or later.

{-# LANGUAGE CPP #-}

module Darcs.Repository.Format
    ( RepoFormat(..)
    , RepoProperty(..)
    , identifyRepoFormat
    , tryIdentifyRepoFormat
    , createRepoFormat
    , writeRepoFormat
    , writeProblem
    , readProblem
    , readfromAndWritetoProblem
    , formatHas
    ) where

#include "impossible.h"

import Control.Monad ( mplus, (<=<) )
import qualified Data.ByteString.Char8 as BC ( split, unpack, elemIndex )
import qualified Data.ByteString  as B ( null, empty )
import Data.List ( partition, intercalate )
import Data.Maybe ( isJust, mapMaybe )

import Darcs.Repository.External
    ( fetchFilePS
    , Cachable( Cachable )
    )
import Darcs.Global ( darcsdir )
import Darcs.Repository.Lock ( writeBinFile )
import Darcs.SignalHandler ( catchNonSignal )
import Darcs.Utils ( catchall, prettyException )

import ByteStringUtils ( linesPS )
import Progress ( beginTedious, endTedious, finishedOneIO )

data RepoProperty = Darcs1_0
                  | Darcs2
                  | HashedInventory
                  | NoWorkingDir
                  | UnknownFormat String
                  deriving ( Eq )

-- Define string constants in one place, for reuse in show/parse functions.
darcs1Format, darcs2Format, hashedInventoryFormat, noWorkingDirFormat :: String
darcs1Format = "darcs-1.0"
darcs2Format = "darcs-2"
hashedInventoryFormat = "hashed"
noWorkingDirFormat = "no-working-dir"

instance Show RepoProperty where
    show Darcs1_0 = darcs1Format
    show Darcs2 = darcs2Format
    show HashedInventory = hashedInventoryFormat
    show NoWorkingDir = noWorkingDirFormat
    show (UnknownFormat f) = "Unknown format: " ++ f

readRepoProperty :: String -> RepoProperty
readRepoProperty input
    | input == darcs1Format = Darcs1_0
    | input == darcs2Format = Darcs2
    | input == hashedInventoryFormat = HashedInventory
    | input == noWorkingDirFormat = NoWorkingDir
    | otherwise = UnknownFormat input

-- | @RepoFormat@ is the representation of the format of a repository. Each
-- sublist corresponds to a line in the format file.
newtype RepoFormat = RF [[RepoProperty]]

-- | Is a given property contained within a given format?
formatHas :: RepoProperty -> RepoFormat -> Bool
formatHas f (RF rps) = f `elem` concat rps

instance Show RepoFormat where
    show (RF rf) = unlines $ map (intercalate "|" . map show) rf

-- | @identifyRepoFormat URL@ identifies the format of the repository at the
-- given address. Fails if we weren't able to identify the format.
identifyRepoFormat :: String -> IO RepoFormat
identifyRepoFormat = either fail return <=< tryIdentifyRepoFormat

-- | @tryIdentifyRepoFormat URL@ identifies the format of the repository at the
-- given address. Return @Left reason@ if it fails, where @reason@ explains why
-- we weren't able to identify the format. Note that we do no verification of
-- the format, which is handled by 'readProblem' or 'writeProblem' on the
-- resulting 'RepoFormat'.
tryIdentifyRepoFormat :: String -> IO (Either String RepoFormat)
tryIdentifyRepoFormat repo = do
    let k = "Identifying repository " ++ repo
    beginTedious k
    finishedOneIO k "format"
    formatInfo <- fetchFilePS (repoPath "format") Cachable
                  `catchall` return B.empty
    -- We use a workaround for servers that don't return a 404 on nonexistent
    -- files (we trivially check for something that looks like a HTML/XML tag).
    format <- if B.null formatInfo || isJust (BC.elemIndex '<' formatInfo)
                  then do
                      finishedOneIO k "inventory"
                      missingInvErr <- checkFile (repoPath "inventory")
                      case missingInvErr of
                          Nothing -> return . Right $ RF [[Darcs1_0]]
                          Just e -> return . Left $ makeErrorMsg e
                  else return . Right $ readFormat formatInfo
    endTedious k
    return format
  where
    repoPath fileName = repo ++ "/" ++ darcsdir ++ "/" ++ fileName

    readFormat = RF . map (map (readRepoProperty . BC.unpack)) . splitFormat

    -- split into lines, then split each non-empty line on '|'
    splitFormat = map (BC.split '|') . filter (not . B.null) . linesPS

    checkFile path = (fetchFilePS path Cachable >> return Nothing)
                     `catchNonSignal`
                     (return . Just . prettyException)

    makeErrorMsg e = unlines
        [ "Not a repository: " ++ repo ++ " (" ++ e ++ ")"
        , ""
        , "HINT: Do you have the right URI for the repository?"
        , ""
        , "      If so, check with the repository owner to see if the "
          ++ "following files"
        , "      are readable:"
        , ""
        , "        1. _darcs/format    - might not exist; that's OK"
        , "        2. _darcs/inventory - should exist if #1 is missing"
        , "        3. _darcs/hashed_inventory - should exist if #2 is missing"
        ]

-- | @writeRepoFormat@ writes the repo format to the given file.
writeRepoFormat :: RepoFormat -> FilePath -> IO ()
writeRepoFormat rf loc = writeBinFile loc $ show rf

-- | @createRepoFormat useFormat1 useNoWorkingDir@ create a repo format
createRepoFormat :: Bool -> Bool -> RepoFormat
createRepoFormat useFormat1 useNoWorkingDir = RF $ (HashedInventory : flags2wd) : flags2format
  where
    flags2format = if useFormat1 then [] else [[Darcs2]]
    flags2wd = [NoWorkingDir | useNoWorkingDir ]

-- | @writeProblem form@ tells if we can write to a repo in format @form@,
-- first checking if we can read that repo It returns @Nothing@ if there's no
-- problem writing to such a repository.
writeProblem :: RepoFormat -> Maybe String
writeProblem rf = readProblem rf `mplus` findProblems rf wp
  where
    wp [] = impossible
    wp x = case partition isKnown x of
               (_, []) -> Nothing
               (_, unknowns) -> Just . unwords $
                    "Can't write repository format: " : map show unknowns

-- | @readfromAndWritetoProblem form@ tells if we can read from one and write
-- to another repo format, returning @Nothing@ if there's no problem.
readfromAndWritetoProblem :: RepoFormat -> RepoFormat -> Maybe String
readfromAndWritetoProblem inrf outrf
    | formatHas Darcs2 inrf /= formatHas Darcs2 outrf =
        Just "Cannot mix darcs-2 repositories with older formats"
    | otherwise = readProblem inrf `mplus` writeProblem outrf

-- | @readProblem form@ tells if we can read from a repo in format @form@.
-- It returns @Nothing@ if there's no problem reading from such a repository.
readProblem :: RepoFormat -> Maybe String
readProblem rf
    | formatHas Darcs1_0 rf && formatHas Darcs2 rf =
        Just "Invalid repositoryformat: format 2 is incompatible with format 1"
readProblem rf = findProblems rf rp
  where
    rp x | any isKnown x = Nothing
    rp [] = impossible
    rp x = Just . unwords $ "Can't understand repository format:" : map show x

-- |'findProblems' applies a function that maps format-entries to an optional
-- error message, to each repoformat entry. Returning any errors.
findProblems :: RepoFormat -> ([RepoProperty] -> Maybe String) -> Maybe String
findProblems (RF ks) formatHasProblem = case mapMaybe formatHasProblem ks of
                                            [] -> Nothing
                                            xs -> Just $ unlines xs

-- | Does this version of darcs know how to handle this property?
isKnown :: RepoProperty -> Bool
isKnown p = p `elem` knownProperties
  where
    knownProperties :: [RepoProperty]
    knownProperties = [ Darcs1_0
                      , Darcs2
                      , HashedInventory
                      , NoWorkingDir
                      ]
