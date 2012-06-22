--  Copyright (C) 2002-2003 David Roundy
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


module Darcs.Repository.Prefs ( addToPreflist, getPreflist, setPreflist,
                   getGlobal, environmentHelpHome,
                   defaultrepo, setDefaultrepo,
                   getPrefval, setPrefval, changePrefval,
                   defPrefval,
                   writeDefaultPrefs,
                   boringRegexps, boringFileFilter, darcsdirFilter,
                   FileType(..), filetypeFunction,
                   getCaches,
                   binariesFileHelp, boringFileHelp,
                   globalCacheDir,
                   globalPrefsDirDoc,
                 ) where

import System.IO ( stderr )
import System.IO.Error ( isDoesNotExistError )
import Control.Monad ( unless, when, liftM )
import qualified Control.Exception as C
import Text.Regex ( Regex, mkRegex, matchRegex )
import Data.Char ( toUpper )
import Data.Maybe ( isJust, fromMaybe, mapMaybe, catMaybes )
import Data.List ( nub, isPrefixOf, union, sortBy )
import System.Directory ( getAppUserDataDirectory, doesDirectoryExist, createDirectory, doesFileExist )
import Darcs.Repository.Lock( readBinFile, writeBinFile )
import Darcs.Repository.Flags( UseCache (..), DryRun (..), SetDefault (..), RemoteRepos (..) )
import System.FilePath.Posix ( (</>) )
import System.Environment ( getEnvironment )

import Darcs.Path ( AbsolutePath, ioAbsolute, toFilePath, getCurrentDirectory )
import Darcs.Utils ( catchall )
import Darcs.Repository.External
    ( gzFetchFilePS
    , Cachable( Cachable )
    )
import qualified Data.ByteString.Char8 as BC ( unpack )
import qualified Data.ByteString       as B  ( empty )
import Darcs.Global ( darcsdir )
import Darcs.Repository.Cache ( Cache(..), CacheType(..), CacheLoc(..),
                                WritableOrNot(..), compareByLocality )
import Darcs.URL ( isFile )
import Printer( hPutDocLn, text )


writeDefaultPrefs :: IO ()
writeDefaultPrefs =   do setPreflist "boring" defaultBoring
                         setPreflist "binaries" defaultBinaries
                         setPreflist "motd" []

{-# NOINLINE defaultBoring #-}
defaultBoring :: [String]
defaultBoring = help ++
                [ "",
                  "### compiler and interpreter intermediate files",
                  "# haskell (ghc) interfaces",
                  "\\.hi$", "\\.hi-boot$", "\\.o-boot$",
                  "# object files",
                  "\\.o$","\\.o\\.cmd$",
                  "# profiling haskell",
                  "\\.p_hi$", "\\.p_o$",
                  "# haskell program coverage resp. profiling info",
                  "\\.tix$", "\\.prof$",
                  "# fortran module files",
                  "\\.mod$",
                  "# linux kernel",
                  "\\.ko\\.cmd$","\\.mod\\.c$",
                  "(^|/)\\.tmp_versions($|/)",
                  "# *.ko files aren't boring by default because they might",
                  "# be Korean translations rather than kernel modules",
                  "# \\.ko$",
                  "# python, emacs, java byte code",
                  "\\.py[co]$", "\\.elc$","\\.class$",
                  "# objects and libraries; lo and la are libtool things",
                  "\\.(obj|a|exe|so|lo|la)$",
                  "# compiled zsh configuration files",
                  "\\.zwc$",
                  "# Common LISP output files for CLISP and CMUCL",
                  "\\.(fas|fasl|sparcf|x86f)$",
                  "",
                  "### build and packaging systems",
                  "# cabal intermediates",
                  "\\.installed-pkg-config",
                  "\\.setup-config",
                  "# standard cabal build dir, might not be boring for everybody",
                  "# ^dist(/|$)",
                  "# autotools",
                  "(^|/)autom4te\\.cache($|/)", "(^|/)config\\.(log|status)$",
                  "# microsoft web expression, visual studio metadata directories",
                  "\\_vti_cnf$",
                  "\\_vti_pvt$",
                  "# gentoo tools",
                  "\\.revdep-rebuild.*",
                  "# generated dependencies",
                  "^\\.depend$",
                  "",
                  "### version control systems",
                  "# cvs",
                  "(^|/)CVS($|/)","\\.cvsignore$",
                  "# cvs, emacs locks",
                  "^\\.#",
                  "# rcs",
                  "(^|/)RCS($|/)", ",v$",
                  "# subversion",
                  "(^|/)\\.svn($|/)",
                  "# mercurial",
                  "(^|/)\\.hg($|/)",
                  "# git",
                  "(^|/)\\.git($|/)",
                  "# bzr",
                  "\\.bzr$",
                  "# sccs",
                  "(^|/)SCCS($|/)",
                  "# darcs",
                  "(^|/)"++darcsdir++"($|/)", "(^|/)\\.darcsrepo($|/)",
                  "^\\.darcs-temp-mail$",
                  "-darcs-backup[[:digit:]]+$",
                  "# gnu arch",
                  "(^|/)(\\+|,)",
                  "(^|/)vssver\\.scc$",
                  "\\.swp$","(^|/)MT($|/)",
                  "(^|/)\\{arch\\}($|/)","(^|/).arch-ids($|/)",
                  "# bitkeeper",
                  "(^|/)BitKeeper($|/)","(^|/)ChangeSet($|/)",
                  "",
                  "### miscellaneous",
                  "# backup files",
                  "~$","\\.bak$","\\.BAK$",
                  "# patch originals and rejects",
                  "\\.orig$", "\\.rej$",
                  "# X server",
                  "\\..serverauth.*",
                  "# image spam",
                  "\\#", "(^|/)Thumbs\\.db$",
                  "# vi, emacs tags",
                  "(^|/)(tags|TAGS)$",
                  "#(^|/)\\.[^/]",
                  "# core dumps",
                  "(^|/|\\.)core$",
                  "# partial broken files (KIO copy operations)",
                  "\\.part$",
                  "# waf files, see http://code.google.com/p/waf/",
                  "(^|/)\\.waf-[[:digit:].]+-[[:digit:]]+($|/)",
                  "(^|/)\\.lock-wscript$",
                  "# mac os finder",
                  "(^|/)\\.DS_Store$" ]
 where
  help = map ("# "++) boringFileHelp

boringFileHelp :: [String]
boringFileHelp =
 [ "This file contains a list of extended regular expressions, one per"
 , "line. A file path matching any of these expressions will be filtered"
 , "out during `darcs add', or when the `--look-for-adds' flag is passed"
 , "to `darcs whatsnew' and `record'.  The entries in " ++ globalPrefsDirDoc ++ "boring (if"
 , "it exists) supplement those in this file."
 , ""
 , "Blank lines, and lines beginning with an octothorpe (#) are ignored."
 , "See regex(7) for a description of extended regular expressions."
 ]

darcsdirFilter :: [FilePath] -> [FilePath]
darcsdirFilter = filter (not.isDarcsdir)
isDarcsdir :: FilePath -> Bool
isDarcsdir ('.':'/':f) = isDarcsdir f
isDarcsdir "." = True
isDarcsdir "" = True
isDarcsdir ".." = True
isDarcsdir "../" = True
isDarcsdir "_darcs" = True
isDarcsdir fp = "_darcs/" `isPrefixOf` fp

-- | The path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows.
globalPrefsDir :: IO (Maybe FilePath)
globalPrefsDir = do
  env <- getEnvironment
  case lookup "DARCS_TESTING_PREFS_DIR" env of
    Just d -> return (Just d)
    Nothing -> fmap Just (getAppUserDataDirectory "darcs")
               `catchall` (return Nothing)

-- | The relative path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows. This is used for online documentation.
globalPrefsDirDoc :: String
globalPrefsDirDoc =
#ifndef WIN32
  "~/.darcs/"
#else
  "%APPDATA%\\darcs\\"
#endif

environmentHelpHome :: ([String], [String])
environmentHelpHome = (["HOME", "APPDATA"], [
 "Per-user preferences are set in $HOME/.darcs (on Unix) or",
 "%APPDATA%/darcs (on Windows).  This is also the default location of",
 "the cache."])

getGlobal :: String -> IO [String]
getGlobal f = do
  dir <- globalPrefsDir
  case dir of
    (Just d) -> getPreffile $ d </> f
    Nothing -> return []

globalCacheDir :: IO (Maybe FilePath)
globalCacheDir = slash_cache `fmap` globalPrefsDir
  where slash_cache = fmap (</> "cache")

-- |tryMakeBoringRegexp attempts to create a Regex from a given String. The
-- evaluation is forced, to ensure any malformed exceptions are thrown here,
-- and not later.
tryMakeBoringRegexp :: String -> IO (Maybe Regex)
tryMakeBoringRegexp input = regex `C.catch` handleBadRegex where
    regex = C.evaluate (Just $! mkRegex input)
    handleBadRegex :: C.SomeException -> IO (Maybe Regex)
    handleBadRegex = \_ -> hPutDocLn stderr warning >> return Nothing
    warning = text $ "Warning: Ignored invalid boring regex: " ++ input

-- |boringRegexps returns a list of the boring regexps, from the local and
-- global prefs/boring files. Any invalid regexps are filtered, preventing an
-- exception in (potentially) pure code, when the regexps are used.
boringRegexps :: IO [Regex]
boringRegexps = do
    borefile <- defPrefval "boringfile" (darcsdir ++ "/prefs/boring")
    localBores <- getPrefLines borefile `catchall` return []
    globalBores <- getGlobal "boring"
    liftM catMaybes $ mapM tryMakeBoringRegexp $ localBores ++ globalBores

boringFileFilter :: IO ([FilePath] -> [FilePath])
boringFileFilter = fmap actualBoringFileFilter boringRegexps

noncomments :: [String] -> [String]
noncomments ss = filter is_ok ss
                 where is_ok "" = False
                       is_ok ('#':_) = False
                       is_ok _ = True

getPrefLines :: FilePath -> IO [String]
getPrefLines f = (notconflicts . noncomments . map stripCr . lines)
              `fmap` readBinFile f
    where notconflicts = filter nc
          startswith [] _ = True
          startswith (x:xs) (y:ys) | x == y = startswith xs ys
          startswith _ _ = False
          nc l | startswith "v v v v v v v" l = False
          nc l | startswith "*************" l = False
          nc l | startswith "^ ^ ^ ^ ^ ^ ^" l = False
          nc _ = True
          stripCr ""     = ""
          stripCr "\r"   = ""
          stripCr (c:cs) = c : stripCr cs

-- | From a list of paths, filter out any that are within @_darcs@ or
-- match a boring regexp.
actualBoringFileFilter :: [Regex] -> [FilePath] -> [FilePath]
actualBoringFileFilter regexps files = filter (not . boring . normalize) files
    where boring file = isDarcsdir file ||
                        any (\regexp -> isJust $ matchRegex regexp file) regexps

normalize :: FilePath -> FilePath
normalize ('.':'/':f) = normalize f
normalize f = normalize_helper $ reverse f
              where
              normalize_helper ('/':rf) = normalize_helper rf
              normalize_helper rf = reverse rf

data FileType = BinaryFile | TextFile
                deriving (Eq)

{-# NOINLINE defaultBinaries #-}
-- | The lines that will be inserted into @_darcs/prefs/binaries@ when
-- @darcs init@ is run.  Hence, a list of comments, blank lines and
-- regular expressions (ERE dialect).
--
-- Note that while this matches .gz and .GZ, it will not match .gZ,
-- i.e. it is not truly case insensitive.
defaultBinaries :: [String]
defaultBinaries = help ++
                   ["\\.(" ++ e ++ "|" ++ map toUpper e ++ ")$" | e <- extensions ]
    where extensions = ["a","bmp","bz2","doc","elc","exe","gif","gz","iso",
                        "jar","jpe?g","mng","mpe?g","p[nbgp]m","pdf","png",
                        "pyc","so","tar","tgz","tiff?","z","zip"]
          help = map ("# "++) binariesFileHelp

binariesFileHelp :: [String]
binariesFileHelp =
  ["This file contains a list of extended regular expressions, one per",
   "line.  A file path matching any of these expressions is assumed to",
   "contain binary data (not text).  The entries in " ++ globalPrefsDirDoc ++ "binaries (if",
   "it exists) supplement those in this file.",
   "",
   "Blank lines, and lines beginning with an octothorpe (#) are ignored.",
   "See regex(7) for a description of extended regular expressions."]

filetypeFunction :: IO (FilePath -> FileType)
filetypeFunction = do
    binsfile <- defPrefval "binariesfile" (darcsdir ++ "/prefs/binaries")
    bins <- getPrefLines binsfile `catch`
             (\e-> if isDoesNotExistError e then return [] else ioError e)
    gbs <- getGlobal "binaries"
    let regexes = map mkRegex (bins ++ gbs)
    let isbin f = any (\r -> isJust $ matchRegex r f) regexes
        ftf f = if isbin $ normalize f then BinaryFile else TextFile
        in
        return ftf

-- this avoids a circular dependency with Repository
prefsDirectory :: IO (Maybe String)
prefsDirectory =
    do darcs <- doesDirectoryExist darcsdir
       return $ if darcs
          then Just $ darcsdir ++ "/prefs/"
          else Nothing

withPrefsDirectory :: (String -> IO ()) -> IO ()
withPrefsDirectory j = prefsDirectory >>= maybe (return ()) j

addToPreflist :: String -> String -> IO ()
addToPreflist p s = withPrefsDirectory $ \prefs -> do
  hasprefs <- doesDirectoryExist prefs
  unless hasprefs $ createDirectory prefs
  pl <- getPreflist p
  writeBinFile (prefs ++ p) $ unlines $ union [s] pl

getPreflist :: String -> IO [String]
getPreflist p =  prefsDirectory >>= maybe (return []) (\prefs -> getPreffile $ prefs ++ p)

getPreffile :: FilePath -> IO [String]
getPreffile f = do
  hasprefs <- doesFileExist f
  if hasprefs
    then getPrefLines f
    else return []

setPreflist :: String -> [String] -> IO ()
setPreflist p ls = withPrefsDirectory $ \prefs -> do
  haspref <- doesDirectoryExist prefs
  when haspref $ writeBinFile (prefs ++ p) (unlines ls)

defPrefval :: String -> String -> IO String
defPrefval p d = fromMaybe d `fmap` getPrefval p

getPrefval :: String -> IO (Maybe String)
getPrefval p =
    do pl <- getPreflist "prefs"
       return $
        case map snd $ filter ((==p).fst) $ map (break (==' ')) pl of
           [val] ->
                case words val of
                    [] -> Nothing
                    _ -> Just $ tail val
           _ -> Nothing

setPrefval :: String -> String -> IO ()
setPrefval p v =  do pl <- getPreflist "prefs"
                     setPreflist "prefs" $
                       filter ((/=p) . fst . break (==' ')) pl ++ [p++" "++v]

changePrefval :: String -> String -> String -> IO ()
changePrefval p f t =
    do pl <- getPreflist "prefs"
       ov <- getPrefval p
       let newval = case ov of
                        Nothing -> t
                        Just old -> if old == f then t else old
       setPreflist "prefs" $
                    filter ((/=p) . fst . break(==' ')) pl ++ [p++" "++newval]

defaultrepo :: RemoteRepos -> AbsolutePath -> [String] -> IO [String]
defaultrepo (RemoteRepos rrepos) _ [] =
  do let fixR r | isFile r = toFilePath `fmap` ioAbsolute r
                | otherwise = return r
     case rrepos of
       [] -> do defrepo <- getPreflist "defaultrepo"
                case defrepo of
                  [r] -> (:[]) `fmap` fixR r
                  _ -> return []
       rs -> mapM fixR rs
defaultrepo _ _ r = return r

setDefaultrepo :: String -> DryRun -> RemoteRepos -> SetDefault -> IO ()
setDefaultrepo r dryRun (RemoteRepos rrepos) setDefault
                      =  do olddef <- getPreflist "defaultrepo"
                            let doit = null noSetDefault && greenLight
                                greenLight = wetRun
                                           && not rIsTmp
                                           && (olddef /= [r] || olddef == [])
                            if doit
                               then setPreflist "defaultrepo" [r]
                               else when (True `notElem` noSetDefault && greenLight) $ putStr . unlines $
                                      -- the nuance here is that we should only notify when the
                                      -- reason we're not setting default is the --no-set-default
                                      -- flag, not the various automatic show stoppers
                                      [ "HINT: if you want to change the default remote repository to"
                                      , "      " ++ r ++ ","
                                      , "      quit now and issue the same command with the --set-default flag."
                                      ]
                            addToPreflist "repos" r
                         `catchall` return () -- we don't care if this fails!
 where
  wetRun = dryRun == NoDryRun
  rIsTmp = r `elem` rrepos
  noSetDefault = case setDefault of NoSetDefault x -> [x] ; _ -> []

getCaches :: UseCache -> String -> IO Cache
getCaches useCache repodir =
    do here <- parsehs `fmap` getPreffile (darcsdir ++ "/prefs/sources")
       there <- (parsehs . lines . BC.unpack) `fmap`
                (gzFetchFilePS (repodir </> darcsdir </> "prefs/sources") Cachable
                 `catchall` return B.empty)
       globalcachedir <- globalCacheDir
       let globalcache = case (nocache,globalcachedir) of
                           (True,_) -> []
                           (_,Just d) -> [Cache Directory Writable d]
                           _ -> []
       globalsources <- parsehs `fmap` getGlobal "sources"
       thisdir <- getCurrentDirectory
       let thisrepo = [Cache Repo Writable $ toFilePath thisdir]
       let tempCache = nub $ thisrepo ++ globalcache ++ globalsources ++
                  here ++ [Cache Repo NotWritable repodir] ++ filterExternalSources there
       return $ Ca $ sortBy compareByLocality tempCache
      where
            parsehs = mapMaybe readln . noncomments
            readln l | "repo:"     `isPrefixOf` l = Just (Cache Repo NotWritable (drop 5 l))
                     | nocache = Nothing
                     | "cache:"    `isPrefixOf` l = Just (Cache Directory Writable    (drop 6 l))
                     | "readonly:" `isPrefixOf` l = Just (Cache Directory NotWritable (drop 9 l))
                     | otherwise = Nothing
            nocache = useCache == NoUseCache
            filterExternalSources there = if isFile repodir
                                          then
                                            there
                                          else
                                            filter (not . isFile . cacheSource) there
