{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using Prelude.catch
module Darcs.Repository.External
    ( cloneTree
    , cloneFile
    , fetchFilePS
    , fetchFileLazyPS
    , gzFetchFilePS
    , speculateFileOrUrl
    , copyFileOrUrl
    , Cachable(..)
    , backupByRenaming
    , backupByCopying
    ) where

import System.Posix.Files ( createLink )
import System.Posix.Files
    ( getSymbolicLinkStatus
    , isRegularFile
    , isDirectory
    )
import System.Directory
    ( createDirectory
    , getDirectoryContents
    , doesDirectoryExist
    , doesFileExist
    , renameFile
    , renameDirectory
    , copyFile
    )

import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv )
import System.FilePath.Posix ( (</>), normalise )
import System.IO.Error ( isDoesNotExistError )
import Control.Monad ( when, zipWithM_ )
import Data.Char ( toUpper )

import Exec ( exec, Redirect(..) )
import URL
    ( copyUrl
    , copyUrlFirst
    , waitUrl
    , Cachable(..)
    )

import Darcs.URL
    ( isFile
    , isHttpUrl
    , isSshUrl
    , splitSshUrl
    )
import Darcs.Ssh ( copySSH )
import Darcs.Utils ( catchall, breakCommand )
import Darcs.Repository.Flags ( RemoteDarcs(..) )
import Darcs.Repository.Lock ( withTemp ) 

import ByteStringUtils ( gzReadFilePS )
import qualified Data.ByteString as B (ByteString, readFile )
import qualified Data.ByteString.Lazy as BL

#ifdef HAVE_HTTP
import Network.Browser
    ( browse
    , request
    , setErrHandler
    , setOutHandler
    , setAllowRedirects
    )
import Network.HTTP
    ( RequestMethod(GET)
    , rspCode
    , rspBody
    , rspReason
    , mkRequest
    )
import Network.URI
    ( parseURI
    , uriScheme
    )
#endif

copyFileOrUrl :: RemoteDarcs -> FilePath -> FilePath -> Cachable -> IO ()
copyFileOrUrl _    fou out _     | isFile fou = copyLocal fou out
copyFileOrUrl _    fou out cache | isHttpUrl  fou = copyRemote fou out cache
copyFileOrUrl rd   fou out _     | isSshUrl  fou = copySSH rd (splitSshUrl fou) out
copyFileOrUrl _    fou _   _     = fail $ "unknown transport protocol: " ++ fou

copyLocal  :: String -> FilePath -> IO ()
copyLocal fou out = createLink fou out `catchall` cloneFile fou out

copyRemote :: String -> FilePath -> Cachable -> IO ()
copyRemote u v cache =
    do maybeget <- maybeURLCmd "GET" u
       case maybeget of
         Nothing -> copyRemoteNormal u v cache
         Just get ->
           do let (cmd,args) = breakCommand get
              r <- exec cmd (args++[u]) (Null, File v, AsIs)
              when (r /= ExitSuccess) $
                  fail $ "(" ++ get ++ ") failed to fetch: " ++ u

cloneTree :: FilePath -> FilePath -> IO ()
cloneTree = cloneTreeExcept []

cloneTreeExcept :: [FilePath] -> FilePath -> FilePath -> IO ()
cloneTreeExcept except source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` (".":"..":except)) fps
            mk_source fp = source </> fp
            mk_dest   fp = dest   </> fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else fail ("cloneTreeExcept: Bad source " ++ source)
   `catch` fail ("cloneTreeExcept: Bad source " ++ source)

cloneSubTree :: FilePath -> FilePath -> IO ()
cloneSubTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        createDirectory dest
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` [".", ".."]) fps
            mk_source fp = source </> fp
            mk_dest   fp = dest   </> fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else if isRegularFile fs then
        cloneFile source dest
     else fail ("cloneSubTree: Bad source "++ source)
    `catch` (\e -> if isDoesNotExistError e
                   then return ()
                   else ioError e)

cloneFile :: FilePath -> FilePath -> IO ()
cloneFile = copyFile

backupByRenaming :: FilePath -> IO ()
backupByRenaming = backupBy rename
 where rename x y = do
         isD <- doesDirectoryExist x
         if isD then renameDirectory x y else renameFile x y

backupByCopying :: FilePath -> IO ()
backupByCopying = backupBy copy
 where
  copy x y = do
    isD <- doesDirectoryExist x
    if isD then do createDirectory y
                   cloneTree (normalise x) (normalise y)
           else copyFile x y

backupBy :: (FilePath -> FilePath -> IO ()) -> FilePath -> IO ()
backupBy backup f =
           do hasBF <- doesFileExist f
              hasBD <- doesDirectoryExist f
              when (hasBF || hasBD) $ helper 0
  where
  helper :: Int -> IO ()
  helper i = do existsF <- doesFileExist next
                existsD <- doesDirectoryExist next
                if (existsF || existsD)
                   then helper (i + 1)
                   else do putStrLn $ "Backing up " ++ f ++ "(" ++ suffix ++ ")"
                           backup f next
             where next = f ++ suffix
                   suffix = ".~" ++ show i ++ "~"

copyAndReadFile :: (FilePath -> IO a) -> String -> Cachable -> IO a
copyAndReadFile readfn fou _ | isFile fou = readfn fou
copyAndReadFile readfn fou cache = withTemp $ \t -> do copyFileOrUrl DefaultRemoteDarcs fou t cache
                                                       readfn t

-- | @fetchFile fileOrUrl cache@ returns the content of its argument (either a
-- file or an URL). If it has to download an url, then it will use a cache as
-- required by its second argument.
--
-- We always use default remote darcs, since it is not fatal if the remote
-- darcs does not exist or is too old -- anything that supports transfer-mode
-- should do, and if not, we will fall back to SFTP or SCP.
fetchFilePS :: String -> Cachable -> IO B.ByteString
fetchFilePS = copyAndReadFile B.readFile

-- | @fetchFileLazyPS fileOrUrl cache@ lazily reads the content of its argument
-- (either a file or an URL). Warning: this function may constitute a fd leak;
-- make sure to force consumption of file contents to avoid that. See
-- "fetchFilePS" for details.
fetchFileLazyPS :: String -> Cachable -> IO BL.ByteString
#ifdef HAVE_HTTP
fetchFileLazyPS x c = case parseURI x of
  Just x' | uriScheme x' == "http:" -> do
    rsp <- fmap snd . browse $ do
      setErrHandler . const $ return ()
      setOutHandler . const $ return ()
      setAllowRedirects True
      request $ mkRequest GET x'
    if rspCode rsp /= (2, 0, 0)
      then fail $ "fetchFileLazyPS: " ++ rspReason rsp
      else return $ rspBody rsp
  _ -> copyAndReadFile BL.readFile x c
#else
fetchFileLazyPS x c = copyAndReadFile BL.readFile x c
#endif

gzFetchFilePS :: String -> Cachable -> IO B.ByteString
gzFetchFilePS = copyAndReadFile gzReadFilePS

maybeURLCmd :: String -> String -> IO(Maybe(String))
maybeURLCmd what url =
  do let prot = map toUpper $ takeWhile (/= ':') url
     fmap Just (getEnv ("DARCS_" ++ what ++ "_" ++ prot))
             `catch` \_ -> return Nothing

copyRemoteNormal :: String -> FilePath -> Cachable -> IO ()
copyRemoteNormal u v cache = copyUrlFirst u v cache >> waitUrl u

speculateFileOrUrl :: String -> FilePath -> IO ()
speculateFileOrUrl fou out | isHttpUrl fou = speculateRemote fou out
                           | otherwise = return ()

speculateRemote :: String -> FilePath -> IO () -- speculations are always Cachable
#if defined(HAVE_CURL) || defined(HAVE_HTTP)
speculateRemote u v =
    do maybeget <- maybeURLCmd "GET" u
       case maybeget of
         Just _ -> return () -- can't pipeline these
         Nothing -> copyUrl u v Cachable
#else
speculateRemote u _ = maybeURLCmd "GET" u >> return ()
#endif
