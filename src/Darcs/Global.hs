-- Copyright (C) 2005 Tomasz Zielonka
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

{-# LANGUAGE CPP #-}

-- |
-- Module      : Darcs.Global
-- Copyright   : 2005 Tomasz Zielonka
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- This was originally Tomasz Zielonka's AtExit module, slightly generalised
-- to include global variables.  Here, we attempt to cover broad, global
-- features, such as exit handlers.  These features slightly break the Haskellian
-- purity of darcs, in favour of programming convenience.

module Darcs.Global
    (
      atexit
    , withAtexit
    , SshSettings(..)
    , defaultSsh
    , timingsMode
    , setTimingsMode
    , whenDebugMode
    , withDebugMode
    , setDebugMode
    , debugMessage
    , debugFail
    , putTiming
    , addCRCWarning
    , getCRCWarnings
    , resetCRCWarnings
    , addBadSource
    , getBadSourcesList
    , isBadSource
    , darcsdir
    , isReachableSource
    , addReachableSource
    , windows
    ) where


import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( when )
import Control.Concurrent.MVar
import Control.Exception.Extensible ( bracket_, catch, catchJust, SomeException
                                    , mask
                                    )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Data.IORef ( modifyIORef )
import Data.List ( isPrefixOf )
import System.Info ( os )
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, hPutStr, stderr)
import System.IO.Error ( ioeGetErrorType, isDoesNotExistErrorType )
import System.Process ( readProcessWithExitCode )
import System.Time ( calendarTimeToString, toCalendarTime, getClockTime )
import System.Environment ( getEnv )
import Prelude hiding (catch)


windows :: Bool
windows = "mingw" `isPrefixOf` os


atexitActions :: MVar (Maybe [IO ()])
atexitActions = unsafePerformIO (newMVar (Just []))
{-# NOINLINE atexitActions #-}


-- | Registers an IO action to run just before darcs exits. Useful for removing
-- temporary files and directories, for example. Referenced in Issue1914.
atexit :: IO ()
       -> IO ()
atexit action =
    modifyMVar_ atexitActions $ \ml ->
        case ml of
            Just l ->
                return (Just (action : l))
            Nothing -> do
                hPutStrLn stderr "It's too late to use atexit"
                return Nothing


withAtexit :: IO a -> IO a
withAtexit prog =
    bracket_
        (return ())
        exit
        prog
  where
    exit = mask $ \unmask -> do
        Just actions <- swapMVar atexitActions Nothing
        -- from now on atexit will not register new actions
        mapM_ (runAction unmask) actions
    runAction unmask action =
        catch (unmask action) $ \(exn :: SomeException) -> do
            hPutStrLn stderr $ "Exception thrown by an atexit registered action:"
            hPutStrLn stderr $ show exn


-- Write-once-read-many global variables make it easier to implement flags, such
-- as --no-ssh-cm. Using global variables reduces the number of parameters that
-- we have to pass around, but it is rather unsafe and should be used sparingly.


_debugMode :: IORef Bool
_debugMode = unsafePerformIO $ newIORef False
{-# NOINLINE _debugMode #-}


setDebugMode :: IO ()
setDebugMode = writeIORef _debugMode True


whenDebugMode :: IO () -> IO ()
whenDebugMode j = do b <- readIORef _debugMode
                     when b j


withDebugMode :: (Bool -> IO a) -> IO a
withDebugMode j = readIORef _debugMode >>= j


debugMessage :: String -> IO ()
debugMessage m = whenDebugMode $ do putTiming; hPutStrLn stderr m


debugFail :: String -> IO a
debugFail m = debugMessage m >> fail m


putTiming :: IO ()
putTiming = when timingsMode $ do
    t <- getClockTime >>= toCalendarTime
    hPutStr stderr (calendarTimeToString t++": ")


_timingsMode :: IORef Bool
_timingsMode = unsafePerformIO $ newIORef False
{-# NOINLINE _timingsMode #-}


setTimingsMode :: IO ()
setTimingsMode = writeIORef _timingsMode True


timingsMode :: Bool
timingsMode = unsafePerformIO $ readIORef _timingsMode
{-# NOINLINE timingsMode #-}


data SshSettings = SshSettings
    { ssh :: String
    , scp :: String
    , sftp :: String
    } deriving (Show, Eq)


_defaultSsh :: IORef SshSettings
_defaultSsh = unsafePerformIO $ newIORef =<< detectSsh


-- | Expected properties:
--
-- * only ever runs once in the lifetime of the program
-- * environment variables override all
-- * tries Putty first on Windows
-- * falls back to plain old ssh
detectSsh :: IO SshSettings
detectSsh = do
    whenDebugMode (putStrLn "Detecting SSH settings")
    vanilla <-  if windows
                  then do
                    plinkStr <- (snd3 <$> readProcessWithExitCode "plink" [] "")
                                  `catch` \(e :: SomeException) -> return (show e)
                    whenDebugMode $ putStrLn $
                        "SSH settings (plink): " ++
                        (concat . take 1 . lines $ plinkStr)
                    if "PuTTY" `isPrefixOf` plinkStr
                      then return (SshSettings "plink" "pscp -q" "psftp")
                      else return rawVanilla
                  else return rawVanilla
    settings <- SshSettings <$> fromEnv (ssh vanilla)  "DARCS_SSH"
                            <*> fromEnv (scp vanilla)  "DARCS_SCP"
                            <*> fromEnv (sftp vanilla) "DARCS_SFTP"
    whenDebugMode (putStrLn $ "SSH settings: " ++ show settings)
    return settings
  where
    snd3 (_, x, _) = x
    rawVanilla = SshSettings "ssh" "scp -q" "sftp"
    fromEnv :: String -> String -> IO String
    fromEnv d v = catchJust notFound
                            (getEnv v)
                            (const (return d))
    notFound e =  if isDoesNotExistErrorType (ioeGetErrorType e)
                  then Just ()
                  else Nothing


defaultSsh :: SshSettings
defaultSsh = unsafePerformIO $ readIORef _defaultSsh


type CRCWarningList = [FilePath]
_crcWarningList :: IORef CRCWarningList
_crcWarningList = unsafePerformIO $ newIORef []
{-# NOINLINE _crcWarningList #-}


addCRCWarning :: FilePath -> IO ()
addCRCWarning fp = modifyIORef _crcWarningList (fp:)


getCRCWarnings :: IO [FilePath]
getCRCWarnings = readIORef _crcWarningList


resetCRCWarnings :: IO ()
resetCRCWarnings = writeIORef _crcWarningList []


_badSourcesList :: IORef [String]
_badSourcesList = unsafePerformIO $ newIORef []
{- NOINLINE _badSourcesList -}


addBadSource :: String -> IO ()
addBadSource cache = modifyIORef _badSourcesList (cache:)


getBadSourcesList :: IO [String]
getBadSourcesList = readIORef _badSourcesList


isBadSource :: IO (String -> Bool)
isBadSource = do
    badSources <- getBadSourcesList
    return (`elem` badSources)


_reachableSourcesList :: IORef [String]
_reachableSourcesList = unsafePerformIO $ newIORef []
{- NOINLINE _reachableSourcesList -}


addReachableSource :: String -> IO ()
addReachableSource src = modifyIORef _reachableSourcesList (src:)


getReachableSources :: IO [String]
getReachableSources = readIORef _reachableSourcesList


isReachableSource :: IO (String -> Bool)
isReachableSource =  do
    reachableSources <- getReachableSources
    return (`elem` reachableSources)


darcsdir :: String
darcsdir = "_darcs"
