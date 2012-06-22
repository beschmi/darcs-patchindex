{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using isEmptyChan
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : URL
-- Copyright   : 2008 Dmitry Kurochkin <dmitry.kurochkin@gmail.com>
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module URL ( copyUrl, copyUrlFirst, setDebugHTTP,
             disableHTTPPipelining, maxPipelineLength,
             waitUrl, Cachable(Cachable, Uncachable, MaxAge),
             environmentHelpProxy, environmentHelpProxyPassword,
             ConnectionError(..),
           ) where

import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Directory ( copyFile )
import System.IO.Unsafe ( unsafePerformIO )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( isEmptyChan, newChan, readChan, writeChan, Chan )
import Control.Concurrent.MVar ( isEmptyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, withMVar, MVar )
import Control.Monad ( unless, when )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( evalStateT, get, modify, put, StateT )

import Workaround ( renameFile )
import Darcs.Global ( atexit )
import Progress ( debugMessage )
import Darcs.Repository.Lock ( removeFileMayNotExist )

import Numeric ( showHex )
import System.Random ( randomRIO )
import URL.Request

#ifdef HAVE_CURL
import qualified URL.Curl as Curl
#elif defined(HAVE_HTTP)
import qualified URL.HTTP as HTTP 
#else
import Progress ( debugFail )
import qualified HTTP ( requestUrl, waitNextUrl )
#endif
#include "impossible.h"

{-# NOINLINE maxPipelineLengthRef #-}
maxPipelineLengthRef :: IORef Int
maxPipelineLengthRef = unsafePerformIO $ do
  enabled <- pipeliningEnabled
#ifdef HAVE_CURL
  when (not enabled) (debugMessage $
                      "Warning: pipelining is disabled, because libcurl "++
                      "version darcs was compiled with is too old (< 7.19.1)")
#endif
  newIORef $ if enabled then 100 else 1

maxPipelineLength :: IO Int
maxPipelineLength = readIORef maxPipelineLengthRef

{-# NOINLINE urlNotifications #-}
urlNotifications :: MVar (Map String (MVar String))
urlNotifications = unsafePerformIO $ newMVar Map.empty

{-# NOINLINE urlChan #-}
urlChan :: Chan UrlRequest
urlChan = unsafePerformIO $ do
  ch <- newChan
  _ <- forkIO (urlThread ch)
  return ch

-- ----------------------------------------------------------------------
-- urlThread
-- ----------------------------------------------------------------------

type UrlM a = StateT UrlState IO a

urlThread :: Chan UrlRequest -> IO ()
urlThread ch = do junk <- flip showHex "" `fmap` randomRIO rrange
                  evalStateT (urlThread' ch) (UrlState Map.empty emptyQ 0 junk)
    where rrange = (0, 2^(128 :: Integer) :: Integer)

-- | Internal to urlThread
urlThread' :: Chan UrlRequest -> UrlM ()
urlThread' ch = do
            empty <- liftIO $ isEmptyChan ch
            st <- get
            let l = pipeLength st
                w = waitToStart st
            reqs <- if not empty || (nullQ w && l == 0)
                    then liftIO (readAllRequests ch)
                    else return []
            mapM_ addReq reqs
            checkWaitToStart
            waitNextUrl
            urlThread' ch

-- | Internal to urlThread
readAllRequests :: Chan UrlRequest -> IO [UrlRequest]
readAllRequests ch = do
            r <- readChan ch
            debugMessage $ "URL.urlThread ("++url r++"\n"++
                           "            -> "++file r++")"
            empty <- isEmptyChan ch
            reqs <- if not empty
                    then readAllRequests ch
                    else return []
            return (r:reqs)

-- | Internal to urlThread
addReq :: UrlRequest -> UrlM ()
addReq r = do
  d <- liftIO (alreadyDownloaded u)
  if d
     then dbg "Ignoring UrlRequest of URL that is already downloaded."
     else reallyAdd
 where
   f = file r
   c = cachable r
   u = url r
   reallyAdd = do
                 st <- get
                 let p = inProgress st
                     w = waitToStart st
                     e = (f, [], c)
                     new_w = case priority r of
                               High -> pushQ u w
                               Low  -> insertQ u w
                     new_st = st { inProgress = Map.insert u e p
                                 , waitToStart = new_w }
                 case Map.lookup u p of
                   Just (f', fs', c') -> do
                     let new_c = minCachable c c'
                     when (c /= c') $ let new_p = Map.insert u (f', fs', new_c) p
                                      in do modify (\s -> s { inProgress = new_p })
                                            dbg $ "Changing "++u++" request cachability from "++show c++" to "++show new_c
                     when (u `elemQ` w && priority r == High) $ do
                       modify (\s -> s { waitToStart = pushQ u (deleteQ u w) })
                       dbg $ "Moving "++u++" to head of download queue."
                     if f `notElem` (f':fs')
                        then let new_p = Map.insert u (f', f:fs', new_c) p
                             in do modify (\s -> s { inProgress = new_p })
                                   dbg "Adding new file to existing UrlRequest."
                        else dbg "Ignoring UrlRequest of file that's already queued."
                   _ -> put new_st


-- | Internal to urlThread
alreadyDownloaded :: String -> IO Bool
alreadyDownloaded u = do
            n <- withMVar urlNotifications (return . (Map.lookup u))
            case n of
              Just v  -> not `fmap` isEmptyMVar v
              Nothing -> return True

-- ----------------------------------------------------------------------

checkWaitToStart :: StateT UrlState IO ()
checkWaitToStart = do
  st <- get
  let l = pipeLength st
  mpl <- liftIO maxPipelineLength
  when (l < mpl) $ do
    let w = waitToStart st
    case readQ w of
      Just (u,rest) -> do
        case Map.lookup u (inProgress st) of
          Just (f, _, c) -> do
            dbg ("URL.requestUrl ("++u++"\n"++
                 "              -> "++f++")")
            let f_new = f++"-new_"++randomJunk st
            err <- liftIO $ requestUrl u f_new c
            if null err
               then do liftIO $ atexit (removeFileMayNotExist f_new)
                       put $ st { waitToStart = rest
                                , pipeLength = l + 1 }
               else do dbg $ "Failed to start download URL "++u++": "++err
                       liftIO $ do removeFileMayNotExist f_new
                                   downloadComplete u err
                       put $ st { waitToStart = rest }
          _              -> bug $ "Possible bug in URL.checkWaitToStart "++u
        checkWaitToStart
      _ -> return ()

copyUrlFirst :: String -> FilePath -> Cachable -> IO ()
copyUrlFirst = copyUrlWithPriority High

copyUrl :: String -> FilePath -> Cachable -> IO ()
copyUrl = copyUrlWithPriority Low

copyUrlWithPriority :: Priority -> String -> String -> Cachable -> IO ()
copyUrlWithPriority p u f c = do
  debugMessage ("URL.copyUrlWithPriority ("++u++"\n"++
                "                      -> "++f++")")
  v <- newEmptyMVar
  let fn _ old_val = old_val
  modifyMVar_ urlNotifications (return . (Map.insertWith fn u v))
  let r = UrlRequest u f c p
  writeChan urlChan r

waitNextUrl :: StateT UrlState IO ()
waitNextUrl = do
  st <- get
  let l = pipeLength st
  when (l > 0) $ do
                dbg "URL.waitNextUrl start"
                (u, e, ce) <- liftIO $ waitNextUrl'
                let p = inProgress st
                    new_st = st { inProgress = Map.delete u p
                                , pipeLength = l - 1 }
                liftIO $ if null e
                         then case Map.lookup u p of
                                Just (f, fs, _) -> do
                                  renameFile (f++"-new_"++randomJunk st) f
                                  mapM_ (safeCopyFile st f) fs
                                  downloadComplete u e
                                  debugMessage $ "URL.waitNextUrl succeeded: "++u++" "++f
                                Nothing -> bug $ "Possible bug in URL.waitNextUrl: "++u
                         else case Map.lookup u p of
                                Just (f, _, _) -> do
                                  removeFileMayNotExist (f++"-new_"++randomJunk st)
                                  case ce of
                                    Just httpError -> downloadComplete u (show httpError)
                                    Nothing        -> downloadComplete u e
                                  debugMessage $ "URL.waitNextUrl failed: "++
                                               u++" "++f++" "++e
                                Nothing -> bug $ "Another possible bug in URL.waitNextUrl: "++u++" "++e
                unless (null u) $ put new_st
    where safeCopyFile st f t = let new_t = t++"-new_"++randomJunk st
                                in do copyFile f new_t
                                      renameFile new_t t

downloadComplete :: String -> String -> IO ()
downloadComplete u e = do
  r <- withMVar urlNotifications (return . (Map.lookup u))
  case r of
    Just notifyVar -> putMVar notifyVar e
    Nothing -> debugMessage $ "downloadComplete URL '"++u++"' downloaded several times"

waitUrl :: String -> IO ()
waitUrl u = do debugMessage $ "URL.waitUrl "++u
               r <- withMVar urlNotifications (return . (Map.lookup u))
               case r of
                 Just var -> do
                        e <- readMVar var
                        modifyMVar_ urlNotifications (return . (Map.delete u))
                        unless (null e) $ do
                          debugMessage $ "Failed to download URL "++u++": "++e
                          fail e
                 Nothing  -> return () -- file was already downloaded

dbg :: String -> StateT a IO ()
dbg = liftIO . debugMessage

minCachable :: Cachable -> Cachable -> Cachable
minCachable Uncachable _          = Uncachable
minCachable _          Uncachable = Uncachable
minCachable (MaxAge a) (MaxAge b) = MaxAge $ min a b
minCachable (MaxAge a) _          = MaxAge a
minCachable _          (MaxAge b) = MaxAge b
minCachable _          _          = Cachable

disableHTTPPipelining :: IO ()
disableHTTPPipelining = writeIORef maxPipelineLengthRef 1

setDebugHTTP :: IO ()
requestUrl :: String -> FilePath -> Cachable -> IO String
waitNextUrl' :: IO (String, String, Maybe ConnectionError)
pipeliningEnabled :: IO Bool

#ifdef HAVE_CURL

setDebugHTTP = Curl.setDebugHTTP
requestUrl = Curl.requestUrl
waitNextUrl' = Curl.waitNextUrl
pipeliningEnabled = Curl.pipeliningEnabled

#elif defined(HAVE_HTTP)

setDebugHTTP = return ()
requestUrl = HTTP.requestUrl
waitNextUrl' = HTTP.waitNextUrl
pipeliningEnabled = return False

#else

setDebugHTTP = debugMessage "URL.setDebugHttp works only with libcurl"
requestUrl _ _ _ = debugFail "URL.requestUrl: there is no libcurl!"
waitNextUrl' = debugFail "URL.waitNextUrl': there is no libcurl!"
pipeliningEnabled = return False

#endif

-- Usage of these environment variables happens in C code, so the
-- closest to "literate" user documentation is here, where the
-- offending function 'curl_request_url' is imported.
environmentHelpProxy :: ([String], [String])
environmentHelpProxy = (["HTTP_PROXY", "HTTPS_PROXY", "FTP_PROXY",
                         "ALL_PROXY", "NO_PROXY"], [
 "If Darcs was built with libcurl, the environment variables HTTP_PROXY,",
 "HTTPS_PROXY and FTP_PROXY can be set to the URL of a proxy in the form",
 "",
 "  [protocol://]<host>[:port]",
 "",
 "In which case libcurl will use the proxy for the associated protocol",
 "(HTTP, HTTPS and FTP).  The environment variable ALL_PROXY can be used",
 "to set a single proxy for all libcurl requests.",
 "",
 "If the environment variable NO_PROXY is a comma-separated list of host",
 "names, access to those hosts will bypass proxies defined by the above",
 "variables.  For example, it is quite common to avoid proxying requests",
 "to machines on the local network with",
 "",
 "  NO_PROXY=localhost,*.localdomain",
 "",
 "For compatibility with lynx et al, lowercase equivalents of these",
 "environment variables (e.g. $http_proxy) are also understood and are",
 "used in preference to the uppercase versions.",
 "",
 "If Darcs was not built with libcurl, all these environment variables",
 "are silently ignored, and there is no way to use a web proxy."])

environmentHelpProxyPassword :: ([String], [String])
environmentHelpProxyPassword = (["DARCS_PROXYUSERPWD"], [
 "If Darcs was built with libcurl, and you are using a web proxy that",
 "requires authentication, you can set the $DARCS_PROXYUSERPWD",
 "environment variable to the username and password expected by the",
 "proxy, separated by a colon.  This environment variable is silently",
 "ignored if Darcs was not built with libcurl."])
