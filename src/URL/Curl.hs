{-# OPTIONS_GHC -fno-warn-dodgy-imports #-} -- needed for GHC 7.0/7.2
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module URL.Curl where

import Control.Exception.Extensible ( bracket )
import Control.Monad ( when )
import Foreign.C.Types ( CLong(..), CInt(..) )

import Progress ( debugMessage )

import URL.Request

import Foreign.C.String ( withCString, peekCString, CString )
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

setDebugHTTP :: IO ()
setDebugHTTP = curl_enable_debug

requestUrl :: String -> FilePath -> Cachable -> IO String
requestUrl u f cache =
    withCString u $ \ustr ->
    withCString f $ \fstr -> do
      err <- curl_request_url ustr fstr (cachableToInt cache) >>= peekCString
      return err

waitNextUrl :: IO (String, String, Maybe ConnectionError)
waitNextUrl =
  bracket malloc free $ \ errorPointer ->
  bracket malloc free $ \ httpErrorPointer -> do
    e <- curl_wait_next_url errorPointer httpErrorPointer >>= peekCString
    ce <- do
           errorNum <- peek errorPointer
           if not (null e)
             then return $
              case errorNum of
                6  -> Just CouldNotResolveHost
                7  -> Just CouldNotConnectToServer
                28 -> Just OperationTimeout
                _  -> Nothing
             else do
              when (errorNum == 90 ) $ debugMessage "The environment variable DARCS_CONNECTION_TIMEOUT is not a number"
              return Nothing
    u <- curl_last_url >>= peekCString
    httpErrorCode <- peek httpErrorPointer
    let detailedErrorMessage = if httpErrorCode > 0
                               then e ++ " " ++ show httpErrorCode
                               else e
    return (u, detailedErrorMessage, ce)

pipeliningEnabled :: IO Bool
pipeliningEnabled = do
  r <- curl_pipelining_enabled
  return $ r /= 0

cachableToInt :: Cachable -> CInt
cachableToInt Cachable = -1
cachableToInt Uncachable = 0
cachableToInt (MaxAge n) = n

foreign import ccall "hscurl.h curl_request_url"
  curl_request_url :: CString -> CString -> CInt -> IO CString

foreign import ccall "hscurl.h curl_wait_next_url"
  curl_wait_next_url :: Ptr CInt -> Ptr CLong-> IO CString

foreign import ccall "hscurl.h curl_last_url"
  curl_last_url :: IO CString

foreign import ccall "hscurl.h curl_enable_debug"
  curl_enable_debug :: IO ()

foreign import ccall "hscurl.h curl_pipelining_enabled"
  curl_pipelining_enabled :: IO CInt
