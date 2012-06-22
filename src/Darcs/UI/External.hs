{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using Prelude.catch
{-# LANGUAGE ForeignFunctionInterface #-}

module Darcs.UI.External
    ( sendEmail
    , generateEmail
    , sendEmailDoc
    , resendEmail
    , signString
    , verifyPS
    , execDocPipe
    , execPipeIgnoreError
    , pipeDoc
    , pipeDocSSH
    , execSSH
    , maybeURLCmd
    , viewDoc
    , viewDocWith
    , haveSendmail
    , sendmailPath
    , diffProgram
    , darcsProgram
    , editText
    , editFile
    , catchall
    --  * Locales
    , setDarcsEncodings
    , getSystemEncoding
    , isUTF8Locale
    ) where

import qualified Ratified
import Data.Maybe ( isJust, isNothing, maybeToList )
import Control.Monad ( when, filterM, liftM2, void )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv, getProgName )
import System.IO ( hSetBinaryMode, hPutStr, hPutStrLn, hClose,
                   openBinaryFile, IOMode( ReadMode ),
                   openBinaryTempFile,
                   hIsTerminalDevice, stdout, stderr, Handle )
import System.Directory ( doesFileExist,
                          findExecutable )
import System.FilePath.Posix ( (</>) )
import System.Process ( runProcess, runInteractiveProcess, waitForProcess )
import Exec ( execInteractive )

#ifdef FORCE_CHAR8_ENCODING
import GHC.IO.Encoding ( setFileSystemEncoding, setForeignEncoding, char8 )
#endif

import Foreign.C.String ( CString, peekCString )

import Control.Concurrent ( forkIO, newEmptyMVar, putMVar, takeMVar )
import Control.Exception.Extensible ( bracket, try, finally, SomeException )
import Data.Char ( toUpper, toLower )
import Text.Regex
#if defined (HAVE_MAPI)
import Foreign.C ( withCString )
#endif
#ifdef HAVE_MAPI
import Foreign.Ptr ( nullPtr )
import Darcs.Repository.Lock ( canonFilename, writeDocBinFile )
#endif

import Darcs.SignalHandler ( catchNonSignal )
import Darcs.UI.Flags ( DarcsFlag( SignAs, Sign, SignSSL,
                                Verify, VerifySSL ))
import Darcs.Path
    ( AbsolutePath
    , toFilePath
    , FilePathLike
    )
import Progress ( withoutProgress, debugMessage )

import ByteStringUtils (linesPS, unlinesPS)
import qualified Data.ByteString as B (ByteString, empty, null, readFile
            ,hGetContents, writeFile, hPut, length
            ,take, concat, drop, isPrefixOf, singleton, append)
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Darcs.Repository.Lock
    ( withTemp
    , withNamedTemp
    , withOpenTemp
    , tempdirLoc
    , removeFileMayNotExist
    )
import CommandLine ( parseCmd, addUrlencoded )
import Exec ( exec, Redirect(..), withoutNonBlock )
import Darcs.Ssh ( getSSH, SSHCmd(..) )
import Darcs.URL ( SshFilePath, sshUhost )
import Printer ( Doc, Printers, putDocLnWith, hPutDoc, hPutDocLn, hPutDocWith, ($$), renderPS,
                 simplePrinters,
                 text, empty, packedString, vcat, renderString )
import Darcs.UI.Email ( formatHeader )

sendmailPath :: IO String
sendmailPath = do
  l <- filterM doesFileExist $ liftM2 (</>)
                [ "/usr/sbin", "/sbin", "/usr/lib" ]
                [ "sendmail" ]
  ex <- findExecutable "sendmail"
  when (isNothing ex && null l) $ fail "Cannot find the \"sendmail\" program."
  return $ head $ maybeToList ex ++ l

diffProgram :: IO String
diffProgram = do
  l <- filterM (fmap isJust . findExecutable) [ "gdiff", "gnudiff", "diff" ]
  when (null l) $ fail "Cannot find the \"diff\" program."
  return $ head l

-- |Get the name of the darcs executable (as supplied by @getProgName@)
darcsProgram :: IO String
darcsProgram = getProgName
-- Another option: getEnv "DARCS" `catch` \_ -> getProgName

maybeURLCmd :: String -> String -> IO(Maybe(String))
maybeURLCmd what url =
  do let prot = map toUpper $ takeWhile (/= ':') url
     fmap Just (getEnv ("DARCS_" ++ what ++ "_" ++ prot))
             `catch` \_ -> return Nothing


-- | Run a command on a remote location without passing it any input or
--   reading its output.  Return its ExitCode
execSSH :: SshFilePath -> String -> IO ExitCode
execSSH remoteAddr command =
    do (ssh, ssh_args) <- getSSH SSH
       debugMessage $ unwords (ssh:ssh_args++[sshUhost remoteAddr,command])
       withoutProgress $ do hid <- runProcess ssh (ssh_args++[sshUhost remoteAddr,command])
                                   Nothing Nothing Nothing Nothing Nothing
                            waitForProcess hid

pipeDoc :: String -> [String] -> Doc -> IO ExitCode
pipeDoc c args inp = withoutNonBlock $ withoutProgress $
    do debugMessage $ unwords (c:args)
       (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
       hSetBinaryMode i True
       hSetBinaryMode o True
       mvare <- newEmptyMVar
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       mvaro <- newEmptyMVar
       _ <- forkIO ((Ratified.hGetContents o >>= -- ratify: immediately consumed
                hPutStr stdout)
               `finally` putMVar mvaro ())
       hPutDoc i inp
       hClose i
       rval <- waitForProcess pid
       takeMVar mvare
       takeMVar mvaro
       when (rval == ExitFailure 127) $
            putStrLn $ "Command not found:\n   "++ show (c:args)
       return rval

pipeDocSSH :: SshFilePath -> [String] -> Doc -> IO ExitCode
pipeDocSSH remoteAddr args input =
    do (ssh, ssh_args) <- getSSH SSH
       pipeDoc ssh (ssh_args++ (sshUhost remoteAddr:args)) input

sendEmail :: String -> String -> String -> String -> String -> String -> IO ()
sendEmail f t s cc scmd body =
  sendEmailDoc f t s cc scmd Nothing (text body)


generateEmail
    :: Handle  -- ^ handle to write email to
    -> String  -- ^ From
    -> String  -- ^ To
    -> String  -- ^ Subject
    -> String  -- ^ CC
    -> Doc     -- ^ body
    -> IO ()
generateEmail h f t s cc body = do
     putHeader "To" t
     putHeader "From" f
     putHeader "Subject" s
     when (not (null cc)) (putHeader "Cc" cc)
     putHeader "X-Mail-Originator" "Darcs Version Control System"
     hPutDocLn h body
  where putHeader field value
            = B.hPut h (B.append (formatHeader field value) newline)
        newline = B.singleton 10

haveSendmail :: IO Bool
haveSendmail = (sendmailPath >> return True) `catch` (\_ -> return False)

-- | Send an email, optionally containing a patch bundle
--   (more precisely, its description and the bundle itself)
sendEmailDoc
  :: String           -- ^ from
  -> String           -- ^ to
  -> String           -- ^ subject
  -> String           -- ^ cc
  -> String           -- ^ send command
  -> Maybe (Doc, Doc) -- ^ (content,bundle)
  -> Doc              -- ^ body
  -> IO ()
sendEmailDoc _ "" _ "" _ _ _ = return ()
sendEmailDoc f "" s cc scmd mbundle body =
  sendEmailDoc f cc s "" scmd mbundle body
sendEmailDoc f t s cc scmd mbundle body = do
  use_sendmail <- haveSendmail
  if use_sendmail || scmd /= "" then
    withOpenTemp $ \(h,fn) -> do
      generateEmail h f t s cc body
      hClose h
      withOpenTemp $ \(hat,at) -> do
        ftable' <- case mbundle of
                   Just (content,bundle) -> do
                       hPutDocLn hat $ bundle
                       return [ ('b', renderString content) , ('a', at) ]
                   Nothing ->
                       return [ ('b', renderString body) ]
        hClose hat
        let ftable = [ ('t',addressOnly t),('c',cc),('f',f),('s',s) ] ++ ftable'
        r <- execSendmail ftable scmd fn
        when (r /= ExitSuccess) $ fail ("failed to send mail to: "
                                       ++ t ++ cc_list cc
                                       ++ "\nPerhaps sendmail is not configured.")
#ifdef HAVE_MAPI
   else do
     r <- withCString t $ \tp ->
           withCString f $ \fp ->
            withCString cc $ \ccp ->
             withCString s $ \sp ->
              withOpenTemp $ \(h,fn) -> do
               hPutDoc h body
               hClose h
               writeDocBinFile "mailed_patch" body
               cfn <- canonFilename fn
               withCString cfn $ \pcfn ->
                c_send_email fp tp ccp sp nullPtr pcfn
     when (r /= 0) $ fail ("failed to send mail to: " ++ t)
#else
   else fail $ "no mail facility (sendmail or mapi) located at configure time!"
#endif
  where addressOnly a =
          case dropWhile (/= '<') a of
          ('<':a2) -> takeWhile (/= '>') a2
          _        -> a

        cc_list [] = []
        cc_list c = " and cc'ed " ++ c

resendEmail :: String -> String -> B.ByteString -> IO ()
resendEmail "" _ _ = return ()
resendEmail t scmd body = do
  use_sendmail <- haveSendmail
  if use_sendmail || scmd /= ""
   then
    withOpenTemp $ \(h,fn) -> do
     hPutStrLn h $ "To: "++ t
     hPutStrLn h $ find_from (linesPS body)
     hPutStrLn h $ find_subject (linesPS body)
     hPutDocLn h $ fixit $ linesPS body
     hClose h
     let ftable = [('t',t)]
     r <-  execSendmail ftable scmd fn
     when (r /= ExitSuccess) $ fail ("failed to send mail to: " ++ t)
   else
#ifdef HAVE_MAPI
    fail "Don't know how to resend email with MAPI"
#else
    fail "no mail facility (sendmail or mapi) located at configure time (use the sendmail-command option)!"
#endif
  where br            = BC.pack "\r"
        darcsurl      = BC.pack "DarcsURL:"
        content       = BC.pack "Content-"
        from_start    = BC.pack "From:"
        subject_start = BC.pack "Subject:"
        fixit (l:ls)
         | B.null l = packedString B.empty $$ vcat (map packedString ls)
         | l == br = packedString B.empty $$ vcat (map packedString ls)
         | B.take 9 l == darcsurl || B.take 8 l == content
            = packedString l $$ fixit ls
         | otherwise = fixit ls
        fixit [] = empty
        find_from (l:ls) | B.take 5 l == from_start = BC.unpack l
                         | otherwise = find_from ls
        find_from [] = "From: unknown"
        find_subject (l:ls) | B.take 8 l == subject_start = BC.unpack l
                            | otherwise = find_subject ls
        find_subject [] = "Subject: (no subject)"

execSendmail :: [(Char,String)] -> String -> String -> IO ExitCode
execSendmail ftable scmd fn =
  if scmd == "" then do
     cmd <- sendmailPath
     exec cmd ["-i", "-t"] (File fn, Null, AsIs)
  else case parseCmd (addUrlencoded ftable) scmd of
         Right (arg0:opts, wantstdin) ->
           do let stdin = if wantstdin then File fn else Null
              exec arg0 opts (stdin, Null, AsIs)
         Left e -> fail $ ("failed to send mail, invalid sendmail-command: "++(show e))
         _ -> fail $ ("failed to send mail, invalid sendmail-command")

#ifdef HAVE_MAPI
foreign import ccall "win32/send_email.h send_email" c_send_email
             :: CString -> {- sender -}
                CString -> {- recipient -}
                CString -> {- cc -}
                CString -> {- subject -}
                CString -> {- body -}
                CString -> {- path -}
                IO Int
#endif

execPSPipe :: String -> [String] -> B.ByteString -> IO B.ByteString
execPSPipe c args ps = fmap renderPS
                     $ execDocPipe c args
                     $ packedString ps

execDocPipe :: String -> [String] -> Doc -> IO Doc
execDocPipe c args instr = withoutProgress $
    do (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
       _ <- forkIO $ hPutDoc i instr >> hClose i
       mvare <- newEmptyMVar
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       out <- B.hGetContents o
       rval <- waitForProcess pid
       takeMVar mvare
       case rval of
         ExitFailure ec ->fail $ "External program '"++c++
                          "' failed with exit code "++ show ec
         ExitSuccess -> return $ packedString out

-- The following is needed for diff, which returns non-zero whenever
-- the files differ.
execPipeIgnoreError :: String -> [String] -> Doc -> IO Doc
execPipeIgnoreError c args instr = withoutProgress $
    do (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
       _ <- forkIO $ hPutDoc i instr >> hClose i
       mvare <- newEmptyMVar
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       out <- B.hGetContents o
       _ <- waitForProcess pid
       takeMVar mvare
       return $ if B.null out then empty else packedString out

signString :: [DarcsFlag] -> Doc -> IO Doc
signString [] d = return d
signString (Sign:_) d = signPGP [] d
signString (SignAs keyid:_) d = signPGP ["--local-user", keyid] d
signString (SignSSL idf:_) d = signSSL idf d
signString (_:os) d = signString os d

signPGP :: [String] -> Doc -> IO Doc
signPGP args t = execDocPipe "gpg" ("--clearsign":args) t

signSSL :: String -> Doc -> IO Doc
signSSL idfile t =
    withTemp $ \cert -> do
    opensslPS ["req", "-new", "-key", idfile,
               "-outform", "PEM", "-days", "365"]
                (BC.pack "\n\n\n\n\n\n\n\n\n\n\n")
                >>= opensslPS ["x509", "-req", "-extensions",
                               "v3_ca", "-signkey", idfile,
                               "-outform", "PEM", "-days", "365"]
                >>= opensslPS ["x509", "-outform", "PEM"]
                >>= B.writeFile cert
    opensslDoc ["smime", "-sign", "-signer", cert,
                "-inkey", idfile, "-noattr", "-text"] t
    where opensslDoc = execDocPipe "openssl"
          opensslPS = execPSPipe "openssl"


verifyPS :: [DarcsFlag] -> B.ByteString -> IO (Maybe B.ByteString)
verifyPS [] ps = return $ Just ps
verifyPS (Verify pks:_) ps = verifyGPG pks ps
verifyPS (VerifySSL auks:_) ps = verifySSL auks ps
verifyPS (_:os) ps = verifyPS os ps

verifyGPG :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifyGPG goodkeys s =
    withOpenTemp $ \(th,tn) -> do
      B.hPut th s
      hClose th
      rval <- exec "gpg"  ["--batch","--no-default-keyring",
                           "--keyring",fix_path $ toFilePath goodkeys, "--verify"]
                           (File tn, Null, Null)
      case rval of
          ExitSuccess -> return $ Just gpg_fixed_s
          _ -> return Nothing
      where gpg_fixed_s = let
                not_begin_signature x =
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----"
                    &&
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----\r"
                in unlinesPS $ map fix_line $ tail $ dropWhile not_begin_signature $ linesPS s
            fix_line x | B.length x < 3 = x
                       | BC.pack "- -" `B.isPrefixOf` x = B.drop 2 x
                       | otherwise = x
#if defined(WIN32)
            fix_sep c | c=='/' = '\\'   | otherwise = c
            fix_path p = map fix_sep p
#else
            fix_path p = p
#endif

verifySSL :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifySSL goodkeys s = do
    certdata <- opensslPS ["smime", "-pk7out"] s
                >>= opensslPS ["pkcs7", "-print_certs"]
    cruddy_pk <- opensslPS ["x509", "-pubkey"] certdata
    let key_used = B.concat $ tail $
                   takeWhile (/= BC.pack"-----END PUBLIC KEY-----")
                           $ linesPS cruddy_pk
        in do allowed_keys <- linesPS `fmap` B.readFile (toFilePath goodkeys)
              if not $ key_used `elem` allowed_keys
                then return Nothing -- Not an allowed key!
                else withTemp $ \cert ->
                     withTemp $ \on ->
                     withOpenTemp $ \(th,tn) -> do
                     B.hPut th s
                     hClose th
                     B.writeFile cert certdata
                     rval <- exec "openssl" ["smime", "-verify", "-CAfile",
                                             cert, "-certfile", cert]
                                             (File tn, File on, Null)
                     case rval of
                       ExitSuccess -> Just `fmap` B.readFile on
                       _ -> return Nothing
    where opensslPS = execPSPipe "openssl"

viewDoc :: Doc -> IO ()
viewDoc = viewDocWith simplePrinters

viewDocWith :: Printers -> Doc -> IO ()
viewDocWith pr msg = do
  isTerminal <- hIsTerminalDevice stdout
  void $ if isTerminal && lengthGreaterThan (20 :: Int) (lines $ renderString msg)
     then do viewerPlusArgs <- getViewer
             let (viewer:args) = words viewerPlusArgs
             pipeDocToPager viewer args pr msg
               `ortryrunning` pipeDocToPager  "less" [] pr msg
               `ortryrunning` pipeDocToPager  "more" [] pr msg
#ifdef WIN32
               `ortryrunning` pipeDocToPager  "more.com" [] pr msg
#endif
               `ortryrunning` pipeDocToPager "" [] pr msg
     else pipeDocToPager "" [] pr msg
              where lengthGreaterThan n _ | n <= 0 = True
                    lengthGreaterThan _ [] = False
                    lengthGreaterThan n (_:xs) = lengthGreaterThan (n-1) xs

getViewer :: IO String
getViewer = getEnv "DARCS_PAGER" `catchall`
             getEnv "PAGER" `catchall` return "less"

pipeDocToPager :: String -> [String] -> Printers -> Doc -> IO ExitCode

pipeDocToPager "" _ pr inp = do
  putDocLnWith pr inp
  return ExitSuccess

pipeDocToPager c args pr inp = withoutNonBlock $ withoutProgress $ do
  tmp <- tempdirLoc
  bracket (openBinaryTempFile tmp "darcs-pager") cleanup $ \(fn,fh) ->
    do hPutDocWith pr fh inp
       hClose fh
       bracket (openBinaryFile fn ReadMode) hClose $ \h ->
         do x <- do waitForProcess  =<< runProcess c args Nothing Nothing (Just h) Nothing Nothing
            when (x == ExitFailure 127) $
                 putStrLn $ "Command not found:\n   "++ show (c:args)
            return x
  where
    cleanup (f,h) = do _ <- try (hClose h) :: IO (Either SomeException ())
                       removeFileMayNotExist f

-- | Given two shell commands as arguments, execute the former.  The
-- latter is then executed if the former failed because the executable
-- wasn't found (code 127), wasn't executable (code 126) or some other
-- exception occurred.  Other failures (such as the user holding ^C)
-- do not cause the second command to be tried.
ortryrunning :: IO ExitCode
             -> IO ExitCode
             -> IO ExitCode
a `ortryrunning` b = do
  ret <- try a
  case ret of
    (Right (ExitFailure 126)) -> b -- command not executable
    (Right (ExitFailure 127)) -> b -- command not found
#ifdef WIN32
    (Right (ExitFailure 9009)) -> b -- command not found by cmd.exe on Windows
#endif
    (Right x) -> return x          -- legitimate success/failure
    (Left (_ :: SomeException)) -> b  -- an exception

editText :: String -> B.ByteString -> IO B.ByteString
editText desc txt = withNamedTemp desc $ \f -> do
  B.writeFile f txt
  _ <- runEditor f
  B.readFile f

-- | @editFile f@ lets the user edit a file which could but does not need to
-- already exist. This function returns the exit code from the text editor and a
-- flag indicating if the user made any changes.
editFile :: FilePathLike p
         => p
         -> IO (ExitCode, Bool)
editFile ff = do
    old_content <- file_content
    ec <- runEditor f
    new_content <- file_content
    return (ec, new_content /= old_content)
  where
    f = toFilePath ff
    file_content = do
      exists <- doesFileExist f
      if exists then do content <- B.readFile f
                        return $ Just content
                else return Nothing


runEditor :: FilePath
          -> IO ExitCode
runEditor f = do
    ed <- getEditor
    execInteractive ed f
         `ortryrunning` execInteractive "emacs" f
         `ortryrunning` execInteractive "emacs -nw" f
         `ortryrunning` execInteractive "nano" f
#ifdef WIN32
         `ortryrunning` execInteractive "edit" f
#endif


getEditor :: IO String
getEditor = getEnv "DARCS_EDITOR" `catchall`
             getEnv "DARCSEDITOR" `catchall`
             getEnv "VISUAL" `catchall`
             getEnv "EDITOR" `catchall` return "vi"

catchall :: IO a
         -> IO a
         -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)


-- | In some environments, darcs requires that certain global GHC library variables that
-- control the encoding used in internal translations are set to specific values.
--
-- @setDarcsEncoding@ enforces those settings, and should be called before the
-- first time any darcs operation is run, and again if anything else might have
-- set those encodings to different values.
--
-- Note that it isn't thread-safe and has a global effect on your program.
--
-- The current behaviour of this function is as follows, though this may
-- change in future:
--
-- Encodings are only set on GHC 7.4 and up, on any non-Windows platform.
--
-- Two encodings are set, both to @GHC.IO.Encoding.char8@:
-- @GHC.IO.Encoding.setFileSystemEncoding@ and @GHC.IO.Encoding.setForeignEncoding@.
--
setDarcsEncodings :: IO ()
setDarcsEncodings = do
#ifdef FORCE_CHAR8_ENCODING

-- This is needed for appropriate behaviour from getArgs and from general
-- filesystem calls (e.g. getDirectoryContents, readFile, ...)
    setFileSystemEncoding char8

-- This ensures that foreign calls made by hashed-storage to stat
-- filenames returned from getDirectoryContents are translated appropriately
    setForeignEncoding char8

#endif
    return ()

-- The following functions are copied from the encoding package (BSD3
-- licence, by Henning Günther).

-- | @getSystemEncoding@ fetches the current encoding from locale
foreign import ccall "system_encoding.h get_system_encoding"
     get_system_encoding :: IO CString


getSystemEncoding :: IO String
getSystemEncoding = do
    enc <- get_system_encoding
    peekCString enc


-- | @isUTF8@ checks if an encoding is UTF-8 (or ascii, since it is a
-- subset of UTF-8).
isUTF8Locale :: String -> Bool
isUTF8Locale codeName = case (normalizeEncoding codeName) of
    -- ASCII
    "ascii"              -> True
    "646"                -> True
    "ansi_x3_4_1968"     -> True
    "ansi_x3.4_1986"     -> True
    "cp367"              -> True
    "csascii"            -> True
    "ibm367"             -> True
    "iso646_us"          -> True
    "iso_646.irv_1991"   -> True
    "iso_ir_6"           -> True
    "us"                 -> True
    "us_ascii"           -> True
    -- UTF-8
    "utf_8"              -> True
    "u8"                 -> True
    "utf"                -> True
    "utf8"               -> True
    "utf8_ucs2"          -> True
    "utf8_ucs4"          -> True
    -- Everything else
    _                    -> False
  where
    normalizeEncoding s = map toLower $ subRegex sep s "_"
    sep = mkRegex "[^0-9A-Za-z]+"
