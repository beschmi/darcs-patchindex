{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Darcs.Ssh
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Ssh
    (
      copySSH
    , SSHCmd(..)
    , getSSH
    , environmentHelpSsh
    , environmentHelpScp
    , environmentHelpSshPort
    , remoteDarcs
  ) where


import Prelude hiding ( lookup, catch )

import qualified Ratified ( hGetContents )

import System.Environment ( getEnv )
import System.Exit ( ExitCode(..) )

import Control.Monad ( when )

import qualified Data.ByteString as B (ByteString, hGet, writeFile )

import Data.Map ( Map, empty, insert, lookup )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )

import System.IO ( Handle, hSetBinaryMode, hPutStrLn, hGetLine, hFlush )
import System.IO.Unsafe ( unsafePerformIO )
import System.Process ( runInteractiveProcess )

import Darcs.SignalHandler ( catchNonSignal )
import Darcs.Repository.Flags( RemoteDarcs(..) )
import Darcs.Global ( defaultSsh, SshSettings)
import Darcs.URL (SshFilePath(..), urlOf)
import Darcs.Utils ( breakCommand, prettyException, catchall )
import Exec ( exec, Redirect(..), )
import Progress ( withoutProgress, debugMessage, debugFail )

import qualified Darcs.Global as Settings


sshConnections :: IORef (Map String (Maybe Connection))
sshConnections = unsafePerformIO $ newIORef empty
{-# NOINLINE sshConnections #-}


data Connection = C
    { inp :: !Handle
    , out :: !Handle
    , err :: !Handle
    , deb :: String -> IO ()
    }


-- | @withSSHConnection rdarcs repoid  withconnection withoutconnection@
-- performs an action on a remote host. If we are already connected to @repoid @,
-- then it does @withconnection@, else @withoutconnection@.
withSSHConnection :: String                 -- ^ rdarcs
                  -> SshFilePath            -- ^ Destination repo id
                  -> (Connection -> IO a)   -- ^ withconnection
                  -> IO a                   -- ^ withoutconnection
                  -> IO a
withSSHConnection rdarcs repoid withconnection withoutconnection =
    withoutProgress $
    do cs <- readIORef sshConnections
       case lookup (sshUhost repoid) (cs :: Map String (Maybe Connection)) of
         Just Nothing -> withoutconnection
         Just (Just c) -> withconnection c
         Nothing ->
           do mc <- do (ssh,sshargs_) <- getSSH SSH
                       let sshargs = sshargs_ ++ [sshUhost repoid, rdarcs,
                                                  "transfer-mode","--repodir",sshRepo repoid]
                       debugMessage $ unwords (ssh : sshargs)
                       (i,o,e,_) <- runInteractiveProcess ssh sshargs Nothing Nothing
                       hSetBinaryMode i True
                       hSetBinaryMode o True
                       l <- hGetLine o
                       if l == "Hello user, I am darcs transfer mode"
                           then return ()
                           else debugFail "Couldn't start darcs transfer-mode on server"
                       let c = C { inp = i, out = o, err = e,
                                   deb = \s -> debugMessage ("with ssh (transfer-mode) " ++ sshUhost repoid ++ " " ++ s) }
                       modifyIORef sshConnections (insert (sshUhost repoid) (Just c))
                       return $ Just c
                    `catchNonSignal`
                            \e -> do debugMessage $ "Failed to start ssh connection:\n    "++
                                                    prettyException e
                                     severSSHConnection repoid
                                     debugMessage $ unlines $
                                         [ "NOTE: the server may be running a version of darcs prior to 2.0.0."
                                         , ""
                                         , "Installing darcs 2 on the server will speed up ssh-based commands."
                                         ]
                                     return Nothing
              maybe withoutconnection withconnection mc

severSSHConnection :: SshFilePath
                   -> IO ()
severSSHConnection x = do
    debugMessage $ "Severing ssh failed connection to " ++ (sshUhost x)
    modifyIORef sshConnections (insert (urlOf x) Nothing)


grabSSH :: SshFilePath -> Connection -> IO B.ByteString
grabSSH dest c = do
  debugMessage $ "grabSSH dest=" ++ urlOf dest
  let failwith e = do severSSHConnection dest
                        -- hGetContents is ok here because we're
                        -- only grabbing stderr, and we're also
                        -- about to throw the contents.
                      eee <- Ratified.hGetContents (err c)
                      debugFail $ e ++ " grabbing ssh file "++
                        urlOf dest++"/"++ file ++"\n"++eee
      file = sshFile dest
  deb c $ "get "++ file
  hPutStrLn (inp c) $ "get " ++ file
  hFlush (inp c)
  l2 <- hGetLine (out c)
  if l2 == "got "++file
    then do showlen <- hGetLine (out c)
            case reads showlen of
              [(len,"")] -> B.hGet (out c) len
              _ -> failwith "Couldn't get length"
    else if l2 == "error "++file
         then do e <- hGetLine (out c)
                 case reads e of
                   (msg,_):_ -> debugFail $ "Error reading file remotely:\n"++msg
                   [] -> failwith "An error occurred"
         else failwith "Error"

remoteDarcs :: RemoteDarcs
            -> String
remoteDarcs DefaultRemoteDarcs = "darcs"
remoteDarcs (RemoteDarcs x) = x


copySSH :: RemoteDarcs
        -> SshFilePath
        -> FilePath
        -> IO ()
copySSH remote dest to | rdarcs <- remoteDarcs remote = do
    debugMessage $ "copySSH file: " ++ urlOf dest
    withSSHConnection rdarcs dest (\c -> grabSSH dest c >>= B.writeFile to) $ do
        let u = escape_dollar $ urlOf dest
        (scp, args) <- getSSH SCP
        r <- exec scp (args ++ [u, to]) (AsIs,AsIs,AsIs)
        when (r /= ExitSuccess) $
            debugFail $ "(scp) failed to fetch: " ++ u
  where
    -- '$' in filenames is troublesome for scp, for some reason.
    escape_dollar :: String -> String
    escape_dollar = concatMap tr
      where
        tr '$' = "\\$"
        tr c = [c]


-- ---------------------------------------------------------------------
-- older ssh helper functions
-- ---------------------------------------------------------------------

data SSHCmd = SSH
            | SCP
            | SFTP


fromSshCmd :: SshSettings
           -> SSHCmd
           -> String
fromSshCmd s SSH  = Settings.ssh s
fromSshCmd s SCP  = Settings.scp s
fromSshCmd s SFTP = Settings.sftp s


-- | Return the command and arguments needed to run an ssh command
--   First try the appropriate darcs environment variable and SSH_PORT
--   defaulting to "ssh" and no specified port.
getSSH :: SSHCmd
       -> IO (String, [String])
getSSH cmd = do
    port <- (portFlag cmd `fmap` getEnv "SSH_PORT") `catchall` return []
    let (ssh, ssh_args) = breakCommand command
    return (ssh, ssh_args ++ port)
  where
    command = fromSshCmd defaultSsh cmd
    portFlag SSH  x = ["-p", x]
    portFlag SCP  x = ["-P", x]
    portFlag SFTP x = ["-oPort=" ++ x]


environmentHelpSsh :: ([String], [String])
environmentHelpSsh = (["DARCS_SSH"], [
    "Repositories of the form [user@]host:[dir] are taken to be remote",
    "repositories, which Darcs accesses with the external program ssh(1).",
    "",
    "The environment variable $DARCS_SSH can be used to specify an",
    "alternative SSH client.  Arguments may be included, separated by",
    "whitespace.  The value is not interpreted by a shell, so shell",
    "constructs cannot be used; in particular, it is not possible for the",
    "program name to contain whitespace by using quoting or escaping."])


environmentHelpScp :: ([String], [String])
environmentHelpScp = (["DARCS_SCP", "DARCS_SFTP"], [
    "When reading from a remote repository, Darcs will attempt to run",
    "`darcs transfer-mode' on the remote host.  This will fail if the",
    "remote host only has Darcs 1 installed, doesn't have Darcs installed",
    "at all, or only allows SFTP.",
    "",
    "If transfer-mode fails, Darcs will fall back on scp(1) and sftp(1).",
    "The commands invoked can be customized with the environment variables",
    "$DARCS_SCP and $DARCS_SFTP respectively, which behave like $DARCS_SSH.",
    "If the remote end allows only sftp, try setting DARCS_SCP=sftp."])


environmentHelpSshPort :: ([String], [String])
environmentHelpSshPort = (["SSH_PORT"], [
    "If this environment variable is set, it will be used as the port",
    "number for all SSH calls made by Darcs (when accessing remote",
    "repositories over SSH).  This is useful if your SSH server does not",
    "run on the default port, and your SSH client does not support",
    "ssh_config(5).  OpenSSH users will probably prefer to put something",
    "like `Host *.example.net Port 443' into their ~/.ssh/config file."])
