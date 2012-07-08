--  Copyright (C) 2002,2003,2005 David Roundy
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

module Darcs.UI.Commands ( CommandControl( CommandData, HiddenCommand, GroupName ),
                       DarcsCommand( DarcsCommand, commandProgramName, commandName,
                                     commandHelp, commandDescription,
                                     commandBasicOptions, commandAdvancedOptions,
                                     commandCommand,
                                     commandPrereq,
                                     commandExtraArgHelp,
                                     commandExtraArgs,
                                     commandArgdefaults,
                                     commandGetArgPossibilities,
                                     SuperCommand,
                                     commandSubCommands ),
                       commandAlias, commandStub,
                       commandOptions, commandAlloptions,
                       disambiguateCommands, CommandArgs(..),
                       getCommandHelp, getCommandMiniHelp,
                       getSubcommands,
                       usage, usageHelper, subusage, chompNewline,
                       extractCommands,
                       superName,
                       nodefaults,
                       putInfo, putVerbose, putWarning, abortRun
 , printDryRunMessageAndExit
 , setEnvDarcsPatches
 , setEnvDarcsFiles
 , formatPath
 , defaultRepo
 , amInHashedRepository
 , amInRepository
 , amNotInRepository
 , findRepository
 ) where

import System.Console.GetOpt( OptDescr )
import System.IO ( stderr )
import System.Exit ( ExitCode(ExitSuccess), exitWith )
import Control.Monad (when, unless)
import Data.List ( sort, isPrefixOf )

import Storage.Hashed.Tree( Tree )

import Darcs.UI.Usage ( usageInfo )
import Darcs.UI.PrintPatch ( showFriendly )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, hopefullyM )
import Darcs.Patch ( RepoPatch, xmlSummary, Patchy  )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Info ( toXml )
import Darcs.Patch.Apply( ApplyState )
#ifndef WIN32
import qualified Darcs.Patch ( summary )
#endif

import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )
import Darcs.UI.Arguments
    ( DarcsFlag( All
               , Summary
               , Quiet
               , Verbose
               , DryRun
               , XMLOutput
               )
    , DarcsOption
    , disable
    , help
    , anyVerbosity
    , noCache
    , posthookCmd
    , posthookPrompt
    , prehookCmd
    , prehookPrompt
    , optionFromDarcsOption
    )
import Darcs.Path ( AbsolutePath, rootDirectory )

import Printer
    ( Doc
    , putDocLn
    , hPutDocLn
    , text
    , (<+>)
    , errorDoc
    , vsep
    , insertBeforeLastline
    , prefix
    , ($$)
    , vcat
    )

#ifndef WIN32
import Printer ( renderString )
import System.Posix.Env ( setEnv )
import Darcs.Patch ( listTouchedFiles )
import Progress ( beginTedious, endTedious, tediousSize, finishedOneIO )
#endif

import Darcs.UI.Flags ( remoteRepos, workRepo )
import Darcs.Repository.Prefs ( defaultrepo )
import qualified Darcs.Repository as R
    ( amInHashedRepository, amInRepository, amNotInRepository, findRepository )

extractCommands, extractHiddenCommands :: [CommandControl] -> [DarcsCommand]
extractCommands cs = concatMap (\x -> case x of { CommandData cmd_d -> [cmd_d]; _ -> []}) cs
extractHiddenCommands cs = concatMap (\x -> case x of { HiddenCommand cmd_d -> [cmd_d]; _ -> []}) cs

data CommandControl = CommandData DarcsCommand
                    | HiddenCommand DarcsCommand
                    | GroupName String

data DarcsCommand =
    DarcsCommand {commandProgramName, commandName, commandHelp, commandDescription :: String,
                  commandExtraArgs :: Int,
                  commandExtraArgHelp :: [String],
                  commandCommand :: [DarcsFlag] -> [String] -> IO (),
                  commandPrereq :: [DarcsFlag] -> IO (Either String ()),
                  commandGetArgPossibilities :: IO [String],
                  commandArgdefaults :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String],
                  commandBasicOptions :: [DarcsOption],
                  commandAdvancedOptions :: [DarcsOption]}
  | SuperCommand {commandProgramName, commandName, commandHelp, commandDescription :: String,
                  commandPrereq :: [DarcsFlag] -> IO (Either String ()),
                  commandSubCommands :: [CommandControl]}

commandAlloptions :: DarcsCommand -> ([DarcsOption], [DarcsOption])
commandAlloptions DarcsCommand { commandBasicOptions = opts1
                                , commandAdvancedOptions = opts2 }
    = (opts1 ++ [disable, help],
       anyVerbosity ++ opts2 ++
                [noCache
                ,posthookCmd, posthookPrompt
                ,prehookCmd, prehookPrompt])

--  Supercommands cannot be disabled.
commandAlloptions SuperCommand { } = ([help],[])

--  Obtain options suitable as input to
--  System.Console.Getopt, including the --disable option (which is
--  not listed explicitly in the DarcsCommand definitions).
commandOptions :: AbsolutePath -> DarcsCommand -> ([OptDescr DarcsFlag], [OptDescr DarcsFlag])
commandOptions cwd c = (convert basic, convert advanced)
 where (basic, advanced) = commandAlloptions c
       convert = concatMap (optionFromDarcsOption cwd)

nodefaults :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
nodefaults _ _ xs = return xs

getSubcommands :: DarcsCommand -> [CommandControl]
getSubcommands c@(SuperCommand {}) = commandSubCommands c
getSubcommands _ = []

commandAlias :: String -> Maybe DarcsCommand -> DarcsCommand -> DarcsCommand
commandAlias n msuper c =
  c { commandName = n
    , commandDescription = "Alias for `" ++ commandProgramName c ++ " " ++ cmdName ++ "'."
    , commandHelp = "The `" ++ commandProgramName c ++ " " ++ n ++ "' command is an alias for " ++
                     "`" ++ commandProgramName c ++ " " ++ cmdName ++ "'.\n" ++
                     commandHelp c
    }
 where
  cmdName = unwords . map commandName . maybe id (:) msuper $ [ c ]

commandStub :: String -> String -> String -> DarcsCommand -> DarcsCommand
commandStub n h d c =
  c { commandName = n
    , commandHelp = h
    , commandDescription = d
    , commandCommand = \_ _ -> putStr h
    }

usage :: [CommandControl] -> String
usage cs = "Usage: darcs COMMAND ...\n\nCommands:\n" ++
           usageHelper cs ++ "\n" ++
           "Use 'darcs COMMAND --help' for help on a single command.\n" ++
           "Use 'darcs --version' to see the darcs version number.\n" ++
           "Use 'darcs --exact-version' to get the exact version of this darcs instance.\n" ++
           "Use 'darcs help patterns' for help on patch matching.\n" ++
           "Use 'darcs help environment' for help on environment variables.\n" ++
           "\n" ++
           "Check bug reports at http://bugs.darcs.net/\n"

subusage :: DarcsCommand -> String
subusage super =
    (usageInfo
     ("Usage: " ++ commandProgramName super ++ " "++commandName super++" SUBCOMMAND ... " ++
      "\n\n"++ commandDescription super++
      "\n\nSubcommands:\n" ++ usageHelper (getSubcommands super) ++ "\nOptions:")
     (optionFromDarcsOption rootDirectory help))
    ++ "\n" ++ commandHelp super

usageHelper :: [CommandControl] -> String
usageHelper [] = ""
usageHelper (HiddenCommand _:cs) = usageHelper cs
usageHelper ((CommandData c):cs) = "  "++padSpaces (commandName c) 15 ++
                      chompNewline (commandDescription c)++"\n"++usageHelper cs
usageHelper ((GroupName n):cs) = "\n" ++ n ++ "\n" ++ usageHelper cs

chompNewline :: String -> String
chompNewline "" = ""
chompNewline s = if last s == '\n' then init s else s

padSpaces :: String -> Int -> String
padSpaces s n = s ++ replicate (n - length s) ' '

superName :: Maybe DarcsCommand -> String
superName Nothing  = ""
superName (Just x) = commandName x ++ " "

getCommandMiniHelp :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandMiniHelp msuper cmd =
  getCommandHelpCore msuper cmd ++
  "\n\nSee " ++ commandProgramName cmd ++ " help "
  ++ (maybe "" (\c -> commandName c ++ " ") msuper)
  ++ commandName cmd ++ " for details."

getCommandHelp :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandHelp msuper cmd =
    unlines (reverse basicR)
    ++ (if null advanced then ""
        else "\nAdvanced options:\n" ++ unlines (reverse advancedR))
    ++ "\n" ++ commandHelp cmd
    where -- we could just call usageInfo twice, but then the advanced
          -- options might not line up with the basic ones (no short flags)
          (advancedR, basicR) =
             splitAt (length advanced) $ reverse $ lines combinedUsage
          combinedUsage = usageInfo
            (getCommandHelpCore msuper cmd ++ subcommands ++ "\n\nOptions:")
            (basic ++ advanced)
          (basic, advanced) = commandOptions rootDirectory cmd
          subcommands =
            case msuper of
            Nothing -> case getSubcommands cmd of
                       [] -> []
                       s  -> "\n\nSubcommands:\n" ++ (usageHelper s)
            -- we don't want to list subcommands if we're already specifying them
            Just _  -> ""

getCommandHelpCore :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandHelpCore msuper cmd =
    "Usage: " ++ commandProgramName cmd ++ " "++superName msuper++commandName cmd++
    " [OPTION]... " ++ unwords args_help ++
    "\n"++ commandDescription cmd
    where args_help = case cmd of
            (DarcsCommand {}) ->
              commandExtraArgHelp cmd
            _ -> []

data CommandArgs = CommandOnly      DarcsCommand
                 | SuperCommandOnly DarcsCommand
                 | SuperCommandSub  DarcsCommand DarcsCommand

-- Parses a darcs command line with potentially abbreviated commands
disambiguateCommands :: [CommandControl] -> String -> [String]
                      -> Either String (CommandArgs, [String])
disambiguateCommands allcs cmd args =
 do c <- extract cmd allcs
    case (getSubcommands c, args) of
      ([], _)         -> return (CommandOnly c, args)
      (_ ,[])         -> return (SuperCommandOnly c, args)
      (subcs, (a:as)) -> case extract a subcs of
                         Left _   -> return (SuperCommandOnly c, args)
                         Right sc -> return (SuperCommandSub c sc, as)

extract :: String -> [CommandControl] -> Either String DarcsCommand
extract cmd cs =
 case [ c | c <- extractCommands cs, cmd `isPrefixOf` commandName c ] ++
      [ h | h <- extractHiddenCommands cs,    cmd == commandName h ] of
   []  -> Left $ "No such command '" ++ cmd ++ "'\n"
   [c] -> Right c
   cs' -> Left $ "Ambiguous command...\n\n" ++
                    "The command '"++cmd++"' could mean one of:\n" ++
                    unwords (sort $ map commandName cs')

amVerbose :: [DarcsFlag] -> Bool
amVerbose = elem Verbose

amQuiet :: [DarcsFlag] -> Bool
amQuiet = elem Quiet

putVerbose :: [DarcsFlag] -> Doc -> IO ()
putVerbose opts = when (amVerbose opts) . putDocLn

putInfo :: [DarcsFlag] -> Doc -> IO ()
putInfo opts = unless (amQuiet opts) . putDocLn

putWarning :: [DarcsFlag] -> Doc -> IO ()
putWarning opts = unless (amQuiet opts) . hPutDocLn stderr

abortRun :: [DarcsFlag] -> Doc -> IO ()
abortRun opts msg = if DryRun `elem` opts
                    then putInfo opts $ text "NOTE:" <+> msg
                    else errorDoc msg

-- | @'printDryRunMessageAndExit' action opts patches@ prints a string
-- representing the action that would be taken if the @--dry-run@ option
-- had not been passed to darcs. Then darcs exits successfully.
-- @action@ is the name of the action being taken, like @\"push\"@
-- @opts@ is the list of flags which were sent to darcs
-- @patches@ is the sequence of patches which would be touched by @action@.
printDryRunMessageAndExit :: (RepoPatch p, ApplyState p ~ Tree)
                          => String
                          -> [DarcsFlag]
                          -> FL (PatchInfoAnd p) wX wY
                          -> IO ()
printDryRunMessageAndExit action opts patches =
     do when (DryRun `elem` opts) $ do
          putInfoX $ text $ "Would " ++ action ++ " the following changes:"
          putDocLn $ put_mode
          putInfoX $ text $ ""
          putInfoX $ text $ "Making no changes:  this is a dry run."
          exitWith ExitSuccess
        when (All `elem` opts && Summary `elem` opts) $ do
          putInfoX $ text $ "Will " ++ action ++ " the following changes:"
          putDocLn $ put_mode
     where put_mode = if XMLOutput `elem` opts
                      then (text "<patches>" $$
                            vcat (mapFL (indent . xml_info) patches) $$
                            text "</patches>")
                      else (vsep $ mapFL (showFriendly opts) patches)
           putInfoX = if XMLOutput `elem` opts then \_ -> return () else putDocLn
           xml_info pl
              | Summary `elem` opts = xml_with_summary pl
              | otherwise = (toXml . info) pl

           xml_with_summary hp
               | Just p <- hopefullyM hp = insertBeforeLastline
                                            (toXml $ info hp) (indent $ xmlSummary p)
           xml_with_summary hp = toXml (info hp)
           indent = prefix "    "

-- | Set the DARCS_PATCHES and DARCS_PATCHES_XML environment variables
-- with info about the given patches, for use in post-hooks.
setEnvDarcsPatches :: (RepoPatch p, ApplyState p ~ Tree) => FL (PatchInfoAnd p) wX wY -> IO ()
#ifndef WIN32
setEnvDarcsPatches ps = do
  let k = "Defining set of chosen patches"
  beginTedious k
  tediousSize k 3
  finishedOneIO k "DARCS_PATCHES"
  setEnvCautiously "DARCS_PATCHES" (renderString $ Darcs.Patch.summary ps)
  finishedOneIO k "DARCS_PATCHES_XML"
  setEnvCautiously "DARCS_PATCHES_XML"
                       (renderString $ text "<patches>" $$
                        vcat (mapFL (toXml . info) ps) $$
                        text "</patches>")
  finishedOneIO k "DARCS_FILES"
  setEnvCautiously "DARCS_FILES" (unlines$ listTouchedFiles ps)
  endTedious k

-- | Set some environment variable to the given value, unless said value
-- is longer than 10K characters, in which case do nothing.
setEnvCautiously :: String -> String -> IO ()
setEnvCautiously e v | toobig (10*1024) v = return ()
                     | otherwise = setEnv e v True
    where toobig :: Int -> [a] -> Bool
          toobig 0 _ = True
          toobig _ [] = False
          toobig n (_:xs) = toobig (n-1) xs
#else
setEnvDarcsPatches _ = return ()
#endif

-- | Set the DARCS_FILES environment variable to the files touched by the
-- given patch, one per line, for use in post-hooks.
setEnvDarcsFiles :: (PatchInspect p, Patchy p) => p wX wY -> IO ()
#ifndef WIN32
setEnvDarcsFiles ps = setEnvCautiously "DARCS_FILES" (unlines $ listTouchedFiles ps)
#else
setEnvDarcsFiles _ = return ()
#endif

-- | Format a path for screen output, so that the user sees where the path
-- begins and ends. Could (should?) also warn about unprintable characters here.
formatPath :: String
           -> String
formatPath path = "\"" ++ quote path ++ "\""
    where quote "" = ""
          quote (c:cs) = if c `elem` ['\\', '"']
                         then '\\':c:quote cs
                         else c:quote cs

defaultRepo :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
defaultRepo fs ap ss = defaultrepo (remoteRepos fs) ap ss

amInHashedRepository :: [DarcsFlag] -> IO (Either String ())
amInHashedRepository fs = R.amInHashedRepository (workRepo fs)

amInRepository :: [DarcsFlag] -> IO (Either String ())
amInRepository fs = R.amInRepository (workRepo fs)

amNotInRepository :: [DarcsFlag] -> IO (Either String ())
amNotInRepository fs = R.amNotInRepository (workRepo fs)

findRepository :: [DarcsFlag] -> IO (Either String ())
findRepository fs = R.findRepository (workRepo fs)
