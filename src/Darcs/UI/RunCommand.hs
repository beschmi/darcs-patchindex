-- Copyright (C) 2002,2003,2005 David Roundy
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
module Darcs.UI.RunCommand ( runTheCommand ) where

import Control.Monad ( unless, when )
import System.Console.GetOpt( ArgOrder( Permute, RequireOrder ),
                              OptDescr( Option ),
                              getOpt )
import System.Exit ( ExitCode ( ExitSuccess ), exitWith )

import Darcs.UI.Arguments ( DarcsFlag(..),
                         help,
                         fixUrlFlag,
                         optionFromDarcsOption,
                         listOptions, nubOptions )
import Darcs.UI.ArgumentDefaults ( getDefaultFlags )
import Darcs.UI.Flags ( toMatchFlags, verbosity, getPrehookCmd, getPosthookCmd )
import Darcs.UI.Commands ( CommandArgs( CommandOnly, SuperCommandOnly, SuperCommandSub ),
                        CommandControl,
                        DarcsCommand,
                        commandName,
                        commandCommand,
                        commandPrereq,
                        commandExtraArgHelp,
                        commandExtraArgs,
                        commandArgdefaults,
                        commandGetArgPossibilities,
                        commandOptions, commandAlloptions,
                        disambiguateCommands,
                        getCommandHelp, getCommandMiniHelp,
                        getSubcommands,
                        extractCommands,
                        superName,
                        subusage, chompNewline
    , formatPath
    )
import Darcs.UI.Commands.GZCRCs ( doCRCWarnings )

import Darcs.Global ( atexit )
import Darcs.UI.External ( viewDoc )
import Darcs.Global ( setDebugMode, setTimingsMode )
import Darcs.Patch.Match ( checkMatchSyntax )
import Progress ( setProgressMode )
import Darcs.Path ( getCurrentDirectory )
import Darcs.Repository.Test ( runPosthook, runPrehook )
import Data.List ( intercalate )
import Printer ( text )
import URL ( setDebugHTTP, disableHTTPPipelining )

runTheCommand :: [CommandControl] -> String -> [String] -> IO ()
runTheCommand commandControlList cmd args =
  either fail rtc $ disambiguateCommands commandControlList cmd args
 where
  rtc (CommandOnly c, as)       = runCommand Nothing c  as
  rtc (SuperCommandOnly c,  as) = runRawSupercommand c as
  rtc (SuperCommandSub c s, as) = runCommand (Just c) s as

-- This is the actual heavy lifter code, which is responsible for parsing the
-- arguments and then running the command itself.

runCommand :: Maybe DarcsCommand -> DarcsCommand -> [String] -> IO ()

runCommand _ _ args -- Check for "dangerous" typoes...
    | "-all" `elem` args = -- -all indicates --all --look-for-adds!
        fail "Are you sure you didn't mean --all rather than -all?"
runCommand msuper cmd args = do
   cwd <- getCurrentDirectory
   let options = opts1 ++ opts2
       (opts1, opts2) = commandOptions cwd cmd
   case getOpt Permute
             (optionFromDarcsOption cwd listOptions++options) args of
    (opts,extra,[])
      | Help `elem` opts -> viewDoc $ text $ getCommandHelp msuper cmd
      | ListOptions `elem` opts  -> do
           setProgressMode False
           -- If we are only listing a command's options, we don't care if the
           -- prereq returns an error.
           _ <- commandPrereq cmd opts
           file_args <- commandGetArgPossibilities cmd
           putStrLn $ unlines $ getOptionsOptions (opts1++opts2) : file_args
      | otherwise -> considerRunning msuper cmd (addVerboseIfDebug opts) extra
    (_,_,ermsgs) -> fail $ chompNewline(unlines ermsgs)
    where addVerboseIfDebug opts | DebugVerbose `elem` opts = Debug:Verbose:opts
                                 | otherwise = opts

considerRunning :: Maybe DarcsCommand -> DarcsCommand
                 -> [DarcsFlag] -> [String] -> IO ()
considerRunning msuper cmd opts old_extra = do
 cwd <- getCurrentDirectory
 location <- commandPrereq cmd opts
 case location of
   Left complaint -> fail $ "Unable to " ++
                     formatPath ("darcs " ++ superName msuper ++ commandName cmd) ++
                     " here.\n\n" ++ complaint
   Right () -> do
    specops <- nubopts `fmap` addCommandDefaults cmd opts
    extra <- (commandArgdefaults cmd) specops cwd old_extra
    when (Disable `elem` specops) $
      fail $ "Command "++commandName cmd++" disabled with --disable option!"
    case extraArgumentsError extra cmd msuper of
        Nothing     -> runWithHooks specops extra
        Just msg    -> fail msg
       where nubopts = nubOptions (uncurry (++) $ commandAlloptions cmd)
             runWithHooks os ex = do
               here <- getCurrentDirectory
               checkMatchSyntax $ toMatchFlags os
               -- set any global variables
               when (Timings `elem` os) setTimingsMode
               when (Debug `elem` os) setDebugMode
               when (DebugHTTP `elem` os) setDebugHTTP
               when (Quiet `elem` os) $ setProgressMode False
               when (NoHTTPPipelining `elem` os) $ disableHTTPPipelining
               unless (Quiet `elem` os) $ atexit $ doCRCWarnings (Verbose `elem` os)
               -- actually run the command and its hooks
               preHookExitCode <- runPrehook (getPrehookCmd os) (AskPrehook `elem` os) (verbosity os) here
               if preHookExitCode /= ExitSuccess
                  then exitWith preHookExitCode
                  else do let fixFlag = FixFilePath here cwd
                          fixedOs <- mapM (fixUrlFlag [fixFlag]) os
                          (commandCommand cmd) (fixFlag : fixedOs) ex
                          postHookExitCode <- runPosthook (getPosthookCmd os) (AskPosthook `elem` os) (verbosity os) here
                          exitWith postHookExitCode

-- Checks if the number of extra arguments matches the number of extra
-- arguments supported by the command as specified in `commandExtraArgs`.
-- Extra arguments are arguments that follow the command but aren't
-- considered a flag. In `darcs push xyz`, xyz would be an extra argument.
extraArgumentsError :: [String]             -- extra commands provided by user
                    -> DarcsCommand
                    -> Maybe DarcsCommand
                    -> Maybe String
extraArgumentsError extra cmd msuper
    | extraArgsCmd < 0 = Nothing
    | extraArgsInput > extraArgsCmd = Just badArg
    | extraArgsInput < extraArgsCmd = Just missingArg
    | otherwise = Nothing
        where
            extraArgsInput = length extra
            extraArgsCmd = commandExtraArgs cmd
            badArg     = "Bad argument: `" ++ unwords extra ++
                         "'\n" ++ getCommandMiniHelp msuper cmd
            missingArg = "Missing argument:  " ++ nthArg (length extra + 1) ++
                         "\n" ++ getCommandMiniHelp msuper cmd
            nthArg n       = nthOf n (commandExtraArgHelp cmd)
            nthOf 1 (h:_)  = h
            nthOf n (_:hs) = nthOf (n-1) hs
            nthOf _ []     = "UNDOCUMENTED"

addCommandDefaults :: DarcsCommand -> [DarcsFlag] -> IO [DarcsFlag]
addCommandDefaults cmd already = do
  let (opts1, opts2) = commandAlloptions cmd
  defaults <- getDefaultFlags (commandName cmd) (opts1 ++ opts2) already
  return $ already ++ defaults

getOptionsOptions :: [OptDescr DarcsFlag] -> String
getOptionsOptions = intercalate "\n" . concatMap goo
 where
  goo (Option _ os _ _) = map ("--"++) os

runRawSupercommand :: DarcsCommand -> [String] -> IO ()
runRawSupercommand super [] =
    fail $ "Command '"++ commandName super ++"' requires subcommand!\n\n"
             ++ subusage super
runRawSupercommand super args = do
  cwd <- getCurrentDirectory
  case getOpt RequireOrder
             (optionFromDarcsOption cwd help++
              optionFromDarcsOption cwd listOptions) args of
    (opts,_,[])
      | Help `elem` opts ->
            viewDoc $ text $ getCommandHelp Nothing super
      | ListOptions `elem` opts -> do
            putStrLn "--help"
            mapM_ (putStrLn . commandName) (extractCommands $ getSubcommands super)
      | otherwise ->
            if Disable `elem` opts
            then fail $ "Command " ++ (commandName super) ++
                      " disabled with --disable option!"
            else fail $ "Invalid subcommand!\n\n" ++ subusage super
    (_,_,ermsgs) -> fail $ chompNewline(unlines ermsgs)
