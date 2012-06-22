--  Copyright (C) 2002-2004 David Roundy
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

module Darcs.UI.Commands.Help (
 helpCmd,
 commandControlList, environmentHelp,          -- these are for preproc.hs
 printVersion,
 listAvailableCommands ) where

import Darcs.UI.Arguments
    ( DarcsFlag(..)
    , environmentHelpEmail
    , environmentHelpSendmail
    )
import Darcs.UI.Commands
    ( CommandArgs(..)
    , CommandControl(..)
    , DarcsCommand(..)
    , disambiguateCommands
    , extractCommands
    , getCommandHelp
    , nodefaults
    , usage
    )
import Darcs.UI.External ( viewDoc )
import Darcs.Repository.Lock ( environmentHelpTmpdir, environmentHelpKeepTmpdir )
import Darcs.Patch.Match ( helpOnMatchers )
import Darcs.Repository.Prefs ( boringFileHelp, binariesFileHelp, environmentHelpHome )
import Darcs.Ssh ( environmentHelpSsh, environmentHelpScp, environmentHelpSshPort )
import Darcs.Utils ( withCurrentDirectory )
import Data.Char ( isAlphaNum, toLower, toUpper )
import Data.List ( groupBy, isPrefixOf, intercalate, nub )
import English ( andClauses )
import Printer (text, vcat, vsep, ($$))
import System.Exit ( ExitCode(..), exitWith )
import Version ( version )
import URL (environmentHelpProxy, environmentHelpProxyPassword)
import Workaround ( getCurrentDirectory )
import qualified Darcs.UI.TheCommands as TheCommands

helpDescription :: String
helpDescription = "Display help about darcs and darcs commands."

helpHelp :: String
helpHelp =
 "Without arguments, `darcs help' prints a categorized list of darcs\n" ++
 "commands and a short description of each one.  With an extra argument,\n" ++
 "`darcs help foo' prints detailed help about the darcs command foo.\n"

help :: DarcsCommand
help = DarcsCommand {commandProgramName = "darcs",
                     commandName = "help",
                     commandHelp = helpHelp,
                     commandDescription = helpDescription,
                     commandExtraArgs = -1,
                     commandExtraArgHelp = ["[<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  "],
                     commandCommand = \ x y -> helpCmd x y >> exitWith ExitSuccess,
                     commandPrereq = \_ -> return $ Right (),
                     commandGetArgPossibilities = return [],
                     commandArgdefaults = nodefaults,
                     commandAdvancedOptions = [],
                     commandBasicOptions = []}

helpCmd :: [DarcsFlag] -> [String] -> IO ()
helpCmd _ ["manpage"] = putStr $ unlines manpageLines
helpCmd _ ["patterns"] = viewDoc $ text $ helpOnMatchers
helpCmd _ ("environment":vs_) =
    viewDoc . vsep $
       if null vs
          then header : map render environmentHelp
          else          map render known ++ [footer]
    where header = vcat [ text "Environment Variables"
                        , text "====================="
                        ]
          footer = text ("Unknown environment variables: " ++ intercalate ", " unknown)
          render (ks, ds) = text (andClauses ks ++ ":") $$
                            vcat [text ("  " ++ d) | d <- ds]
          lookupEnv v = [ e | e@(ks,_) <- environmentHelp, v `elem` ks ]
          unknown = [ v | v <- vs, null (lookupEnv v) ]
          known   = nub (concatMap lookupEnv vs)
          vs      = map (map toUpper) vs_

helpCmd _ [] = viewDoc $ text $ usage commandControlList

helpCmd _ (cmd:args) =
    let disambiguated = disambiguateCommands commandControlList cmd args
    in case disambiguated of
         Left err -> fail err
         Right (cmds,_) ->
             let msg = case cmds of
                         CommandOnly c       -> getCommandHelp Nothing  c
                         SuperCommandOnly c  -> getCommandHelp Nothing  c
                         SuperCommandSub c s -> getCommandHelp (Just c) s
             in viewDoc $ text msg

listAvailableCommands :: IO ()
listAvailableCommands =
    do here <- getCurrentDirectory
       is_valid <- mapM
                   (\c-> withCurrentDirectory here $ (commandPrereq c) [])
                   (extractCommands commandControlList)
       putStr $ unlines $ map (commandName . fst) $
                filter (isRight.snd) $
                zip (extractCommands commandControlList) is_valid
       putStrLn "--help"
       putStrLn "--version"
       putStrLn "--exact-version"
       putStrLn "--overview"
    where isRight (Right _) = True
          isRight _ = False

printVersion :: IO ()
printVersion = putStrLn $ "darcs version " ++ version

-- avoiding a module import cycle between Help and TheCommands
commandControlList :: [CommandControl]
commandControlList =
  CommandData help : TheCommands.commandControlList

-- FIXME: the "grouping" comments below should made subsections in the
-- manpage, as we already do for DarcsCommand groups. --twb, 2009

-- | Help on each environment variable in which Darcs is interested.
environmentHelp :: [([String], [String])]
environmentHelp = [
 -- General-purpose
 environmentHelpHome,
 environmentHelpEditor,
 environmentHelpPager,
 environmentHelpTmpdir,
 environmentHelpKeepTmpdir,
 environmentHelpEmail,
 environmentHelpSendmail,
 -- Remote Repositories
 environmentHelpSsh,
 environmentHelpScp,
 environmentHelpSshPort,
 environmentHelpProxy,
 environmentHelpProxyPassword]

-- | This module is responsible for emitting a darcs "man-page", a
-- reference document used widely on Unix-like systems.  Manpages are
-- primarily used as a quick reference, or "memory jogger", so the
-- output should be terser than the user manual.
--
-- Before modifying the output, please be sure to read the man(7) and
-- man-pages(7) manpages, as these respectively describe the relevant
-- syntax and conventions.

-- | The lines of the manpage to be printed.
manpageLines :: [String]
manpageLines = [
 ".TH DARCS 1 \"" ++ version ++ "\"",
 ".SH NAME",
 "darcs \\- an advanced revision control system",
 ".SH SYNOPSIS",
 ".B darcs", ".I command", ".RI < arguments |[ options ]>...",
 "",
 "Where the", ".I commands", "and their respective", ".I arguments", "are",
 "",
 unlines synopsis,
 ".SH DESCRIPTION",
 -- FIXME: this is copy-and-pasted from darcs.cabal, so
 -- it'll get out of date as people forget to maintain
 -- both in sync.
 "Darcs is a free, open source revision control",
 "system. It is:",
 ".TP 3", "\\(bu",
 "Distributed: Every user has access to the full",
 "command set, removing boundaries between server and",
 "client or committer and non\\(hycommitters.",
 ".TP", "\\(bu",
 "Interactive: Darcs is easy to learn and efficient to",
 "use because it asks you questions in response to",
 "simple commands, giving you choices in your work",
 "flow. You can choose to record one change in a file,",
 "while ignoring another. As you update from upstream,",
 "you can review each patch name, even the full `diff'",
 "for interesting patches.",
 ".TP", "\\(bu",
 "Smart: Originally developed by physicist David",
 "Roundy, darcs is based on a unique algebra of",
 "patches.",
 "This smartness lets you respond to changing demands",
 "in ways that would otherwise not be possible. Learn",
 "more about spontaneous branches with darcs.",
 ".SH OPTIONS",
 "Different options are accepted by different Darcs commands.",
 "Each command's most important options are listed in the",
 ".B COMMANDS",
 "section.  For a full list of all options accepted by",
 "a particular command, run `darcs", ".I command", "\\-\\-help'.",
 ".SS " ++ escape helpOnMatchers, -- FIXME: this is a kludge.
 ".SH COMMANDS",
 unlines commands,
 unlines environment,
 ".SH FILES",
 ".SS \"_darcs/prefs/binaries\"",
 escape $ unlines binariesFileHelp,
 ".SS \"_darcs/prefs/boring\"",
 escape $ unlines boringFileHelp,
 ".SH BUGS",
 "At http://bugs.darcs.net/ you can find a list of known",
 "bugs in Darcs.  Unknown bugs can be reported at that",
 "site (after creating an account) or by emailing the",
 "report to bugs@darcs.net.",
 -- ".SH EXAMPLE",
 -- FIXME:
 -- new project: init, rec -la;
 -- track upstream project: get, pull -a;
 -- contribute to project: add, rec, push/send.
 ".SH SEE ALSO",
 "A user manual is included with Darcs, in PDF and HTML",
 "form.  It can also be found at http://darcs.net/manual/.",
 ".SH LICENSE",
 "Darcs is free software; you can redistribute it and/or modify",
 "it under the terms of the GNU General Public License as published by",
 "the Free Software Foundation; either version 2, or (at your option)",
 "any later version." ]
    where
      -- | A synopsis line for each command.  Uses 'foldl' because it is
      -- necessary to avoid blank lines from Hidden_commands, as groff
      -- translates them into annoying vertical padding (unlike TeX).
      synopsis :: [String]
      synopsis = foldl iter [] commandControlList
          where iter :: [String] -> CommandControl -> [String]
                iter acc (GroupName _) = acc
                iter acc (HiddenCommand _) = acc
                iter acc (CommandData c@SuperCommand {}) =
                    acc ++ concatMap
                            (render (commandName c ++ " "))
                            (extractCommands (commandSubCommands c))
                iter acc (CommandData c) = acc ++ render "" c
                render :: String -> DarcsCommand -> [String]
                render prefix c =
                    [".B darcs " ++ prefix ++ commandName c] ++
                    (map mangle_args $ commandExtraArgHelp c) ++
                    -- In the output, we want each command to be on its own
                    -- line, but we don't want blank lines between them.
                    -- AFAICT this can only be achieved with the .br
                    -- directive, which is probably a GNUism.
                    [".br"]

      -- | As 'synopsis', but make each group a subsection (.SS), and
      -- include the help text for each command.
      commands :: [String]
      commands = foldl iter [] commandControlList
          where iter :: [String] -> CommandControl -> [String]
                iter acc (GroupName x) = acc ++ [".SS \"" ++ x ++ "\""]
                iter acc (HiddenCommand _) = acc
                iter acc (CommandData c@SuperCommand {}) =
                    acc ++ concatMap
                            (render (commandName c ++ " "))
                            (extractCommands (commandSubCommands c))
                iter acc (CommandData c) = acc ++ render "" c
                render :: String -> DarcsCommand -> [String]
                render prefix c =
                    [".B darcs " ++ prefix ++ commandName c] ++
                    (map mangle_args $ commandExtraArgHelp c) ++
                    [".RS 4", escape $ commandHelp c, ".RE"]

      -- | Now I'm showing off: mangle the extra arguments of Darcs commands
      -- so as to use the ideal format for manpages, italic words and roman
      -- punctuation.
      mangle_args :: String -> String
      mangle_args s =
          ".RI " ++ (unwords $ map show (groupBy cmp $ map toLower $ gank s))
              where cmp x y = not $ xor (isAlphaNum x) (isAlphaNum y)
                    xor x y = (x && not y) || (y && not x)
                    gank (' ':'o':'r':' ':xs) = '|' : gank xs
                    gank (x:xs) = x : gank xs
                    gank [] = []

      environment :: [String]
      environment = ".SH ENVIRONMENT" : concat
                    [(".SS \"" ++ andClauses ks ++ "\"") : map escape ds
                     | (ks, ds) <- environmentHelp]

      -- | Copied from Preproc.escape_latex_specials.
      escape :: String -> String
      escape = minus . bs       -- Order is important
        where
          minus      = replace "-"     "\\-"
          bs         = replace "\\"    "\\\\"

          replace :: Eq a => [a] -> [a] -> [a] -> [a]
          replace _ _ [] = []
          replace find repl s =
              if find `isPrefixOf` s
                  then repl ++ (replace find repl (drop (length find) s))
                  else [head s] ++ replace find repl (tail s)

environmentHelpEditor :: ([String], [String])
environmentHelpEditor = (["DARCS_EDITOR", "DARCSEDITOR", "VISUAL", "EDITOR"],[
 "To edit a patch description of email comment, Darcs will invoke an",
 "external editor.  Your preferred editor can be set as any of the",
 "environment variables $DARCS_EDITOR, $DARCSEDITOR, $VISUAL or $EDITOR.",
 "If none of these are set, vi(1) is used.  If vi crashes or is not",
 "found in your PATH, emacs, emacs -nw, nano and (on Windows) edit are",
 "each tried in turn."])

environmentHelpPager :: ([String], [String])
environmentHelpPager = (["DARCS_PAGER", "PAGER"],[
 "Darcs will sometimes invoke a pager if it deems output to be too long",
 "to fit onscreen.  Darcs will use the pager specified by $DARCS_PAGER",
 "or $PAGER.  If neither are set, `less' will be used."])

