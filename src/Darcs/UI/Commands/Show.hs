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

module Darcs.UI.Commands.Show ( showCommand, list, query ) where

import Darcs.UI.Commands ( DarcsCommand(..),
                        CommandControl(CommandData, HiddenCommand),
                        commandAlias, amInRepository
                      )
import Darcs.UI.Commands.ShowAuthors ( showAuthors )
import Darcs.UI.Commands.ShowBug ( showBug )
import Darcs.UI.Commands.ShowContents ( showContents )
import Darcs.UI.Commands.ShowFiles ( showFiles, manifestCmd, toListManifest )
import Darcs.UI.Commands.ShowTags ( showTags )
import Darcs.UI.Commands.ShowRepo ( showRepo )
import Darcs.UI.Commands.ShowIndex ( showIndex, showPristineCmd )
import Darcs.UI.Commands.ShowPatchIndex ( showPatchIndexAll, showPatchIndexFiles, showPatchIndexStatus )

showDescription :: String
showDescription = "Show information which is stored by darcs."

showHelp :: String
showHelp =
 "Use the --help option with the subcommands to obtain help for\n"++
 "subcommands (for example, \"darcs show files --help\").\n" ++
 "\n" ++
 "In previous releases, this command was called `darcs query'.\n" ++
 "Currently this is a deprecated alias.\n"

showCommand :: DarcsCommand
showCommand = SuperCommand {commandProgramName = "darcs",
                      commandName = "show",
                      commandHelp = showHelp,
                      commandDescription = showDescription,
                      commandPrereq = amInRepository,
                      commandSubCommands = [HiddenCommand showBug,
                                              CommandData showContents,
                                              CommandData showFiles, HiddenCommand showManifest,
                                              CommandData showIndex,
                                              CommandData showPristine,
                                              CommandData showRepo,
                                              CommandData showAuthors,
                                              CommandData showTags,
                                              CommandData showPatchIndexAll,
                                              CommandData showPatchIndexFiles,
                                              CommandData showPatchIndexStatus]
                     }

query :: DarcsCommand
query = commandAlias "query" Nothing showCommand

list :: DarcsCommand
list = commandAlias "list" Nothing showCommand

-- unfortunately, aliases for sub-commands have to live in their parent command
-- to avoid an import cycle
showPristine :: DarcsCommand
showPristine = (commandAlias "pristine" (Just showCommand) showIndex) {
  commandCommand = showPristineCmd,
  commandDescription = "Dump contents of pristine cache.",
  commandHelp =
      "The `darcs show pristine' command lists all version-controlled files " ++
      "and directories along with the hashes of their pristine copies. " ++
      "For files, the fields correspond to file size, sha256 of the pristine " ++
      "file content and the filename." }

showManifest :: DarcsCommand
showManifest = (commandAlias "manifest" (Just showCommand) showFiles) {
  commandCommand = manifestCmd toListManifest
}

