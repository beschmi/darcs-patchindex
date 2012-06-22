--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.UI.Commands.Init ( initialize, initializeCmd ) where
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amNotInRepository )
import Darcs.UI.Arguments ( DarcsFlag( UseHashedInventory, UseNoWorkingDir)
                          , workingRepoDir, patchFormatChoices, useWorkingDir )
import Darcs.Repository ( createRepository )

initializeDescription :: String
initializeDescription = "Make the current directory a repository."

initializeHelp :: String
initializeHelp =
 "The `darcs initialize' command turns the current directory into a\n" ++
 "Darcs repository.  Any existing files and subdirectories become\n" ++
 "UNSAVED changes: record them with `darcs record --look-for-adds'.\n" ++
 "\n" ++
 "This command creates the `_darcs' directory, which stores version\n" ++
 "control metadata.  It also contains per-repository settings in\n" ++
 "_darcs/prefs/, which you can read about in the user manual.\n" ++
 "\n" ++
 "By default, patches of the new repository are in the darcs-2 semantics.\n" ++
 "However it is possible to create a repository in darcs-1 semantics with\n" ++
 "the flag `--hashed', althought this is not recommended except for sharing\n" ++
 "patches with a project that uses patches in the darcs-1 semantics.\n" ++
 "\n" ++
 "Initialize is commonly abbreviated to `init'.\n"

initialize :: DarcsCommand
initialize = DarcsCommand {commandProgramName = "darcs",
                         commandName = "initialize",
                         commandHelp = initializeHelp,
                         commandDescription = initializeDescription,
                         commandExtraArgs = 0,
                         commandExtraArgHelp = [],
                         commandPrereq = amNotInRepository,
                         commandCommand = initializeCmd,
                         commandGetArgPossibilities = return [],
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [],

                         commandBasicOptions = [patchFormatChoices,
                                                useWorkingDir,
                                                workingRepoDir]}

initializeCmd :: [DarcsFlag] -> [String] -> IO ()
initializeCmd opts _ =
  createRepository (UseHashedInventory `elem` opts) (UseNoWorkingDir `elem` opts)
