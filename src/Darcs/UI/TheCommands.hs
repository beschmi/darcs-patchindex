-- Copyright (C) 2003 David Roundy
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
module Darcs.UI.TheCommands ( commandControlList ) where

import Prelude ()
import Darcs.UI.Commands.Add ( add )
import Darcs.UI.Commands.AmendRecord ( amendrecord, amendunrecord )
import Darcs.UI.Commands.Annotate ( annotate )
import Darcs.UI.Commands.Apply ( apply )
import Darcs.UI.Commands.Changes ( changes, log )
import Darcs.UI.Commands.Convert ( convert )
import Darcs.UI.Commands.Diff ( diffCommand )
import Darcs.UI.Commands.Dist ( dist )
import Darcs.UI.Commands.Get ( get, clone )
import Darcs.UI.Commands.GZCRCs ( gzcrcs )
import Darcs.UI.Commands.Init ( initialize )
import Darcs.UI.Commands.Show ( showCommand, list, query )
import Darcs.UI.Commands.MarkConflicts ( markconflicts )
import Darcs.UI.Commands.Move ( move, mv )
import Darcs.UI.Commands.Optimize ( optimize )
import Darcs.UI.Commands.Pull ( pull, fetch )
import Darcs.UI.Commands.Push ( push )
import Darcs.UI.Commands.Put ( put )
import Darcs.UI.Commands.Record ( record, commit )
import Darcs.UI.Commands.Remove ( remove, rm, unadd )
import Darcs.UI.Commands.Repair ( repair, check )
import Darcs.UI.Commands.Replace ( replace )
import Darcs.UI.Commands.Revert ( revert )
import Darcs.UI.Commands.Rollback ( rollback )
import Darcs.UI.Commands.Send ( send )
import Darcs.UI.Commands.SetPref ( setpref )
import Darcs.UI.Commands.Tag ( tag )
import Darcs.UI.Commands.Test ( test )
import Darcs.UI.Commands.TransferMode ( transferMode )
import Darcs.UI.Commands.Unrecord ( unrecord, unpull, obliterate )
import Darcs.UI.Commands.Unrevert ( unrevert )
import Darcs.UI.Commands.WhatsNew ( whatsnew, status )
import Darcs.UI.Commands ( CommandControl(CommandData,HiddenCommand,GroupName) )

-- | The commands that darcs knows about (e.g. whatsnew, record),
--   organized into thematic groups.  Note that hidden commands
--   are also listed here.
commandControlList :: [CommandControl]
commandControlList = [GroupName "Changing and querying the working copy:",
                CommandData add,
                CommandData remove, HiddenCommand unadd, HiddenCommand rm,
                CommandData move, HiddenCommand mv,
                CommandData replace,
                CommandData revert,
                CommandData unrevert,
                CommandData whatsnew, HiddenCommand status,
                GroupName "Copying changes between the working copy and the repository:",
                CommandData record, HiddenCommand commit,
                CommandData unrecord,
                CommandData amendrecord,
                HiddenCommand amendunrecord,
                CommandData markconflicts,
                GroupName "Direct modification of the repository:",
                CommandData tag,
                CommandData setpref,
                GroupName "Querying the repository:",
                CommandData diffCommand,
                CommandData changes, HiddenCommand log,
                CommandData annotate,
                CommandData dist,
                CommandData test,
                CommandData showCommand, HiddenCommand list, HiddenCommand query,
                HiddenCommand transferMode,
                GroupName "Copying patches between repositories with working copy update:",
                CommandData pull,
                CommandData fetch,
                CommandData obliterate, HiddenCommand unpull,
                CommandData rollback,
                CommandData push,
                CommandData send,
                CommandData apply,
                CommandData get, HiddenCommand clone,
                CommandData put,
                GroupName "Administrating repositories:",
                CommandData initialize,
                CommandData optimize,
                CommandData repair, HiddenCommand check,
                CommandData convert
                ,HiddenCommand gzcrcs
               ]
