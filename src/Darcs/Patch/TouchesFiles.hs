-- Copyright (C) 2002-2004 David Roundy
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


module Darcs.Patch.TouchesFiles ( lookTouch, chooseTouching, choosePreTouching,
                      selectTouching,
                      deselectNotTouching, selectNotTouching,
                    ) where
import Control.Applicative ( (<$>) )
import Data.List ( isSuffixOf, nub )

import Darcs.Patch.Choices ( PatchChoices, Tag, TaggedPatch,
                             patchChoices, tag, getChoices,
                      forceFirsts, forceLasts, tpPatch,
                    )
import Darcs.Patch ( Patchy, invert )
import Darcs.Patch.Apply ( ApplyState, applyToFilePaths, effectOnFilePaths )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), mapFL_FL, (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Sealed, seal )
import Storage.Hashed.Tree( Tree )

selectTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
               => Maybe [FilePath] -> PatchChoices p wX wY -> PatchChoices p wX wY
selectTouching Nothing pc = pc
selectTouching (Just files) pc = forceFirsts xs pc
    where ct :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => [FilePath] -> FL (TaggedPatch p) wX wY -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case lookTouchOnlyEffect fs (tpPatch tp) of
                             (True, fs') -> tag tp:ct fs' tps
                             (False, fs') -> ct fs' tps
          xs = case getChoices pc of
               _ :> mc :> lc -> ct (map fix files) (mc +>+ lc)

deselectNotTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                    => Maybe [FilePath] -> PatchChoices p wX wY -> PatchChoices p wX wY
deselectNotTouching Nothing pc = pc
deselectNotTouching (Just files) pc = forceLasts xs pc
    where ct :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => [FilePath] -> FL (TaggedPatch p) wX wY -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case lookTouchOnlyEffect fs (tpPatch tp) of
                             (True, fs') -> ct fs' tps
                             (False, fs') -> tag tp:ct fs' tps
          xs = case getChoices pc of
               fc :> mc :> _ -> ct (map fix files) (fc +>+ mc)

selectNotTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                  => Maybe [FilePath] -> PatchChoices p wX wY -> PatchChoices p wX wY
selectNotTouching Nothing pc = pc
selectNotTouching (Just files) pc = forceFirsts xs pc
    where ct :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => [FilePath] -> FL (TaggedPatch p) wX wY -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case lookTouchOnlyEffect fs (tpPatch tp) of
                             (True, fs') -> ct fs' tps
                             (False, fs') -> tag tp:ct fs' tps
          xs = case getChoices pc of
               fc :> mc :> _ -> ct (map fix files) (fc +>+ mc)

fix :: FilePath -> FilePath
fix f | "/" `isSuffixOf` f = fix $ init f
fix "" = "."
fix "." = "."
fix f = "./" ++ f

chooseTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
               => Maybe [FilePath] -> FL p wX wY -> Sealed (FL p wX)
chooseTouching Nothing p = seal p
chooseTouching files p = case getChoices $ selectTouching files $ patchChoices p of
                          fc :> _ :> _ -> seal $ mapFL_FL tpPatch fc

choosePreTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                  => Maybe [FilePath] -> FL p wX wY -> Sealed (FL p wX)
choosePreTouching files patch = chooseTouching filesBeforePatch patch where
    filesBeforePatch = effectOnFilePaths (invert patch) <$> files

lookTouchOnlyEffect :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => [FilePath] -> p wX wY
    -> (Bool, [FilePath])
lookTouchOnlyEffect fs p = (wasTouched, fs') where
    (wasTouched, _, fs', _) = lookTouch Nothing fs p


lookTouch :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => Maybe [(FilePath, FilePath)]
    -> [FilePath] -> p wX wY
    -> (Bool, [FilePath], [FilePath], [(FilePath, FilePath)])
lookTouch renames fs p = (anyTouched, touchedFs, fs', renames')
    where
          touchedFs = nub . concatMap fsAffectedBy $ affected
          fsAffectedBy af = filter (affectedBy af) fs
          anyTouched = length touchedFs > 0
          affectedBy :: FilePath -> FilePath -> Bool
          touched `affectedBy` f =  touched == f
                                 || touched `isSubPathOf` f
                                 || f `isSubPathOf` touched
          isSubPathOf :: FilePath -> FilePath -> Bool
          path `isSubPathOf` parent = case splitAt (length parent) path of
                                 (path', '/':_) -> path' == parent
                                 _ -> False
          (affected, fs', renames') = applyToFilePaths p renames fs
