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


module Darcs.UI.PrintPatch
    ( printPatch
    , contextualPrintPatch
    , printPatchPager
    , printFriendly
    , showFriendly
    ) where

import Darcs.Patch ( showContextPatch, showPatch, showNicely, description )
import qualified Darcs.Patch ( summary )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Show ( ShowPatch )
import Storage.Hashed.Tree( Tree )
import Storage.Hashed.Monad( virtualTreeIO )
import Darcs.UI.Flags ( DarcsFlag(Summary, Verbose), isUnified )
import Printer ( Doc, putDocLnWith )
import Darcs.ColorPrinter ( fancyPrinters )
import Darcs.UI.External ( viewDocWith )

-- | @'printFriendly' opts patch@ prints @patch@ in accordance with the
-- flags in opts, ie, whether @--verbose@ or @--summary@ were passed at
-- the command-line.
printFriendly :: (ShowPatch p, ApplyState p ~ Tree)
              => (Maybe (Tree IO))
              -> [DarcsFlag]
              -> p wX wY
              -> IO ()
printFriendly (Just pristine) opts p
 | isUnified opts = virtualTreeIO (showContextPatch p) pristine >>= putDocLnWith fancyPrinters . fst
printFriendly _ opts p = putDocLnWith fancyPrinters $ showFriendly opts p

-- | @'showFriendly' flags patch@ returns a 'Doc' representing the right
-- way to show @patch@ given the list @flags@ of flags darcs was invoked with.
showFriendly :: ShowPatch p => [DarcsFlag] -> p wX wY -> Doc
showFriendly opts p | Verbose `elem` opts = showNicely p
                    | Summary `elem` opts = Darcs.Patch.summary p
                    | otherwise           = description p

-- | 'printPatch' prints a patch on standard output.
printPatch :: ShowPatch p => p wX wY -> IO ()
printPatch p = putDocLnWith fancyPrinters $ showPatch p

-- | 'printPatchPager' runs '$PAGER' and shows a patch in it.
printPatchPager :: ShowPatch p => p wX wY -> IO ()
printPatchPager p = viewDocWith fancyPrinters $ showPatch p

-- | 'contextualPrintPatch' prints a patch, together with its context,
-- on standard output.
contextualPrintPatch :: (ShowPatch p, ApplyState p ~ Tree) => Tree IO -> p wX wY -> IO ()
contextualPrintPatch s p = virtualTreeIO (showContextPatch p) s >>= putDocLnWith fancyPrinters . fst
