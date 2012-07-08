module Darcs.Patch.V1.Core
    ( Patch(..),
      isMerger, mergerUndo
    ) where

import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(ListFormatV1) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Prim ( FromPrim(..), PrimOf, PrimPatchBase, PrimPatch )
import Darcs.Patch.Repair ( Check )

import Darcs.Patch.Witnesses.Ordered ( FL(..), RL )

#include "impossible.h"

data Patch prim wX wY where
    PP :: prim wX wY -> Patch prim wX wY
    Merger :: FL (Patch prim) wX wY
           -> RL (Patch prim) wX wB
           -> Patch prim wC wB
           -> Patch prim wC wD
           -> Patch prim wX wY
    Regrem :: FL (Patch prim) wX wY
           -> RL (Patch prim) wX wB
           -> Patch prim wC wB
           -> Patch prim wC wA
           -> Patch prim wY wX

instance PrimPatch prim => PrimPatchBase (Patch prim) where
    type PrimOf (Patch prim) = prim

instance FromPrim (Patch prim) where
    fromPrim = PP

isMerger :: Patch prim wA wB -> Bool
isMerger (Merger _ _ _ _) = True
isMerger (Regrem _ _ _ _) = True
isMerger _ = False

mergerUndo :: Patch prim wX wY -> FL (Patch prim) wX wY
mergerUndo (Merger undo _ _ _) = undo
mergerUndo _ = impossible

instance PatchListFormat (Patch prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect Patch to be used with any other argument
   -- anyway, so it doesn't matter.
   patchListFormat = ListFormatV1

instance Check (Patch prim)
   -- no checks

instance PatchDebug prim => PatchDebug (Patch prim)
