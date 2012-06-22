{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V3.Coalesce () where

import Darcs.Patch.Prim.Class ( PrimCanonize(..) )
import Darcs.Witnesses.Ordered( FL(..) )
import Darcs.Patch.Prim.V3.Core( Prim )

-- TODO
instance PrimCanonize Prim where
   tryToShrink = error "tryToShrink"
   tryShrinkingInverse _ = error "tryShrinkingInverse"
   sortCoalesceFL = id
   canonize = (:>: NilFL)
   canonizeFL = id
   join = const Nothing
