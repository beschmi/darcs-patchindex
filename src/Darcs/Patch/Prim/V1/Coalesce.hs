{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Coalesce
    ()
    where

import Prelude hiding ( pi )
import Data.Map ( elems, fromListWith, mapWithKey )

import qualified Data.ByteString as B (ByteString)

import Darcs.Path ( FileName, fp2fn )
import Darcs.Patch.Prim.Class ( PrimCanonize(..) )
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core
    ( Prim(..), FilePatchType(..), DirPatchType(..)
    , comparePrim, isIdentity
    )
import Darcs.Patch.Prim.V1.Show ()
import Darcs.Patch.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..), (:<)(..)
    , reverseRL, mapFL, mapFL_FL
    , concatFL, lengthFL, (+>+) )
import Darcs.Patch.Witnesses.Sealed
    ( unseal, Sealed2(..), unsafeUnseal2
    , Gap(..), unFreeLeft
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Commute ( Commute(..) )
-- import Darcs.Patch.Permutations () -- for Invert instance of FL
import Lcs ( getChanges )

#include "impossible.h"

-- | 'coalesce' @p2 :< p1@ tries to combine @p1@ and @p2@ into a single
--   patch without intermediary changes.  For example, two hunk patches
--   modifying adjacent lines can be coalesced into a bigger hunk patch.
--   Or a patch which moves file A to file B can be coalesced with a
--   patch that moves file B into file C, yielding a patch that moves
--   file A to file C.
coalesce :: (Prim :< Prim) wX wY -> Maybe (FL Prim wX wY)
coalesce (FP f1 _ :< FP f2 _) | f1 /= f2 = Nothing
coalesce (p2 :< p1) | IsEq <- p2 =\/= invert p1 = Just NilFL
coalesce (FP f1 p1 :< FP _ p2) = fmap (:>: NilFL) $ coalesceFilePrim f1 (p1 :< p2) -- f1 = f2
coalesce (Move a b :< Move b' a') | a == a' = Just $ Move b' b :>: NilFL
coalesce (Move a b :< FP f AddFile) | f == a = Just $ FP b AddFile :>: NilFL
coalesce (Move a b :< DP f AddDir) | f == a = Just $ DP b AddDir :>: NilFL
coalesce (FP f RmFile :< Move a b) | b == f = Just $ FP a RmFile :>: NilFL
coalesce (DP f RmDir :< Move a b) | b == f = Just $ DP a RmDir :>: NilFL
coalesce (ChangePref p f1 t1 :< ChangePref p2 f2 t2) | p == p2 && t2 == f1 = Just $ ChangePref p f2 t1 :>: NilFL
coalesce _ = Nothing

mapPrimFL :: (forall wX wY . FL Prim wX wY -> FL Prim wX wY)
             -> FL Prim wW wZ -> FL Prim wW wZ
mapPrimFL f x =
-- an optimisation; break the list up into independent sublists
-- and apply f to each of them
     case mapM toSimpleSealed $ mapFL Sealed2 x of
     Just sx -> concatFL $ unsealList $ elems $
                mapWithKey (\ k p -> Sealed2 (f (fromSimples k (unsealList (p []))))) $
                fromListWith (flip (.)) $
                map (\ (a,b) -> (a,(b:))) sx
     Nothing -> f x
  where
        unsealList :: [Sealed2 p] -> FL p wA wB
        unsealList [] = unsafeCoerceP NilFL
        unsealList (y:ys) = unsafeUnseal2 y :>: unsealList ys

        toSimpleSealed :: Sealed2 Prim -> Maybe (FileName, Sealed2 Simple)
        toSimpleSealed (Sealed2 p) = fmap (\(fn, s) -> (fn, Sealed2 s)) (toSimple p)



data Simple wX wY = SFP !(FilePatchType wX wY) | SDP !(DirPatchType wX wY)
                   | SCP String String String
                   deriving ( Show )

toSimple :: Prim wX wY -> Maybe (FileName, Simple wX wY)
toSimple (FP a b) = Just (a, SFP b)
toSimple (DP a AddDir) = Just (a, SDP AddDir)
toSimple (DP _ RmDir) = Nothing -- ordering is trickier with rmdir present
toSimple (Move _ _) = Nothing
toSimple (ChangePref a b c) = Just (fp2fn "_darcs/prefs/prefs", SCP a b c)

fromSimple :: FileName -> Simple wX wY -> Prim wX wY
fromSimple a (SFP b) = FP a b
fromSimple a (SDP b) = DP a b
fromSimple _ (SCP a b c) = ChangePref a b c

fromSimples :: FileName -> FL Simple wX wY -> FL Prim wX wY
fromSimples a bs = mapFL_FL (fromSimple a) bs

tryHarderToShrink :: FL Prim wX wY -> FL Prim wX wY
tryHarderToShrink x = tryToShrink2 $ maybe x id (tryShrinkingInverse x)

tryToShrink2 :: FL Prim wX wY -> FL Prim wX wY
tryToShrink2 psold =
    let ps = sortCoalesceFL psold
        ps_shrunk = shrinkABit ps
                    in
    if lengthFL ps_shrunk < lengthFL ps
    then tryToShrink2 ps_shrunk
    else ps_shrunk

shrinkABit :: FL Prim wX wY -> FL Prim wX wY
shrinkABit NilFL = NilFL
shrinkABit (p:>:ps) =
    case tryOne NilRL p ps of
    Nothing -> p :>: shrinkABit ps
    Just ps' -> ps'

tryOne :: RL Prim wW wX -> Prim wX wY -> FL Prim wY wZ
        -> Maybe (FL Prim wW wZ)
tryOne _ _ NilFL = Nothing
tryOne sofar p (p1:>:ps) =
    case coalesce (p1 :< p) of
    Just p' -> Just (reverseRL sofar +>+ p' +>+ ps)
    Nothing -> case commute (p :> p1) of
               Nothing -> Nothing
               Just (p1' :> p') -> tryOne (p1':<:sofar) p' ps

-- | The heart of "sortCoalesceFL"
sortCoalesceFL2 :: FL Prim wX wY -> FL Prim wX wY
sortCoalesceFL2 NilFL = NilFL
sortCoalesceFL2 (x:>:xs) | IsEq <- isIdentity x = sortCoalesceFL2 xs
sortCoalesceFL2 (x:>:xs) = either id id $ pushCoalescePatch x $ sortCoalesceFL2 xs

-- | 'pushCoalescePatch' @new ps@ is almost like @new :>: ps@ except
--   as an alternative to consing, we first try to coalesce @new@ with
--   the head of @ps@.  If this fails, we try again, using commutation
--   to push @new@ down the list until we find a place where either
--   (a) @new@ is @LT@ the next member of the list [see 'comparePrim']
--   (b) commutation fails or
--   (c) coalescing succeeds.
--   The basic principle is to coalesce if we can and cons otherwise.
--
--   As an additional optimization, pushCoalescePatch outputs a Left
--   value if it wasn't able to shrink the patch sequence at all, and
--   a Right value if it was indeed able to shrink the patch sequence.
--   This avoids the O(N) calls to lengthFL that were in the older
--   code.
--
--   Also note that pushCoalescePatch is only ever used (and should
--   only ever be used) as an internal function in in
--   sortCoalesceFL2.
pushCoalescePatch :: Prim wX wY -> FL Prim wY wZ
                    -> Either (FL Prim wX wZ) (FL Prim wX wZ)
pushCoalescePatch new NilFL = Left (new:>:NilFL)
pushCoalescePatch new ps@(p:>:ps')
    = case coalesce (p :< new) of
      Just (new' :>: NilFL) -> Right $ either id id $ pushCoalescePatch new' ps'
      Just NilFL -> Right ps'
      Just _ -> impossible -- coalesce either returns a singleton or empty
      Nothing -> if comparePrim new p == LT then Left (new:>:ps)
                            else case commute (new :> p) of
                                 Just (p' :> new') ->
                                     case pushCoalescePatch new' ps' of
                                     Right r -> Right $ either id id $
                                                pushCoalescePatch p' r
                                     Left r -> Left (p' :>: r)
                                 Nothing -> Left (new:>:ps)

coalesceFilePrim :: FileName -> (FilePatchType :< FilePatchType) wX wY
                  -> Maybe (Prim wX wY)
coalesceFilePrim f (Hunk line1 old1 new1 :< Hunk line2 old2 new2)
    = coalesceHunk f line1 old1 new1 line2 old2 new2
-- Token replace patches operating right after (or before) AddFile (RmFile)
-- is an identity patch, as far as coalescing is concerned.
coalesceFilePrim f (TokReplace _ _ _ :< AddFile) = Just $ FP f AddFile
coalesceFilePrim f (RmFile :< TokReplace _ _ _) = Just $ FP f RmFile
coalesceFilePrim f (TokReplace t1 o1 n1 :< TokReplace t2 o2 n2)
    | t1 == t2 && n2 == o1 = Just $ FP f $ TokReplace t1 o2 n1
coalesceFilePrim f (Binary m n :< Binary o m')
    | m == m' = Just $ FP f $ Binary o n
coalesceFilePrim _ _ = Nothing

coalesceHunk :: FileName
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Maybe (Prim wX wY)
coalesceHunk f line1 old1 new1 line2 old2 new2
    | line1 == line2 && lengthold1 < lengthnew2 =
        if take lengthold1 new2 /= old1
        then Nothing
        else case drop lengthold1 new2 of
        extranew -> Just (FP f (Hunk line1 old2 (new1 ++ extranew)))
    | line1 == line2 && lengthold1 > lengthnew2 =
        if take lengthnew2 old1 /= new2
        then Nothing
        else case drop lengthnew2 old1 of
        extraold -> Just (FP f (Hunk line1 (old2 ++ extraold) new1))
    | line1 == line2 = if new2 == old1 then Just (FP f (Hunk line1 old2 new1))
                       else Nothing
    | line1 < line2 && lengthold1 >= line2 - line1 =
        case take (line2 - line1) old1 of
        extra-> coalesceHunk f line1 old1 new1 line1 (extra ++ old2) (extra ++ new2)
    | line1 > line2 && lengthnew2 >= line1 - line2 =
        case take (line1 - line2) new2 of
        extra-> coalesceHunk f line2 (extra ++ old1) (extra ++ new1) line2 old2 new2
    | otherwise = Nothing
    where lengthold1 = length old1
          lengthnew2 = length new2

canonizeHunk :: Gap w
             => FileName -> Int -> [B.ByteString] -> [B.ByteString]
             -> w (FL Prim)
canonizeHunk f line old new
    | null old || null new
        = freeGap (FP f (Hunk line old new) :>: NilFL)
canonizeHunk f line old new = makeHoley f line $ getChanges old new

makeHoley :: Gap w
          => FileName -> Int -> [(Int,[B.ByteString], [B.ByteString])]
          -> w (FL Prim)
makeHoley f line changes =
    foldr (joinGap (:>:)) (emptyGap NilFL) $
    map (\ (l,o,n) -> freeGap (FP f (Hunk (l+line) o n))) changes

instance PrimCanonize Prim where
   tryToShrink = mapPrimFL tryHarderToShrink

   tryShrinkingInverse (x:>:y:>:z)
       | IsEq <- invert x =\/= y = Just z
       | otherwise = case tryShrinkingInverse (y:>:z) of
                     Nothing -> Nothing
                     Just yz' -> Just $ case tryShrinkingInverse (x:>:yz') of
                                        Nothing -> x:>:yz'
                                        Just xyz' -> xyz'
   tryShrinkingInverse _ = Nothing

   sortCoalesceFL = mapPrimFL sortCoalesceFL2
   canonize p | IsEq <- isIdentity p = NilFL
   canonize (FP f (Hunk line old new)) = unseal unsafeCoercePEnd $ unFreeLeft $ canonizeHunk f line old new
   canonize p = p :>: NilFL
   -- Running canonize twice is apparently necessary to fix issue525;
   -- would be nice to understand why.
   canonizeFL = concatFL . mapFL_FL canonize . sortCoalesceFL .
                concatFL . mapFL_FL canonize
   join (x :> y) = coalesce (y :< x)
