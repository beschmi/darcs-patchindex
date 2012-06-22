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


-- | PatchChoices divides a sequence of patches into three sets: "first",
-- "middle" and "last", such that all patches can be applied, if you first
-- apply the first ones then the middle ones and then the last ones.
-- Obviously if there are dependencies between the patches that will put a
-- constraint on how you can choose to divide them up.  The PatchChoices data
-- type and associated functions are here to deal with many of the common
-- cases that come up when choosing a subset of a group of patches.
--
-- 'forceLast' tells PatchChoices that a particular patch is required to be in
-- the "last" group, which also means that any patches that depend on it
-- must be in the "last" group.
--
-- Internally, a PatchChoices doesn't always reorder the patches until
-- it is asked for the final output (e.g. by 'get_first_choice').
-- Instead, each patch is placed in a state of definitely first,
-- definitely last and undecided; undecided leans towards
-- "middle". The patches that are first are commuted to the head
-- immediately, but patches that are middle and last are mixed
-- together. In case you're wondering about the first-middle-last
-- language, it's because in some cases the "yes" answers will be last
-- (as is the case for the revert command), and in others first (as in
-- record, pull and push).
--
-- Some patch marked "middle" may in fact be unselectable because of
-- dependencies: when a patch is marked "last", its dependencies are
-- not updated until patchSlot is called on them.
module Darcs.Patch.Choices ( PatchChoices, patchChoices, patchChoicesTps,
                             patchChoicesTpsSub,
                      patchSlot, patchSlot',
                      getChoices, refineChoices,
                      separateFirstMiddleFromLast,
                      separateFirstFromMiddleLast,
                      forceFirst, forceFirsts, forceLast, forceLasts,
                      forceMatchingFirst, forceMatchingLast,
                      selectAllMiddles,
                      makeUncertain, makeEverythingLater, makeEverythingSooner,
                      TaggedPatch, Tag, tag, tpPatch,
                             Slot(..),
                      substitute
                    ) where

import Control.Monad.Identity ( Identity )
import Control.Monad.State ( StateT(..) )

import Prelude hiding ( pred )

import Darcs.Patch
     ( Patchy, commuteRL, commute, merge, listTouchedFiles, hunkMatches
     , invert )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Permutations ( commuteWhatWeCanRL, commuteWhatWeCanFL )
import Darcs.Patch.Patchy ( Invert, Commute, PatchInspect )
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( FL(..), RL(..),
                             (:>)(..), (:\/:)(..), (:/\:)(..), (:||:)(..),
                             zipWithFL, mapFL_FL, concatFL,
                             (+>+), reverseRL, anyFL )
import Darcs.Witnesses.Sealed ( Sealed2(..) )


#include "impossible.h"


-- | 'TG' @mp i@ acts as a temporary identifier to help us keep track of patches
--   during the selection process.  These are useful for finding patches that
--   may have moved around during patch selection (being pushed forwards or
--   backwards as dependencies arise).
--
--   The identifier is implemented as a tuple @TG mp i@. The @i@ is just some
--   arbitrary label, expected to be unique within the patches being
--   scrutinised.  The @mp@ is motivated by patch splitting; it
--   provides a convenient way to generate a new identifier from the patch
--   being split.  For example, if we split a patch identified as @TG Nothing
--   5@, the resulting sub-patches could be identified as @TG (TG Nothing 5)
--   1@, @TG (TG Nothing 5) 2@, etc.
data Tag = TG (Maybe Tag) Integer deriving ( Eq, Ord )
data TaggedPatch p wX wY = TP Tag (p wX wY)

-- | The @Bool@ parameter indicates whether the patch has been explicitely
-- selected (or rejected) by the user.
data PatchChoice p wX wY = PC { pcPatch :: (TaggedPatch p wX wY)
                               , _pcChoice :: Bool}

data PatchChoices p wX wY where
  PCs :: { pcsFirsts :: FL (TaggedPatch p) wX wM
         , pcsLasts :: FL (PatchChoice p) wM wY}
      -> PatchChoices p wX wY

-- | See module documentation for 'Darcs.Patch.Choices'
data Slot = InFirst | InMiddle | InLast

tag :: TaggedPatch p wX wY -> Tag
tag (TP tg _) = tg

tpPatch :: TaggedPatch p wX wY -> p wX wY
tpPatch (TP _ p) = p

liftTP :: (p wX wY -> p wA wB) -> (TaggedPatch p wX wY -> TaggedPatch p wA wB)
liftTP f (TP t p) = TP t (f p)

-- This is dangerous if two patches from different tagged series are compared
-- ideally Tag (and hence TaggedPatch/PatchChoices) would have a witness type
-- to represent the originally tagged sequence.
compareTags :: TaggedPatch p wA wB -> TaggedPatch p wC wD -> EqCheck (wA, wB) (wC, wD)
compareTags (TP t1 _) (TP t2 _) = if t1 == t2 then unsafeCoerceP IsEq else NotEq

instance MyEq p => MyEq (TaggedPatch p) where
    unsafeCompare (TP t1 p1) (TP t2 p2) = t1 == t2 && unsafeCompare p1 p2

instance Invert p => Invert (TaggedPatch p) where
    invert = liftTP invert

instance Commute p => Commute (TaggedPatch p) where
    commute (TP t1 p1 :> TP t2 p2) = do p2' :> p1' <- commute (p1 :> p2)
                                        return (TP t2 p2' :> TP t1 p1')

instance PatchInspect p => PatchInspect (TaggedPatch p) where
    listTouchedFiles (TP _ p) = listTouchedFiles p
    hunkMatches f (TP _ p) = hunkMatches f p

instance Merge p => Merge (TaggedPatch p) where
    merge (TP t1 p1 :\/: TP t2 p2) = case merge (p1 :\/: p2) of
                                     p2' :/\: p1' -> TP t2 p2' :/\: TP t1 p1'

instance Commute p => Commute (PatchChoice p) where
  commute (PC p1 c1 :> PC p2 c2) = do p2' :> p1' <- commute (p1 :> p2)
                                      return (PC p2' c2 :> PC p1' c1)

instance PatchInspect p => PatchInspect (PatchChoice p) where
  listTouchedFiles (PC p _) = listTouchedFiles p
  hunkMatches f (PC p _) = hunkMatches f p

instance Merge p => Merge (PatchChoice p) where
  merge (PC tp1 c1 :\/: PC tp2 c2) = case merge (tp1 :\/: tp2) of
    tp2' :/\: tp1' -> PC tp2' c2 :/\: PC tp1' c1

patchChoices :: Patchy p => FL p wX wY -> PatchChoices p wX wY
patchChoices = fst . patchChoicesTps

-- |Tag a sequence of patches as subpatches of an existing tag. This is intended for
-- use when substituting a patch for an equivalent patch or patches.
patchChoicesTpsSub :: Patchy p
                      => Maybe Tag -> FL p wX wY
                      -> (PatchChoices p wX wY, FL (TaggedPatch p) wX wY)
patchChoicesTpsSub tg ps = let tps = zipWithFL TP (map (TG tg) [1..]) ps
                           in (PCs NilFL (mapFL_FL (\tp -> PC tp False) tps), tps)

-- |Tag a sequence of patches.
patchChoicesTps :: Patchy p => FL p wX wY -> (PatchChoices p wX wY, FL (TaggedPatch p) wX wY)
patchChoicesTps = patchChoicesTpsSub Nothing

instance MyEq p => MyEq (PatchChoice p) where
    unsafeCompare (PC tp1 _) (PC tp2 _) = unsafeCompare tp1 tp2


separateFirstFromMiddleLast :: Patchy p => PatchChoices p wX wZ
                                -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) wX wZ
separateFirstFromMiddleLast (PCs f l) = f :> mapFL_FL (\ (PC tp _) -> tp) l

separateFirstMiddleFromLast :: Patchy p => PatchChoices p wX wZ
                                -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) wX wZ
separateFirstMiddleFromLast (PCs f l) =
  case pushLasts l of
    (m :> l') -> f +>+ m :> l'

-- | @getChoices@ evaluates a @PatchChoices@ into the first, middle and last sequences
-- by doing the commutes that were needed.
getChoices :: Patchy p => PatchChoices p wX wY
            -> (FL (TaggedPatch p) :> FL (TaggedPatch p) :> FL (TaggedPatch p)) wX wY
getChoices (PCs f l) =
  case pushLasts l of
       (m :> l') -> f :> m :> l'

pushLasts :: Patchy p => FL (PatchChoice p) wX wY
            -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) wX wY
pushLasts NilFL = NilFL :> NilFL
pushLasts (PC tp False :>: pcs) =
  case pushLasts pcs of
       (m :> l) -> (tp :>: m) :> l
pushLasts (PC tp True :>: pcs) =
  case pushLasts pcs of
    (m :> l) ->
      case commuteWhatWeCanFL (tp :> m) of
        (m' :> tp' :> deps) -> m' :> (tp' :>: deps +>+ l)

-- | @refineChoices act@ performs @act@ on the middle part of a sequence
-- of choices, in order to hopefully get more patches into the @first@ and
-- @last@ parts of a @PatchChoices@.
refineChoices :: (Patchy p, Monad m, Functor m) =>
                (forall wU wV . FL (TaggedPatch p) wU wV ->
                      PatchChoices p wU wV ->
                      m (PatchChoices p wU wV))
                -> PatchChoices p wX wY -> m (PatchChoices p wX wY)
refineChoices act ps =
      case getChoices ps of
        (f :> m :> l) -> do
          let mchoices = PCs NilFL . mapFL_FL (flip PC False) $ m
          (PCs f' l') <- act m mchoices
          return . PCs (f +>+ f') $ l' +>+ mapFL_FL (flip PC True) l

patchSlot :: forall p wA wB wX wY. Patchy p => TaggedPatch p wA wB
          -> PatchChoices p wX wY -> (Slot, PatchChoices p wX wY)
patchSlot (TP t _) pc@(PCs f l) =
  if foundIn f
  then (InFirst, pc)
  else psLast f NilRL NilRL l
  where
    foundIn = anyFL ((== t) . tag)
    psLast :: forall wM wC wL .
             FL (TaggedPatch p) wX wM ->
             RL (TaggedPatch p) wM wC ->
             RL (TaggedPatch p) wC wL ->
             FL (PatchChoice p) wL wY ->
             (Slot, PatchChoices p wX wY)
    psLast firsts middles bubble (PC tp True :>: ls)
      | tag tp == t = (InLast
                      , PCs { pcsFirsts = firsts
                            , pcsLasts = settleM middles
                                         +>+ settleB bubble
                                         +>+ PC tp True :>: ls})
    psLast firsts middles bubble (PC tp False :>: ls)
      | tag tp == t =
        case commuteRL (bubble :> tp) of
        Just (tp' :> bubble') -> (InMiddle,
                                 PCs { pcsFirsts = firsts
                                     , pcsLasts = settleM middles
                                                  +>+ PC tp' False
                                                  :>: settleB bubble'
                                                  +>+ ls})
        Nothing -> (InLast,
                   PCs { pcsFirsts = firsts
                       , pcsLasts = settleM middles
                                    +>+ settleB bubble
                                    +>+ PC tp True
                                    :>: ls})
    psLast firsts middles bubble (PC tp True :>: ls) =
      psLast firsts middles (tp :<: bubble) ls
    psLast firsts middles bubble (PC tp False :>: ls) =
      case commuteRL (bubble :> tp) of
        Just (tp' :> bubble') -> psLast firsts (tp' :<: middles) bubble' ls
        Nothing -> psLast firsts middles (tp :<: bubble) ls
    psLast _ _ _ NilFL = impossible
    settleM middles = mapFL_FL (\tp -> PC tp False) $ reverseRL middles
    settleB bubble = mapFL_FL (\tp -> PC tp True) $ reverseRL bubble

patchSlot' :: Patchy p =>
              TaggedPatch p wA wB -> StateT (PatchChoices p wX wY) Identity Slot
patchSlot' tp = StateT (return . patchSlot tp)

forceMatchingFirst :: forall p wA wB. Patchy p =>
                      ( forall wX wY . TaggedPatch p wX wY -> Bool)
                      -> PatchChoices p wA wB
                      -> PatchChoices p wA wB
forceMatchingFirst pred (PCs fn l) =
  fmfLasts fn NilRL l
    where
      fmfLasts :: FL (TaggedPatch p) wA wM
                 -> RL (PatchChoice p) wM wN
                 -> FL (PatchChoice p) wN wB
                 -> PatchChoices p wA wB
      fmfLasts f l1 (a :>: l2)
          | pred_pc a =
            case commuteWhatWeCanRL (l1 :> a) of
              (deps :> a' :> l1') ->
                let
                  f' = f +>+ mapFL_FL pcPatch (reverseRL deps) +>+ (pcPatch a' :>: NilFL)
                in fmfLasts f' l1' l2
      fmfLasts f l1 (a :>: l2) = fmfLasts f (a :<: l1) l2
      fmfLasts f l1 NilFL = PCs { pcsFirsts = f
                                , pcsLasts = reverseRL l1 }
      pred_pc :: forall wX wY . PatchChoice p wX wY -> Bool
      pred_pc (PC tp _) = pred tp

forceFirsts :: Patchy p => [Tag] -> PatchChoices p wA wB
              -> PatchChoices p wA wB
forceFirsts ps = forceMatchingFirst ((`elem` ps) . tag)

forceFirst :: Patchy p => Tag -> PatchChoices p wA wB
              -> PatchChoices p wA wB
forceFirst p = forceMatchingFirst ((== p) . tag)
--TODO: stop after having seen the patch we want to force first

selectAllMiddles :: forall p wX wY. Patchy p => Bool
                   -> PatchChoices p wX wY -> PatchChoices p wX wY
selectAllMiddles True (PCs f l) = PCs f (mapFL_FL g l)
    where g (PC tp _) = PC tp True
selectAllMiddles False (PCs f l) = samf f NilRL NilRL l
  where
    samf :: forall wM1 wM2 wM3 .
           FL (TaggedPatch p) wX wM1 ->
           RL (TaggedPatch p) wM1 wM2 ->
           RL (PatchChoice p) wM2 wM3 ->
           FL (PatchChoice p) wM3 wY ->
           PatchChoices p wX wY
    samf f1 f2 l1 (pc@(PC tp False) :>: l2) =
      case commuteRL (l1 :> pc) of
        Nothing -> samf f1 f2 (PC tp True :<: l1) l2
        Just ((PC tp' _) :> l1') -> samf f1 (tp' :<: f2) l1' l2
    samf f1 f2 l1 (PC tp True :>: l2) = samf f1 f2 (PC tp True :<: l1) l2
    samf f1 f2 l1 NilFL = PCs (f1 +>+ reverseRL f2) (reverseRL l1)

forceMatchingLast :: Patchy p => (forall wX wY . TaggedPatch p wX wY -> Bool)
                     -> PatchChoices p wA wB
                     -> PatchChoices p wA wB
forceMatchingLast pred (PCs f l) = do
  fmlFirst pred True NilRL f l

fmlFirst :: forall p wA wB wM1 wM2 . Patchy p =>
           (forall wX wY . TaggedPatch p wX wY -> Bool) -> Bool
           -> RL (TaggedPatch p) wA wM1
           -> FL (TaggedPatch p) wM1 wM2
           -> FL (PatchChoice p) wM2 wB
           -> PatchChoices p wA wB
fmlFirst pred b f1 (a :>: f2) l
        | pred a =
          case commuteWhatWeCanFL (a :> f2) of
            (f2' :> a' :> deps) ->
              let
                l' = mapFL_FL (\tp -> PC tp b) (a' :>: deps) +>+ l
              in
              fmlFirst pred b f1 f2' l'
fmlFirst pred b f1 (a :>: f2) l = fmlFirst pred b (a :<: f1) f2 l
fmlFirst pred b f1 NilFL l = PCs { pcsFirsts = reverseRL f1
                                 , pcsLasts = mapFL_FL ch l}
  where ch (PC tp c) = (PC tp (if pred tp then b else c) )

forceLasts :: Patchy p => [Tag]
                    -> PatchChoices p wA wB -> PatchChoices p wA wB
forceLasts ps = forceMatchingLast ((`elem` ps) . tag)

forceLast :: Patchy p => Tag
                    -> PatchChoices p wA wB -> PatchChoices p wA wB
forceLast p = forceMatchingLast ((== p) . tag)

makeUncertain :: Patchy p => Tag -> PatchChoices p wA wB -> PatchChoices p wA wB
makeUncertain t (PCs f l) = fmlFirst ((== t) . tag) False NilRL f l

makeEverythingLater :: Patchy p => PatchChoices p wX wY -> PatchChoices p wX wY
makeEverythingLater (PCs f l) =
  let m = mapFL_FL (\tp -> PC tp False) f
      l' = mapFL_FL (\(PC tp _) -> PC tp True) l
  in
  PCs NilFL $ m +>+ l'

makeEverythingSooner :: forall p wX wY.
  Patchy p => PatchChoices p wX wY -> PatchChoices p wX wY
makeEverythingSooner (PCs f l) =
  case mes NilRL NilRL l
       of (m :> l') ->
            PCs (f +>+ m) l'
    where
      mes :: forall wM1 wM2 wM3 .
            RL (TaggedPatch p) wM1 wM2 ->
            RL (TaggedPatch p) wM2 wM3 ->
            FL (PatchChoice p) wM3 wY ->
            (FL (TaggedPatch p) :> FL (PatchChoice p)) wM1 wY
      mes middle bubble (PC tp True :>: ls) = mes middle (tp :<: bubble) ls
      mes middle bubble (PC tp False :>: ls) =
        case commuteRL (bubble :> tp) of
          Nothing -> mes middle (tp :<: bubble) ls
          Just (tp' :> bubble') -> mes (tp' :<: middle) bubble' ls
      mes middle bubble NilFL = (reverseRL middle) :> mapFL_FL (\tp -> PC tp False) (reverseRL bubble)

-- | 'substitute' @(a :||: bs)@ @pcs@ replaces @a@ with @bs@ in @pcs@ preserving the choice
--   associated with @a@
substitute :: forall p wX wY
            . Patchy p
           => Sealed2 (TaggedPatch p :||: FL (TaggedPatch p))
           -> PatchChoices p wX wY
           -> PatchChoices p wX wY
substitute (Sealed2 (tp :||: new_tps)) (PCs f l) =
  PCs (concatFL $ mapFL_FL substTp f) (concatFL $ mapFL_FL substPc l)
   where
     substTp :: TaggedPatch p wA wB -> FL (TaggedPatch p) wA wB
     substTp tp'
       | IsEq <- compareTags tp tp' = new_tps
       | otherwise = tp' :>: NilFL
     substPc :: PatchChoice p wA wB -> FL (PatchChoice p) wA wB
     substPc (PC tp' c)
       | IsEq <- compareTags tp tp' = mapFL_FL (flip PC c) new_tps
       | otherwise = PC tp' c :>: NilFL
