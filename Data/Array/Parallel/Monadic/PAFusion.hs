-- |Fusion rules and associated combinators
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  $Id: PAFusion.hs,v 1.8 2002/08/23 05:41:35 chak Exp $
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 + multi-parameter classes + rewrite rules
--
--  Combinator-based array fusion
--
--- Todo ----------------------------------------------------------------------
--
--  * Add more fusion rules
--

module PAFusion (
  noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL
) where

-- GHC-specific modules
import Data.Generics

-- friends
import PABase (PArr, PAUnit, PAPArr(..), (>:), zipP)
import PALoop (loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc)
import PAEP   (replicateP, loopP, replicateSP, loopSP)


-- |Special forms of loop arguments
-- --------------------------------
--
-- * These are common special cases for the three function arguments of gen
--   and loop; we give them special names to make it easier to trigger RULES
--   applying in the special cases represented by these arguments.  The
--   "INLINE [1]" makes sure that these functions are only inlined in the last
--   two simplifier phases.
--
-- * In the case where the accumulator is not needed, it is better to always
--   explicitly return a value `Unit', rather than just copy the input to the
--   output, as the former gives GHC better local information.
-- 

-- |No element function
--
noEFL :: acc -> Unit -> (acc, Maybe Unit)
{-# INLINE [1] noEFL #-}
noEFL acc _  = (acc, Nothing)

-- |No segment function
--
noSFL :: acc -> Int -> (acc, Maybe Unit)
{-# INLINE [1] noSFL #-}
noSFL acc _  = (acc, Nothing)

-- |No accumulator
--
noAL :: Unit
{-# INLINE [1] noAL #-}
noAL = Unit

-- |Special forms for simple cases
--

-- |Element function expressing a mapping only
--
mapEFL :: (e -> e') -> (Unit -> e -> (Unit, Maybe e'))
{-# INLINE [1] mapEFL #-}
mapEFL f = \a e -> (noAL, Just $ f e)
--mapEFL f = \a e -> let e' = f e in e' `seq` (noAL, Just e')

-- |Element function implementing a filter function only
--
filterEFL :: (e -> Bool) -> (Unit -> e -> (Unit, Maybe e))
{-# INLINE [1] filterEFL #-}
filterEFL p = \a e -> if p e then (noAL, Just e) else (noAL, Nothing)

-- |Element function expressing a reduction only
--
foldEFL :: (acc -> e -> acc) -> (acc -> e -> (acc, Maybe Unit))
{-# INLINE [1] foldEFL #-}
foldEFL f = \a e -> (f a e, Nothing)
--foldEFL f = \a e -> let a' = f a e in a' `seq` (a', Nothing)

-- |Element function expressing a prefix reduction only
--
scanEFL :: (acc -> e -> acc) -> (acc -> e -> (acc, Maybe acc))
{-# INLINE [1] scanEFL #-}
scanEFL f = \a e -> (f a e, Just a)
--scanEFL f  = \a e -> let a' = f a e in a' `seq` (a', Just a)

-- |Segment function transforming the accumulator
--
transSFL :: (acc -> acc) -> (acc -> Int -> (acc, Maybe Unit))
{-# INLINE [1] transSFL #-}
transSFL f = \a _ -> (f a, Nothing)

-- |Segment function transforming the accumulator and collecting accumulator
-- values
--
keepSFL :: (acc -> acc) -> (acc -> Int -> (acc, Maybe acc))
{-# INLINE [1] keepSFL #-}
keepSFL f = \a _ -> (f a, Just a)


-- Fusion rules
-- ------------

-- Rules for unsegmented loops
--

{-# RULES  -- -} (for font-locking)

"loopP/replicateP" forall em start n v.
  loopP em start (replicateP n v) = 
    loopP (\a _ -> em v a) start (replicateP n noAL)

"loopP/loopP" forall em1 em2 start1 start2 arr.
  loopP em2 start2 (loopArr (loopP em1 start1 arr)) =
    let
      em (acc1, acc2) e = case em1 e acc1 of
			    (acc1', Nothing) -> ((acc1', acc2), Nothing)
			    (acc1', Just e') ->
			      case em2 e' acc2 of
			        (acc2', res) -> ((acc1', acc2'), res)
    in
    loopSndAcc (loopP em (start1, start2) arr)

-- fusion across a zipP
--
-- * we can only fuse across a zip if the the fused loop does not drop any
--   elements; the case of a `mapEFL' guarantees this and is easy to recognise
--
-- * note that, due to `mapEFL', we are ignoring the accumulator; so, we can
--   just forget about the accumulator of the inner loop
{- FIXME: typing problem
"loopP/zipP/loopP-left" forall f1 start1 em2 start2 pa1 pa2.
  loopP em2 start2 (zipP (loopArr (loopP (mapEFL f1) start1 pa1)) pa2) =
    let
      em acc2 (e1 :*: e2) = 
        case em2 acc2 (f1 e1 :*: e2) of
	  (acc2', Nothing ) -> (acc2', Nothing )
	  (acc2', Just e'') -> (acc2', Just e'')
    in
    loopP em start2 (zipP pa1 pa2)

"loopP/zipP/loopP-right" forall f1 start1 em2 start2 pa1 pa2.
  loopP em2 start2 (zipP pa2 (loopArr (loopP (mapEFL f1) start1 pa1))) =
    let
      em acc2 (e1 :*: e2) = 
        case em2 acc2 (e1 :*: f1 e2) of
	  (acc2', Nothing ) -> (acc2', Nothing )
	  (acc2', Just e'') -> (acc2', Just e'')
    in
    loopP em start2 (zipP pa1 pa2)
-}
--FIXME: similar zip rules work for scanEFL, too

 #-}

-- Rules combining segmented and unsegmented loops
--

{-# RULES  -- -} (for font-locking)

--FIXME: would it be better to match on PAPArr instead of on ">:"?
--       Problem mit matching auf PAPArr is that GHC inserts some sort of
--	 $wPAPArr, which messes everything up.  Can we prevent this?
"loopSP/loopP[map]" forall f1 em2 sm2 segd start1 start2 arr.
  loopSP em2 sm2 start2 (segd >: loopArr (loopP (mapEFL f1) start1 arr)) =
    let
      em acc2 e = em2 acc2 (f1 e)
    in
    loopSP em sm2 start2 (segd >: arr)

"loopSP/loopP[scan]" forall f1 em2 sm2 segd start1 start2 arr.
  loopSP em2 sm2 start2 (segd >: loopArr (loopP (scanEFL f1) start1 arr)) =
    let
      em (acc1, acc2) e = case em2 acc2 acc1 of
			    (acc2', res) -> ((f1 acc1 e, acc2'), res)
      --
      sm (acc1, acc2) i = case sm2 acc2 i of
			    (acc2', res) -> ((acc1, acc2'), res)
    in
    loopSndAcc $ loopSP em sm (start1, start2) (segd >: arr)

--FIXME: there are zip variants, too

"loopArrS/loopSndAcc" forall x.
  loopArrS (loopSndAcc x) = loopArrS x

"loopAccS/loopSndAcc" forall x.
  loopAccS (loopSndAcc x) = loopAccS x

 #-}

-- Rules for segmented loops
--

-- {-# RULES  -- -} (for font-locking)

-- #-}


-- Misc optimising rules
-- ---------------------

{-# RULES  -- -} (for font-locking)

-- Ignore `seq's on the `Unit' type.  This is essentially to ensure that no
-- superflous demands are introduced by accumulators that are not used.
--
"seq/Unit" forall (u::Unit) e.
  u `seq` e = e

 #-}
