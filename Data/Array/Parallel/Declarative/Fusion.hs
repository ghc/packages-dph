-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Declarative.Fusion
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (Rewrite Rules)
--
-- Description ---------------------------------------------------------------
--
-- Combinator-based array fusion
--
-- Todo ----------------------------------------------------------------------
--
--  * Add more fusion rules
--

module Data.Array.Parallel.Declarative.Fusion (
  module Data.Array.Parallel.Base.Fusion
) where

-- friends
import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Base.Fusion
import Data.Array.Parallel.Monadic.UArr (
  UArr(UAUnit), zipU)
import Data.Array.Parallel.Monadic.SUArr (
  (>:))
import Data.Array.Parallel.Declarative.Loop (
  unitsU, replicateU, loopU, replicateSU, loopSU)

-- Fusion rules
-- ------------

-- Rules for unsegmented loops
--

{-# RULES  -- -} (for font-locking)

-- We have to use (unitsU n) instead of (replicateU n ()) here, as the latter
-- would lead to nontermination.

"loopU/replicateU" forall em start n v.
  loopU em start (replicateU n v) = 
    loopU (\a _ -> em a v) start (unitsU n)

"loopU/loopU" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    let
      em (acc1 :*: acc2) e = 
        case em1 acc1 e of
	  (acc1' :*: Nothing) -> ((acc1' :*: acc2) :*: Nothing)
	  (acc1' :*: Just e') ->
	    case em2 acc2 e' of
	      (acc2' :*: res) -> ((acc1' :*: acc2') :*: res)
    in
    loopSndAcc (loopU em (start1 :*: start2) arr)

-- fusion across a zipU
--
-- * we can only fuse across a zip if the the fused loop does not drop any
--   elements; the case of a `mapEFL' guarantees this and is easy to recognise
--
-- * note that, due to `mapEFL', we are ignoring the accumulator; so, we can
--   just forget about the accumulator of the inner loop
{- FIXME: typing problem
"loopU/zipU/loopU-left" forall f1 start1 em2 start2 pa1 pa2.
  loopU em2 start2 (zipU (loopArr (loopU (mapEFL f1) start1 pa1)) pa2) =
    let
      em acc2 (e1 :*: e2) = 
        case em2 acc2 (f1 e1 :*: e2) of
	  (acc2' :*: Nothing ) -> (acc2' :*: Nothing )
	  (acc2' :*: Just e'') -> (acc2' :*: Just e'')
    in
    loopU em start2 (zipU pa1 pa2)

"loopU/zipU/loopU-right" forall f1 start1 em2 start2 pa1 pa2.
  loopU em2 start2 (zipU pa2 (loopArr (loopU (mapEFL f1) start1 pa1))) =
    let
      em acc2 (e1 :*: e2) = 
        case em2 acc2 (e1 :*: f1 e2) of
	  (acc2' :*: Nothing ) -> (acc2' :*: Nothing )
	  (acc2' :*: Just e'') -> (acc2' :*: Just e'')
    in
    loopU em start2 (zipU pa1 pa2)
-}
--FIXME: similar zip rules work for scanEFL, too

 #-}

-- Rules combining segmented and unsegmented loops
--

{-# RULES  -- -} (for font-locking)

--FIXME: would it be better to match on PAPArr instead of on ">:"?
--       Problem mit matching auf PAPArr is that GHC inserts some sort of
--	 $wPAPArr, which messes everything up.  Can we prevent this?
"loopSU/loopU[map]" forall f1 em2 sm2 segd start1 start2 arr.
  loopSU em2 sm2 start2 (segd >: loopArr (loopU (mapEFL f1) start1 arr)) =
    let
      em acc2 e = em2 acc2 (f1 e)
    in
    loopSU em sm2 start2 (segd >: arr)

"loopSU/loopU[scan]" forall f1 em2 sm2 segd start1 start2 arr.
  loopSU em2 sm2 start2 (segd >: loopArr (loopU (scanEFL f1) start1 arr)) =
    let
      em (acc1 :*: acc2) e = 
        case em2 acc2 acc1 of
	  (acc2' :*: res) -> ((f1 acc1 e :*: acc2') :*: res)
      --
      sm (acc1 :*: acc2) i = 
        case sm2 acc2 i of
	  (acc2' :*: res) -> ((acc1 :*: acc2') :*: res)
    in
    loopSndAcc $ loopSU em sm (start1 :*: start2) (segd >: arr)

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

