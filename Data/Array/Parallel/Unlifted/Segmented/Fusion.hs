-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Fusion
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (Rewrite Rules)
--
-- Description ---------------------------------------------------------------
--
-- Combinator-based fusion of segmented arrays
--
-- Todo ----------------------------------------------------------------------
--
--  * Add more fusion rules
--

module Data.Array.Parallel.Unlifted.Segmented.Fusion()
where

import Data.Array.Parallel.Base.Fusion
import Data.Array.Parallel.Unlifted.Flat.Loop       (loopU)
import Data.Array.Parallel.Unlifted.Flat.Fusion     ({-rules only-})
import Data.Array.Parallel.Unlifted.Segmented.SUArr ((>:))
import Data.Array.Parallel.Unlifted.Segmented.Loop  (loopSU)


-- Fusion rules
-- ------------

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

 #-}

-- Rules for segmented loops
--

-- {-# RULES  -- -} (for font-locking)

-- #-}


