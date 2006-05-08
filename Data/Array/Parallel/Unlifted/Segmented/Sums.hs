-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Sums
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Sum-like operations on segmented list-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Sums (
  andSU, orSU, sumSU, productSU, maximumSU, minimumSU
) where

import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr)
import Data.Array.Parallel.Unlifted.Segmented.Combinators (
  foldSU)

-- |
andSU :: SUArr Bool -> UArr Bool
andSU = foldSU (&&) True

-- |
orSU :: SUArr Bool -> UArr Bool
orSU = foldSU (||) False

-- |Compute the segmented sum of an array of numerals
--
sumSU :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE sumSU #-}
sumSU = foldSU (+) 0

-- |Compute the segmented product of an array of numerals
--
productSU :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE productSU #-}
productSU = foldSU (*) 1

-- |Determine the maximum element in each subarray
--
maximumSU :: (Bounded e, Ord e, UA e) => SUArr e -> UArr e
--FIXME: provisional until fold1SU implemented
--maximumSU :: (Ord e, MUA e) => UArr (UArr e) -> UArr e
{-# INLINE maximumSU #-}
--maximumSU = fold1SU max
maximumSU = foldSU max minBound

-- |Determine the minimum element in each subarray
--
minimumSU :: (Bounded e, Ord e, UA e) => SUArr e -> UArr e
--FIXME: provisional until fold1SU implemented
--minimumSU :: (Ord e, MUA e) => UArr (UArr e) -> UArr e
{-# INLINE minimumSU #-}
--minimumSU = fold1SU min
minimumSU = foldSU min maxBound

