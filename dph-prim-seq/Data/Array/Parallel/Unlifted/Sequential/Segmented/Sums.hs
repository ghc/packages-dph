-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.Sums
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Sum-like operations on segmented list-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Sequential.Segmented.Sums (
  andSU, orSU, sumSU, productSU, maximumSU, minimumSU,
  sumRU
) where

import Data.Array.Parallel.Unlifted.Sequential.Flat (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (
  USegd )
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators (
  foldSU, fold1SU, foldlRU)

-- |
andSU :: USegd -> UArr Bool -> UArr Bool
andSU = foldSU (&&) True

-- |
orSU :: USegd -> UArr Bool -> UArr Bool
orSU = foldSU (||) False

-- |Compute the segmented sum of an array of numerals
--
sumSU :: (Num e, UA e) => USegd -> UArr e -> UArr e
{-# INLINE sumSU #-}
sumSU = foldSU (+) 0

-- |Compute the segmented product of an array of numerals
--
productSU :: (Num e, UA e) => USegd -> UArr e -> UArr e
{-# INLINE productSU #-}
productSU = foldSU (*) 1

-- |Determine the maximum element in each subarray
--
maximumSU :: (Ord e, UA e) => USegd -> UArr e -> UArr e
{-# INLINE maximumSU #-}
maximumSU = fold1SU max

-- |Determine the minimum element in each subarray
--
minimumSU :: (Ord e, UA e) => USegd -> UArr e -> UArr e
{-# INLINE minimumSU #-}
minimumSU = fold1SU min


-- |Compute the segmented sum of an array of numerals
--
sumRU :: (Num e, UA e) => Int -> Int ->UArr e -> UArr e
{-# INLINE sumRU #-}
sumRU = foldlRU (+) 0
