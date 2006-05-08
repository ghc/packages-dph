-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Basics
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
--  Subarray operations on unlifted segmented arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Subarrays (
  sliceSU, extractSU
) where

import Data.Array.Parallel.Unlifted.Flat (
  UA, (!:), sliceU, extractU, mapU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr(..), USegd(..))

-- |Extract a subrange of the segmented array without copying the elements.
--
sliceSU :: UA e => SUArr e -> Int -> Int -> SUArr e
sliceSU (SUArr segd a) i n =
  let
    segd1 = segdUS segd
    psum  = psumUS segd
    m     = if i == 0 then 0 else psum !: (i - 1)
    psum' = mapU (subtract m) (sliceU psum i n)
    segd' = USegd (sliceU segd1 i n) psum'
    i'    = psum !: i
  in
  SUArr segd' (sliceU a i' (psum !: (i + n - 1) - i' + 1))

-- |Extract a subrange of the segmented array (elements are copied).
--
extractSU :: UA e => SUArr e -> Int -> Int -> SUArr e
extractSU (SUArr segd a) i n =
  let
    segd1 = segdUS segd
    psum  = psumUS segd
    m     = if i == 0 then 0 else psum !: (i - 1)
    psum' = mapU (subtract m) (extractU psum i n)
    segd' = USegd (extractU segd1 i n) psum'
    i'    = psum !: i
  in
  SUArr segd' (extractU a i' (psum !: (i + n - 1) - i' + 1))

