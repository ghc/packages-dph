-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Basics
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
--  Subarray operations on unlifted segmented arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Subarrays (
  sliceSU, extractSU, takeCU, dropCU
) where

import Data.Array.Parallel.Unlifted.Flat (
  UA, (!:), sliceU, extractU, lengthU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, sliceUSegd, extractUSegd, (>:), segdSU, lengthSU, indicesSU)
import Data.Array.Parallel.Unlifted.Segmented.Basics (
  concatSU)

-- |Extract a subrange of the segmented array without copying the elements.
--
sliceSU :: UA e => SUArr e -> Int -> Int -> SUArr e
sliceSU sa i n =
  let
    a     = concatSU sa
    i'    = indicesSU sa !: i
    j     = if i+n == lengthSU sa then lengthU a else indicesSU sa !: (i+n)
  in
  sliceUSegd (segdSU sa) i n >: sliceU a i' j
    
-- |Extract a subrange of the segmented array (elements are copied).
--
extractSU :: UA e => SUArr e -> Int -> Int -> SUArr e
extractSU sa i n =
  let
    a     = concatSU sa
    i'    = indicesSU sa !: i
    j     = if i+n == lengthSU sa then lengthU a else indicesSU sa !: (i+n)
  in
  extractUSegd (segdSU sa) i n >: extractU a i' j


takeCU:: (UA e) => Int ->  SUArr e  -> SUArr e
{-# INLINE takeCU #-}
takeCU n xssArr = sliceSU xssArr 0 n 



dropCU:: (UA e) => Int ->  SUArr e  -> SUArr e
{-# INLINE dropCU #-}
dropCU n xssArr = sliceSU xssArr n (lengthSU xssArr - n)
