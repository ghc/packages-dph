-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Subarrays
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
--  Subarrays of flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Subarrays (
  sliceU, extractU, tailU, takeU, dropU, splitAtU,
  {- takeWhileU, dropWhileU, spanU, breakU -}
) where

import Data.Array.Parallel.Stream (
  tailS)

import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr,
  lengthU, sliceU, newU, copyMU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  streamU, unstreamU)

-- sliceU reexported from UArr

{-# INLINE_U extractU #-}
extractU :: UA a => UArr a -> Int -> Int -> UArr a
extractU arr i n = newU n $ \marr -> copyMU marr 0 (sliceU arr i n)

-- |Yield the tail of an array
--
tailU :: UA e => UArr e -> UArr e
{-# INLINE_U tailU #-}
tailU = unstreamU . tailS . streamU

-- |Extract a prefix of an array
--
takeU :: UA e=> Int -> UArr e -> UArr e
{-# INLINE_U takeU #-}
takeU n a = extractU a 0 n

-- |Extract a suffix of an array
--
dropU :: UA e => Int -> UArr e -> UArr e
{-# INLINE_U dropU #-}
dropU n a = let len = lengthU a 
	    in
	    extractU a n (len - n)

-- |Split an array into two halves at the given index
--
splitAtU :: UA e => Int -> UArr e -> (UArr e, UArr e)
{-# INLINE_U splitAtU #-}
splitAtU n a = (takeU n a, dropU n a)

