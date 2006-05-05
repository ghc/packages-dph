-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Subarrays
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
--  Subarrays of flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Subarrays (
  sliceU, extractU, takeU, dropU, splitAtU,
  {- takeWhileU, dropWhileU, spanU, breakU -}
) where

import Data.Array.Parallel.Base (
  runST)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr,
  lengthU, sliceU,
  newMU, copyMU, unsafeFreezeMU)

-- sliceU reexported from UArr

{-# INLINE extractU #-}
extractU :: UA a => UArr a -> Int -> Int -> UArr a
extractU arr i n =
  runST (do
    marr <- newMU n
    copyMU marr 0 $ sliceU arr i n
    unsafeFreezeMU marr n
  )

-- |Extract a prefix of an array
--
takeU :: UA e=> Int -> UArr e -> UArr e
{-# INLINE takeU #-}
takeU n a = extractU a 0 n

-- |Extract a suffix of an array
--
dropU :: UA e => Int -> UArr e -> UArr e
{-# INLINE dropU #-}
dropU n a = let len = lengthU a 
	    in
	    extractU a n (len - n)

-- |Split an array into two halves at the given index
--
splitAtU :: UA e => Int -> UArr e -> (UArr e, UArr e)
{-# INLINE splitAtU #-}
splitAtU n a = (takeU n a, dropU n a)

