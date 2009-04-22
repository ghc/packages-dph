-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Basics
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
--  Basic operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Basics (
  lengthU, nullU, emptyU, singletonU, consU, unitsU, replicateU, 
  (!:), (+:+), repeatU, repeatUS,
  indexedU,
  toU, fromU
) where

import Data.Array.Parallel.Base (
  (:*:)(..))
import Data.Array.Parallel.Stream (
  Step(..), Stream(..),
  consS, singletonS, replicateS, (+++), indexedS,
  {-replicateEachS, zipS,-} toStream)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr, unitsU, lengthU, indexU, newU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  streamU, unstreamU)


infixl 9 !:
infixr 5 +:+

-- lengthU is reexported from UArr

-- |Test whether the given array is empty
--
nullU :: UA e => UArr e -> Bool
nullU  = (== 0) . lengthU

-- |Yield an empty array
--
emptyU :: UA e => UArr e
emptyU = newU 0 (const $ return ())

-- |Yield a singleton array
--
singletonU :: UA e => e -> UArr e
{-# INLINE_U singletonU #-}
singletonU = unstreamU . singletonS

-- |Prepend an element to an array
--
consU :: UA e => e -> UArr e -> UArr e
{-# INLINE_U consU #-}
consU x = unstreamU . consS x . streamU

-- unitsU is reexported from Loop

-- |Yield an array where all elements contain the same value
--
replicateU :: UA e => Int -> e -> UArr e
{-# INLINE_U replicateU #-}
replicateU n e = unstreamU (replicateS n e)

-- |Array indexing
--
(!:) :: UA e => UArr e -> Int -> e
(!:) = indexU

-- |Concatenate two arrays
--
(+:+) :: UA e => UArr e -> UArr e -> UArr e
{-# INLINE_U (+:+) #-}
a1 +:+ a2 = unstreamU (streamU a1 +++ streamU a2)

-- |Repeat an array @n@ times
--
repeatU :: UA e => Int -> UArr e -> UArr e
{-# INLINE_U repeatU #-}
repeatU n xs = unstreamU (repeatUS n xs)

repeatUS :: UA e => Int -> UArr e -> Stream e
{-# INLINE_STREAM repeatUS #-}
repeatUS k !xs = Stream next (0 :*: k) (k*n)
  where
    n = lengthU xs

    {-# INLINE next #-}
    next (i :*: 0) = Done
    next (i :*: k) | i == n    = Skip (0 :*: k-1)
                   | otherwise = Yield (xs !: i) (i+1 :*: k)

-- |Indexing
-- ---------

-- |Associate each element of the array with its index
--
indexedU :: UA e => UArr e -> UArr (Int :*: e)
{-# INLINE_U indexedU #-}
indexedU = unstreamU . indexedS . streamU

-- |Conversion
-- -----------

-- |Turn a list into a parallel array
--
toU :: UA e => [e] -> UArr e
{-# INLINE_U toU #-}
toU = unstreamU . toStream

-- |Collect the elements of a parallel array in a list
--
fromU :: UA e => UArr e -> [e]
{-# INLINE_U fromU #-}
fromU a = [a!:i | i <- [0..lengthU a - 1]]

