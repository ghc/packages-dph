-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Basics
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
--  Basic operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Basics (
  lengthU, nullU, emptyU, consU, unitsU, replicateU, (!:), (+:+),
  indexedU,
  toU, fromU
) where

import Data.Array.Parallel.Base (
  (:*:)(..))
import Data.Array.Parallel.Stream (
  consS, replicateS, (+++), indexedS, toStream)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr, unitsU, lengthU, indexU, newU)
import Data.Array.Parallel.Unlifted.Flat.Stream (
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

-- |Prepend an element to an array
--
consU :: UA e => e -> UArr e -> UArr e
{-# INLINE consU #-}
consU x = unstreamU . consS x . streamU

-- unitsU is reexported from Loop

-- |Yield an array where all elements contain the same value
--
replicateU :: UA e => Int -> e -> UArr e
{-# INLINE replicateU #-}
replicateU n e = unstreamU (replicateS n e)

-- |Array indexing
--
(!:) :: UA e => UArr e -> Int -> e
(!:) = indexU

-- |Concatenate two arrays
--
(+:+) :: UA e => UArr e -> UArr e -> UArr e
{-# INLINE (+:+) #-}
a1 +:+ a2 = unstreamU (streamU a1 +++ streamU a2)

-- |Indexing
-- ---------

-- |Associate each element of the array with its index
--
indexedU :: UA e => UArr e -> UArr (Int :*: e)
{-# INLINE indexedU #-}
indexedU = unstreamU . indexedS . streamU

-- |Conversion
-- -----------

-- |Turn a list into a parallel array
--
toU :: UA e => [e] -> UArr e
{-# INLINE toU #-}
toU = unstreamU . toStream

-- |Collect the elements of a parallel array in a list
--
fromU :: UA e => UArr e -> [e]
{-# INLINE fromU #-}
fromU a = [a!:i | i <- [0..lengthU a - 1]]

