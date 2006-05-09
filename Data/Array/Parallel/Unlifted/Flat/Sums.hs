-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Sums
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
--  Various sum-like combinators for flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Sums (
  andU, orU, anyU, allU,
  elemU, notElemU,
  sumU, productU, maximumU, minimumU,

  -- FIXME
  lengthU'
) where

import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Flat.Combinators (
  mapU, foldU, fold1U, foldlU)

infix  4 `elemU`, `notElemU`

-- |
andU :: UArr Bool -> Bool
andU = foldU (&&) True

-- |
orU :: UArr Bool -> Bool
orU = foldU (||) False

-- |
allU :: UA e => (e -> Bool) -> UArr e -> Bool
{-# INLINE allU #-}
allU p = andU . mapU p

-- |
anyU :: UA e => (e -> Bool) -> UArr e -> Bool
{-# INLINE anyU #-}
anyU p =  orU . mapU p

-- |Compute the sum of an array of numerals
--
sumU :: (Num e, UA e) => UArr e -> e
{-# INLINE sumU #-}
sumU = foldU (+) 0

-- |Compute the product of an array of numerals
--
productU :: (Num e, UA e) => UArr e -> e
{-# INLINE productU #-}
productU = foldU (*) 1

-- |Determine the maximum element in an array
--
maximumU :: (Ord e, UA e) => UArr e -> e
{-# INLINE maximumU #-}
maximumU = fold1U max

-- |Determine the minimum element in an array
--
minimumU :: (Ord e, UA e) => UArr e -> e
{-# INLINE minimumU #-}
minimumU = fold1U min

-- |Determine whether the given element is in an array
--
elemU :: (Eq e, UA e) => e -> UArr e -> Bool
elemU e = anyU (== e)

-- |Negation of `elemU'
--
notElemU :: (Eq e, UA e) => e -> UArr e -> Bool
notElemU e = allU (/= e)

-- |FIXME: A fuseable version of 'lengthU', should go away
--
lengthU' :: UA e => UArr e -> Int
{-# INLINE lengthU' #-}
lengthU' = foldlU (const . (+1)) 0

