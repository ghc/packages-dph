-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Sums
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/rl/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Various sum-like combinators for flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Sequential.Flat.Sums (
  andU, orU, anyU, allU,
  elemU, notElemU,
  sumU, productU,
  maximumU, minimumU, maximumByU, minimumByU,
  maximumIndexU, minimumIndexU,
  maximumIndexByU, minimumIndexByU,

  -- FIXME
  lengthU'
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Basics ( 
  indexedU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators (
  mapU, foldU, fold1U, foldlU)

infix  4 `elemU`, `notElemU`

-- |
andU :: UArr Bool -> Bool
{-# INLINE andU #-}
andU = foldU (&&) True

-- |
orU :: UArr Bool -> Bool
{-# INLINE orU #-}
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

-- |Determine the maximum element in an array under the given ordering
--
maximumByU :: UA e => (e -> e -> Ordering) -> UArr e -> e
{-# INLINE maximumByU #-}
maximumByU = fold1U . maxBy
  where
    maxBy compare x y = case x `compare` y of
                          LT -> y
                          _  -> x

-- |Determine the index of the maximum element in an array
--
maximumIndexU :: (Ord e, UA e) => UArr e -> Int
{-# INLINE maximumIndexU #-}
maximumIndexU = maximumIndexByU compare

-- |Determine the index of the maximum element in an array under the given
-- ordering
--
maximumIndexByU :: UA e => (e -> e -> Ordering) -> UArr e -> Int
{-# INLINE maximumIndexByU #-}
maximumIndexByU cmp = fstS . maximumByU cmp' . indexedU
  where
    cmp' (_ :*: x) (_ :*: y) = cmp x y

-- |Determine the minimum element in an array
--
minimumU :: (Ord e, UA e) => UArr e -> e
{-# INLINE minimumU #-}
minimumU = fold1U min

-- |Determine the minimum element in an array under the given ordering
--
minimumByU :: UA e => (e -> e -> Ordering) -> UArr e -> e
{-# INLINE minimumByU #-}
minimumByU = fold1U . minBy
  where
    minBy compare x y = case x `compare` y of
                          GT -> y
                          _  -> x

-- |Determine the index of the minimum element in an array
--
minimumIndexU :: (Ord e, UA e) => UArr e -> Int
{-# INLINE minimumIndexU #-}
minimumIndexU = minimumIndexByU compare

-- |Determine the index of the minimum element in an array under the given
-- ordering
--
minimumIndexByU :: UA e => (e -> e -> Ordering) -> UArr e -> Int
{-# INLINE minimumIndexByU #-}
minimumIndexByU cmp = fstS . minimumByU cmp' . indexedU
  where
    cmp' (_ :*: x) (_ :*: y) = cmp x y

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

