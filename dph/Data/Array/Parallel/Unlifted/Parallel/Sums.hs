-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Sums
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Sum-like parallel combinators for unlifted arrays
--

module Data.Array.Parallel.Unlifted.Parallel.Sums (
  andUP, orUP, sumUP
) where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  foldUP, foldl1UP, fold1UP, mapUP)
import Data.Array.Parallel.Unlifted.Parallel.Basics ( 
  indexedUP)

import Data.Array.Parallel.Base (
  (:*:)(..), fstS)

andUP :: UArr Bool -> Bool
{-# INLINE andUP #-}
andUP = foldUP (&&) True


-- |
orUP :: UArr Bool -> Bool
{-# INLINE orUP #-}
orUP = foldUP (||) False

allUP :: UA e => (e -> Bool) -> UArr e -> Bool
{-# INLINE allUP #-}
allUP p = andUP . mapUP p

-- |
anyUP :: UA e => (e -> Bool) -> UArr e -> Bool
{-# INLINE anyUP #-}
anyUP p =  orUP . mapUP p


sumUP :: (UA a, DT a, Num a) => UArr a -> a
{-# INLINE sumUP #-}
sumUP = foldUP (+) 0

productUP :: (DT e, Num e, UA e) => UArr e -> e
{-# INLINE productUP #-}
productUP = foldUP (*) 1

-- |Determine the maximum element in an array
--
maximumUP :: (DT e, Ord e, UA e) => UArr e -> e
{-# INLINE maximumUP #-}
maximumUP = fold1UP max


-- |Determine the maximum element in an array under the given ordering
--
maximumByUP :: (DT e, UA e) => (e -> e -> Ordering) -> UArr e -> e
{-# INLINE maximumByUP #-}
maximumByUP = fold1UP . maxBy
  where
    maxBy compare x y = case x `compare` y of
                          LT -> y
                          _  -> x

-- |Determine the index of the maximum element in an array under the given
-- ordering
--
maximumIndexByUP :: (DT e, UA e) => (e -> e -> Ordering) -> UArr e -> Int
{-# INLINE maximumIndexByUP #-}
maximumIndexByUP cmp = fstS . maximumByUP cmp' . indexedUP
  where
    cmp' (_ :*: x) (_ :*: y) = cmp x y
