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

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  foldUP, foldl1UP, fold1UP, mapUP)
import Data.Array.Parallel.Unlifted.Parallel.Basics ( 
  indexedUP)

andUP :: Vector Bool -> Bool
{-# INLINE andUP #-}
andUP = foldUP (&&) True


-- |
orUP :: Vector Bool -> Bool
{-# INLINE orUP #-}
orUP = foldUP (||) False

allUP :: Unbox e => (e -> Bool) -> Vector e -> Bool
{-# INLINE allUP #-}
allUP p = andUP . mapUP p

-- |
anyUP :: Unbox e => (e -> Bool) -> Vector e -> Bool
{-# INLINE anyUP #-}
anyUP p =  orUP . mapUP p


sumUP :: (Unbox a, DT a, Num a) => Vector a -> a
{-# INLINE sumUP #-}
sumUP = foldUP (+) 0

productUP :: (DT e, Num e, Unbox e) => Vector e -> e
{-# INLINE productUP #-}
productUP = foldUP (*) 1

-- |Determine the maximum element in an array
--
maximumUP :: (DT e, Ord e, Unbox e) => Vector e -> e
{-# INLINE maximumUP #-}
maximumUP = fold1UP max


-- |Determine the maximum element in an array under the given ordering
--
maximumByUP :: (DT e, Unbox e) => (e -> e -> Ordering) -> Vector e -> e
{-# INLINE maximumByUP #-}
maximumByUP = fold1UP . maxBy
  where
    maxBy compare x y = case x `compare` y of
                          LT -> y
                          _  -> x

-- |Determine the index of the maximum element in an array under the given
-- ordering
--
maximumIndexByUP :: (DT e, Unbox e) => (e -> e -> Ordering) -> Vector e -> Int
{-# INLINE maximumIndexByUP #-}
maximumIndexByUP cmp = fst . maximumByUP cmp' . indexedUP
  where
    cmp' (_,x) (_,y) = cmp x y
