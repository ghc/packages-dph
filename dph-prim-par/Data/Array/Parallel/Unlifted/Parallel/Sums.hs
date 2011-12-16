{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Sum-like parallel combinators for unlifted arrays
module Data.Array.Parallel.Unlifted.Parallel.Sums (
  andUP, orUP, allUP, anyUP, sumUP, productUP,
  maximumUP, maximumByUP, maximumIndexByUP
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Basics ( 
  indexedUP)

-- | Compute the logical AND of all the elements in a array.
andUP :: Vector Bool -> Bool
{-# INLINE_UP andUP #-}
andUP = foldUP (&&) True


-- | Compute the logical OR of all the elements in a array.
orUP :: Vector Bool -> Bool
{-# INLINE_UP orUP #-}
orUP = foldUP (||) False


-- | Check whether all the elements in a array meet the given predicate.
allUP :: Unbox e => (e -> Bool) -> Vector e -> Bool
{-# INLINE_UP allUP #-}
allUP p = andUP . mapUP p


-- | Check whether any of the elements in a array meet the given predicate.
anyUP :: Unbox e => (e -> Bool) -> Vector e -> Bool
{-# INLINE_UP anyUP #-}
anyUP p =  orUP . mapUP p


-- | Compute the sum all the elements of a array.
sumUP :: (Unbox a, DT a, Num a) => Vector a -> a
{-# INLINE_UP sumUP #-}
sumUP = foldUP (+) 0


-- | Compute the product of all the elements of an array.
productUP :: (DT e, Num e, Unbox e) => Vector e -> e
{-# INLINE_UP productUP #-}
productUP = foldUP (*) 1


-- | Determine the maximum element in an array.
maximumUP :: (DT e, Ord e, Unbox e) => Vector e -> e
{-# INLINE_UP maximumUP #-}
maximumUP = fold1UP max


-- | Determine the maximum element in an array under the given ordering
maximumByUP :: (DT e, Unbox e) => (e -> e -> Ordering) -> Vector e -> e
{-# INLINE_UP maximumByUP #-}
maximumByUP = fold1UP . maxBy
  where
    maxBy compare' x y = case x `compare'` y of
                          LT -> y
                          _  -> x

-- | Determine the index of the maximum element in an array under the given ordering
maximumIndexByUP :: (DT e, Unbox e) => (e -> e -> Ordering) -> Vector e -> Int
{-# INLINE_UP maximumIndexByUP #-}
maximumIndexByUP cmp = fst . maximumByUP cmp' . indexedUP
  where
    cmp' (_,x) (_,y) = cmp x y
