-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Basics
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006/2007    Manuel M T Chakravarty & Roman Leshchinskiy
--		 (c) 2008         Manuel M T Chakravarty & Gabriele Keller & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Basic operations on parallel unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.Basics (
  lengthUP, nullUP, indexedUP,
  replicateUP, repeatUP, interleaveUP
) where

import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators ( mapUP )
import Data.Array.Parallel.Unlifted.Parallel.Enum        ( enumFromToUP )
import Data.Array.Parallel.Unlifted.Parallel.Permute     ( bpermuteUP )

import GHC.Base ( remInt )

-- infixl 9 !
-- infixr 5 +:+

-- some of the functions are exactly the same as the U version

-- |Test whether the given array is empty
--
nullUP :: Unbox e => Vector e -> Bool
nullUP  = (== 0) . Seq.length

-- |Yield an empty array
--
emptyUP :: Unbox e => Vector e
emptyUP = Seq.new 0 (const $ return ())

lengthUP :: Unbox e => Vector e -> Int
lengthUP = Seq.length


-- |Yield an array where all elements contain the same value
--
replicateUP :: Unbox e => Int -> e -> Vector e
{-# INLINE_UP replicateUP #-}
replicateUP n !e = joinD theGang balanced
                 . mapD theGang (\n ->Seq.replicate n e)
                 $ splitLenD theGang n

repeatUP :: Unbox e => Int -> Vector e -> Vector e
{-# INLINE_UP repeatUP #-}
repeatUP n es = seq m
              . bpermuteUP es
              . mapUP (\i -> i `remInt` m)
              $ enumFromToUP 0 (m*n-1)
  where
    m = Seq.length es

-- |Interleave elements of two arrays
--
interleaveUP :: Unbox e => Vector e -> Vector e -> Vector e
{-# INLINE_UP interleaveUP #-}
interleaveUP xs ys = joinD theGang unbalanced
                     (zipWithD theGang Seq.interleave
                       (splitD theGang balanced xs)
                       (splitD theGang balanced ys))
   
-- |Associate each element of the array with its index
--
indexedUP :: (DT e, Unbox e) => Vector e -> Vector (Int,e)
{-# INLINE_U indexedUP #-}
indexedUP = splitJoinD theGang indexedFn 
  where
    sizes  arr   = fst $ scanD theGang (+) 0 $ lengthD arr
    indexedFn = \arr -> (zipWithD theGang (\o -> Seq.map (\(x,y) -> (x + o, y))) (sizes arr) $ 
                        mapD theGang Seq.indexed arr)




