-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Segmented
-- Copyright   : (c) [2006,2007]        Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Parallel combinators for segmented unboxed arrays
--

module Data.Array.Parallel.Unlifted.Parallel.Segmented (
  zipWithSUP, foldlSUP, foldSUP, sumSUP, bpermuteSUP'
) where

import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Unlifted.Segmented
import Data.Array.Parallel.Unlifted.Distributed

zipWithSUP :: (UA a, UA b, UA c)
           => (a -> b -> c) -> SUArr a -> SUArr b -> SUArr c
{-# INLINE zipWithSUP #-}
zipWithSUP f as bs = joinSD   theGang balanced
                   $ zipWithD theGang (zipWithSU f)
                       (splitSD theGang balanced as)
                       (splitSD theGang balanced bs)

foldlSUP :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSUP #-}
foldlSUP f z = joinD   theGang unbalanced
             . mapD    theGang (foldlSU f z)
             . splitSD theGang unbalanced

foldSUP :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldSUP #-}
foldSUP = foldlSUP

sumSUP :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE sumSUP #-}
sumSUP = foldSUP (+) 0

bpermuteSUP' :: UA a => UArr a -> SUArr Int -> SUArr a
{-# INLINE bpermuteSUP' #-}
bpermuteSUP' as = splitJoinSD theGang (bpermuteSD' theGang as)

