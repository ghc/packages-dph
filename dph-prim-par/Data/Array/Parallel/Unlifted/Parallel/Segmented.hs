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

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.Segmented (
  replicateSUP, replicateRSUP, foldlSUP, foldSUP, sumSUP, sumRUP
) where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP, zipWithUP, packUP, combineUP)
import Data.Array.Parallel.Unlifted.Parallel.Sums (
  sumUP )
import Data.Array.Parallel.Unlifted.Parallel.Basics (
  replicateUP, repeatUP)
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute ( bpermuteUP )
import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, uncurryS, unsafe_unpairS)

replicateSUP :: UA a => USegd -> UArr a -> UArr a
{-# INLINE_UP replicateSUP #-}
replicateSUP segd xs = joinD theGang unbalanced
                     . mapD theGang (uncurryS replicateSU)
                     . zipD dsegd
                     $ splitAsD theGang (lengthUSegdD dsegd) xs
  where
    dsegd = splitSegdD theGang segd

-- FIXME: make this efficient
replicateRSUP :: UA a => Int -> UArr a -> UArr a
{-# INLINE_UP replicateRSUP #-}
replicateRSUP n xs = replicateSUP (lengthsToUSegd (replicateUP (lengthU xs) n)) xs
                                  

{-
replicateEachUnbalancedUP :: UA e => UArr Int -> UArr e -> UArr e
{-# INLINE_UP    replicateEachUnbalancedUP #-}
replicateEachUnbalancedUP  ns es = 
  joinD theGang unbalanced $ 
     mapD theGang ((uncurryS.uncurryS) replicateEachU) $ 
     mapD theGang (\t -> (((foldU (+) 0 $ fstS t) :*: fstS t) :*: sndS t)) $ 
     mapD theGang unzipU $ 
     splitD theGang unbalanced $ zipU ns es
                 

packCUP :: UA a => UArr Bool -> USegd -> UArr a -> UArr a
{-# INLINE packCUP #-}
packCUP flags segd xs = packUP xs (replicateSUP segd flags)

combineCUP :: UA a => UArr Bool -> UArr a -> UArr a -> USegd -> UArr a
{-# INLINE combineCUP #-}
combineCUP flags xs ys segd = combineUP (replicateSUP segd flags) xs ys
-}

foldlSUP :: (UA a, UA b) => (b -> a -> b) -> b -> USegd -> UArr a -> UArr b
{-# INLINE foldlSUP #-}
foldlSUP f z segd xs = joinD theGang unbalanced
                      (mapD theGang (uncurry (foldlSU f z) . unsafe_unpairS)
                      (zipD dsegd
                      (splitSD theGang dsegd xs)))
  where
    dsegd = splitSegdD theGang segd

foldSUP :: (UA a, UA b) => (b -> a -> b) -> b -> USegd -> UArr a -> UArr b
{-# INLINE foldSUP #-}
foldSUP = foldlSUP

sumSUP :: (Num e, UA e) => USegd -> UArr e -> UArr e
{-# INLINE sumSUP #-}
sumSUP = foldSUP (+) 0



sumRUP :: (Num e, UA e) => Int ->  Int -> UArr e -> UArr e
{-# INLINE sumRUP #-}
sumRUP = foldRUP (+) 0


foldRUP :: (UA a, UA b) => (b -> a -> b) -> b -> Int -> Int -> UArr a -> UArr b
{-# INLINE foldRUP #-}
foldRUP f z  noOfSegs segSize xs = 
   joinD theGang unbalanced
    (zipWithD theGang 
              (\noS -> \xss -> foldlRU f z noS segSize xss)
      (splitLenD theGang noOfSegs)
      (splitAsD theGang 
                (mapD theGang (*segSize) (splitLenD theGang noOfSegs))
                xs))
