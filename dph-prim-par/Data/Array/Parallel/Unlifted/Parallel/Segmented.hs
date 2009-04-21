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
  mapSUP, filterSUP, packCUP, combineCUP,
  zipWithSUP, foldlSUP, foldlSUP', foldSUP, foldSUP', sumSUP, bpermuteSUP',
  sumRUP, foldRUP,
  enumFromThenToSUP, replicateSUP, replicateCUP, repeatCUP, indexedSUP, jsTest
) where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP, zipWithUP, packUP, combineUP)
import Data.Array.Parallel.Unlifted.Parallel.Sums (
  sumUP )
import Data.Array.Parallel.Unlifted.Parallel.Basics (
  replicateUP, replicateEachUP, repeatUP)
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute ( bpermuteUP )
import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, uncurryS)


jsTest as=   joinSD theGang balanced $
         (splitSD theGang unbalanced as)  


mapSUP:: (UA a, UA b)
           => (a -> b) -> SUArr a -> SUArr b
{-# INLINE mapSUP #-}
mapSUP f as = -- segdSU as >: mapUP f (concatSU as)
  joinSD theGang balanced $
  mapD theGang (mapSU f) $
  (splitSD theGang unbalanced as)  

zipWithSUP :: (UA a, UA b, UA c)
           => (a -> b -> c) -> SUArr a -> SUArr b -> SUArr c
{-# INLINE zipWithSUP #-}
-- zipWithSUP f as bs = segdSU as >: zipWithUP f (concatSU as) (concatSU bs)

zipWithSUP f as bs = joinSD   theGang balanced
                   $ zipWithD theGang (zipWithSU f)
                       (splitSD theGang balanced as)
                       (splitSD theGang balanced bs)

-- |Filter segmented array
--
filterSUP:: (UA e) => (e -> Bool) -> SUArr e -> SUArr e
{-# INLINE_U filterSUP #-}
filterSUP p xssArr = 
  joinSD theGang unbalanced $
  mapD theGang (filterSU p) $
  (splitSD theGang unbalanced xssArr)  


packCUP:: (UA e) => UArr Bool -> SUArr e -> SUArr e
{-# INLINE packCUP #-}
packCUP flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = concatSU $ replicateSUP (segdSU xssArr) flags
    flatData   = packUP (concatSU xssArr) repFlags 
    newLengths = packUP (lengthsSU xssArr) flags    

combineCUP :: UA e => UArr Bool -> SUArr e -> SUArr e -> SUArr e
{-# INLINE combineCUP #-}
combineCUP flags xss yss = newSegd >: flatData
  where
    newLengths = combineUP flags (lengthsSU xss) (lengthsSU yss)
    newSegd    = lengthsToUSegd newLengths
    repFlags   = replicateSU newSegd flags
    flatData   = combineUP (concatSU repFlags) (concatSU xss) (concatSU yss)

foldlSUP :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSUP #-}
foldlSUP f z = joinD   theGang unbalanced
             . mapD    theGang (foldlSU f z)
             . splitSD theGang unbalanced

foldlSUP' :: (UA a, UA b) => (b -> a -> b) -> b -> USegd -> UArr a -> UArr b
{-# INLINE foldlSUP' #-}
foldlSUP' f z segd xs = joinD theGang unbalanced
                       (mapD theGang (foldlSU f z)
                       (splitSD' theGang unbalanced segd xs))

foldSUP :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldSUP #-}
foldSUP = foldlSUP

foldSUP' :: (UA a, UA b) => (b -> a -> b) -> b -> USegd -> UArr a -> UArr b
{-# INLINE foldSUP' #-}
foldSUP' = foldlSUP'

sumSUP :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE sumSUP #-}
sumSUP = foldSUP (+) 0

bpermuteSUP' :: UA a => UArr a -> SUArr Int -> SUArr a
{-# INLINE bpermuteSUP' #-}
bpermuteSUP' as = splitJoinSD theGang (bpermuteSD' theGang as)


-- |Yield a segmented enumerated array using a specific step (unbalanced)
--
enumFromThenToSUP :: (Enum e, UA e) 
		 => UArr e -> UArr e -> UArr e -> SUArr e
{-# INLINE_U enumFromThenToSUP #-}
enumFromThenToSUP  starts nexts ends = 
  joinSD theGang unbalanced $ mapD theGang (\t -> enumFromThenToSU (fstU t) (fstU $ sndU t) (sndU $ sndU t)) $ 
    splitD theGang unbalanced $ zipU starts $ zipU nexts ends


replicateSUP :: UA e => USegd -> UArr e -> SUArr e
{-# INLINE_U replicateSUP #-}
replicateSUP segd es = segd >: arr
  where
    arr = joinD theGang unbalanced
        . mapD theGang mk
        . splitD theGang unbalanced
        $ zipU (lengthsUSegd segd) es

    mk ps = concatSU $ replicateSU (lengthsToUSegd (fstU ps)) (sndU ps)

replicateCUP :: UA e => Int -> UArr e -> SUArr e
{-# INLINE_UP replicateCUP #-}
replicateCUP n arr = segmentArrU (replicateUP n (lengthU arr))
                   $ repeatUP n arr

repeatCUP :: UA e => Int -> UArr Int -> USegd -> UArr e -> UArr e
{-# INLINE_UP repeatCUP #-}
repeatCUP k ns segd xs
  = bpermuteUP xs (repeatIndicesCUP k ns segd)

repeatIndicesCUP :: Int -> UArr Int -> USegd -> UArr Int
{-# INLINE_UP repeatIndicesCUP #-}
repeatIndicesCUP k ns segd = enumFromStepLenEachUP k
                           . mapUP (\(n :*: i) -> i :*: 1 :*: n)
                           . replicateEachUP n ns
                           $ fromUSegd segd
  where
    n = sumUP ns

-- |Associate each data element with its index
--
indexedSUP :: UA e => SUArr e -> SUArr (Int :*: e)
{-# INLINE_U indexedSUP #-}
indexedSUP xss = segdSU xss >: zipU is xs
  where
    xs = concatSU xss

    is = enumFromToEachUP (lengthU xs)
       . zipU (replicateU (lengthSU xss) 0)
       . mapUP (subtract 1)
       $ lengthsSU xss


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