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
  replicateSUP, replicateRSUP, appendSUP,
  foldlSUP, foldSUP, foldRUP, fold1SUP, sumSUP, sumRUP
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
  (:*:)(..), fstS, sndS, uncurryS, unsafe_unpairS, MaybeS(..))
import Data.Array.Parallel.Stream

replicateSUP :: UA a => USegd -> UArr a -> UArr a
{-# INLINE_UP replicateSUP #-}
{-
replicateSUP segd xs = joinD theGang unbalanced
                     . mapD theGang (uncurryS replicateSU)
                     . zipD dsegd
                     $ splitAsD theGang (lengthUSegdD dsegd) xs
  where
    dsegd = splitSegdD theGang segd
-}
replicateSUP segd !xs = joinD theGang balanced
                      . mapD theGang rep
                      $ splitSegdD' theGang segd
  where
    rep (dsegd :*: di :*: _)
      = bpermuteU xs
      . unstreamU
      $ indicesSegdS (lengthsUSegd dsegd) di (elementsUSegd dsegd)
    

indicesSegdS :: UArr Int -> Int -> Int -> Stream Int
{-# INLINE_STREAM indicesSegdS #-}
indicesSegdS lens k n = Stream next (0 :*: 0 :*: (k-1)) n (sNoArgs "indicesSegS")
  where
    !m = lengthU lens

    {-# INLINE next #-}
    next (i :*: j :*: k)
      | j > 0     = Yield k (i   :*: j-1         :*: k)
      | i < m     = Skip    (i+1 :*: (lens !: i) :*: k+1)
      | otherwise = Done

-- FIXME: make this efficient
replicateRSUP :: UA a => Int -> UArr a -> UArr a
{-# INLINE_UP replicateRSUP #-}
replicateRSUP n xs = replicateSUP (lengthsToUSegd (replicateUP (lengthU xs) n)) xs

appendSUP :: UA a => USegd -> USegd -> UArr a -> USegd -> UArr a -> UArr a
{-# INLINE_UP appendSUP #-}
appendSUP segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD theGang append
  $ splitSegdD' theGang segd
  where
    append (segd :*: seg_off :*: el_off)
      = unstreamU $ appendSegS xd xs yd ys (elementsUSegd segd) seg_off el_off

appendSegS :: UA a => USegd -> UArr a -> USegd -> UArr a -> Int -> Int -> Int
                -> Stream a
{-# INLINE_STREAM appendSegS #-}
appendSegS !xd !xs !yd !ys !n seg_off el_off
  = Stream next state n (sNoArgs "appendSegS")
  where
    !xlens = lengthsUSegd xd
    !ylens = lengthsUSegd yd

    state
      | n == 0 = NothingS
      | el_off < xlens !: seg_off
          = let i = (indicesUSegd xd !: seg_off) + el_off
                j = indicesUSegd yd !: seg_off
                k = (lengthsUSegd xd !: seg_off) - el_off
            in
            JustS (False :*: seg_off :*: i :*: j :*: k :*: n)
      | otherwise
          = let -- NOTE: *not* indicesUSegd xd !: (seg_off+1) since seg_off+1
                -- might be out of bounds
                i = (indicesUSegd xd !: seg_off) + (lengthsUSegd xd !: seg_off)
                el_off' = el_off - lengthsUSegd xd !: seg_off
                j = (indicesUSegd yd !: seg_off) + el_off'
                k = (lengthsUSegd yd !: seg_off) - el_off'
            in
            JustS (True :*: seg_off :*: i :*: j :*: k :*: n)


    {-# INLINE next #-}
    {-
    next NothingS
      | n == 0 = Done
      | el_off < xlens !: seg_off
          = let i = (indicesUSegd xd !: seg_off) + el_off
                j = indicesUSegd yd !: seg_off
                k = (lengthsUSegd xd !: seg_off) - el_off
            in
            Skip (JustS (False :*: seg_off :*: i :*: j :*: k :*: n))
      | otherwise
          = let -- NOTE: *not* indicesUSegd xd !: (seg_off+1) since seg_off+1
                -- might be out of bounds
                i = (indicesUSegd xd !: seg_off) + (lengthsUSegd xd !: seg_off)
                el_off' = el_off - lengthsUSegd xd !: seg_off
                j = (indicesUSegd yd !: seg_off) + el_off'
                k = (lengthsUSegd yd !: seg_off) - el_off'
            in
            Skip (JustS (True :*: seg_off :*: i :*: j :*: k :*: n))
    -}
    next NothingS = Done

    next (JustS (False :*: seg :*: i :*: j :*: k :*: n))
      | n == 0 = Done
      | k == 0 = Skip (JustS (True :*: seg :*: i :*: j :*: (ylens !: seg) :*: n))
      | otherwise = Yield (xs!:i)
                          (JustS (False :*: seg :*: i+1 :*: j :*: k-1 :*: n-1))

    next (JustS (True :*: seg :*: i :*: j :*: k :*: n))
      | n == 0 = Done
      | k == 0
        = let !seg' = seg+1
          in
          Skip (JustS (False :*: seg' :*: i :*: j :*: (xlens !: seg') :*: n))
      | otherwise = Yield (ys!:j)
                          (JustS (True :*: seg :*: i :*: j+1 :*: k-1 :*: n-1))

{-
appendSUP :: UA a => USegd -> UArr a -> USegd -> UArr a -> UArr a
{-# INLINE_UP appendSUP #-}
appendSUP xd xs yd ys = appendSU xd xs yd ys
-}
{-
  = joinD theGang unbalanced
  $ zipWithD theGang
             (\p -> uncurry (uncurry appendSU (unsafe_unpairS p))
                 . unsafe_unpairS)
      (zipD dxd (splitSD theGang dxd xs))
      (zipD dyd (splitSD theGang dyd ys))
  where
    dxd = splitSegdD theGang xd
    dyd = splitSegdD theGang yd
-}                                

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

fold1SUP :: UA a => (a -> a -> a) -> USegd -> UArr a -> UArr a
{-# INLINE fold1SUP #-}
fold1SUP f segd xs = joinD theGang unbalanced
                    (mapD theGang (uncurry (fold1SU f) . unsafe_unpairS)
                    (zipD dsegd
                    (splitSD theGang dsegd xs)))
  where
    dsegd = splitSegdD theGang segd

sumSUP :: (Num e, UA e) => USegd -> UArr e -> UArr e
{-# INLINE sumSUP #-}
sumSUP = foldSUP (+) 0



sumRUP :: (Num e, UA e) => Int -> UArr e -> UArr e
{-# INLINE sumRUP #-}
sumRUP = foldRUP (+) 0


foldRUP :: (UA a, UA b) => (b -> a -> b) -> b -> Int -> UArr a -> UArr b
{-# INLINE foldRUP #-}
foldRUP f z !segSize xs = 
   joinD theGang unbalanced
    (mapD theGang 
      (foldlRU f z segSize)
      (splitAsD theGang (mapD theGang (*segSize) dlen) xs))
  where
    noOfSegs = lengthU xs `div` segSize
    dlen = splitLenD theGang noOfSegs
