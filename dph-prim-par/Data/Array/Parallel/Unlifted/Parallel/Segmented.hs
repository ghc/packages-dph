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
  replicateSUP, replicateRSUP, appendSUP, indicesSUP,
  foldSUP, foldRUP, fold1SUP, sumSUP, sumRUP
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
import Data.Array.Parallel.Unlifted.Parallel.UPSegd
import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, uncurryS, unsafe_unpairS, MaybeS(..))
import Data.Array.Parallel.Stream

import Control.Monad.ST ( ST, runST )

replicateSUP :: UA a => UPSegd -> UArr a -> UArr a
{-# INLINE_UP replicateSUP #-}
replicateSUP segd !xs = joinD theGang balanced
                      . mapD theGang rep
                      $ distUPSegd segd
  where
    rep ((dsegd,di),_)
      = replicateSU dsegd (sliceU xs di (lengthUSegd dsegd))

-- FIXME: make this efficient
replicateRSUP :: UA a => Int -> UArr a -> UArr a
{-# INLINE_UP replicateRSUP #-}
replicateRSUP n xs = replicateSUP (lengthsToUPSegd (replicateUP (lengthU xs) n)) xs

appendSUP :: UA a => UPSegd -> UPSegd -> UArr a -> UPSegd -> UArr a -> UArr a
{-# INLINE_UP appendSUP #-}
appendSUP segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD theGang append
  $ distUPSegd segd
  where
    append ((segd,seg_off),el_off)
      = unstreamU $ appendSegS (segdUPSegd xd) xs
                               (segdUPSegd yd) ys
                               (elementsUSegd segd) seg_off el_off

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

fixupFold :: UA a => (a -> a -> a) -> MUArr a s
          -> Dist (Int,UArr a) -> ST s ()
{-# NOINLINE fixupFold #-}
fixupFold f !mrs !dcarry = go 1
  where
    !p = gangSize theGang

    go i | i >= p = return ()
         | nullU c = go (i+1)
         | otherwise   = do
                           x <- readMU mrs k
                           writeMU mrs k (f x (c !: 0))
                           go (i+1)
      where
        (k,c) = indexD dcarry i


folds :: UA a => (a -> a -> a)
              -> (USegd -> UArr a -> UArr a) -> UPSegd -> UArr a -> UArr a
{-# INLINE folds #-}
folds f g segd xs = dcarry `seq` drs `seq` runST (
  do
    mrs <- joinDM theGang drs
    fixupFold f mrs dcarry
    unsafeFreezeAllMU mrs)
  where
    (dcarry,drs)
          = unzipD
          $ mapD theGang partial
          $ zipD (distUPSegd segd)
                 (splitD theGang balanced xs)

    partial (((segd,k),off), as)
      = let rs = g segd as
        in
        rs `seq`
        if off == 0 then ((k, emptyU),     rs)
                    else ((k, takeU 1 rs), dropU 1 rs)


foldSUP :: UA a => (a -> a -> a) -> a -> UPSegd -> UArr a -> UArr a
{-# INLINE foldSUP #-}
foldSUP f !z = folds f (foldlSU f z)

fold1SUP :: UA a => (a -> a -> a) -> UPSegd -> UArr a -> UArr a
{-# INLINE fold1SUP #-}
fold1SUP f = folds f (fold1SU f)

sumSUP :: (Num e, UA e) => UPSegd -> UArr e -> UArr e
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

indicesSUP :: UPSegd -> UArr Int
{-# INLINE_UP indicesSUP #-}
indicesSUP = joinD theGang balanced
           . mapD theGang indices
           . distUPSegd
  where
    indices ((segd,k),off) = indicesSU' off segd

