{-# LANGUAGE EmptyDataDecls #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed.Arrays
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Operations on distributed arrays.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Distributed.Arrays (
  lengthD, splitLenD, splitLenIdxD,
  splitAsD, splitD, joinLengthD, joinD, splitJoinD, joinDM,
  splitSegdD, splitSegdD', splitSD,

  permuteD, bpermuteD, atomicUpdateD,

  Distribution, balanced, unbalanced
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, unsafe_pairS, ST, runST)
import Data.Array.Parallel.Arr (
  replicateBU, appBU )
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize, seqGang)
import Data.Array.Parallel.Unlifted.Distributed.DistST (
  stToDistST, myIndex )
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, mkDPrim, indexD, lengthD, newD, writeMD, zipD, unzipD, fstD, sndD,
  elementsUSegdD,
  checkGangD)
import Data.Array.Parallel.Unlifted.Distributed.Basics
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Scalars (
  sumD)

import Data.Bits ( shiftR )
import Control.Monad ( when )

import GHC.Base ( quotInt, remInt )

here s = "Data.Array.Parallel.Unlifted.Distributed.Arrays." ++ s

data Distribution

balanced :: Distribution
{-# NOINLINE balanced #-}
balanced = error $ here "balanced: touched"

unbalanced :: Distribution
{-# NOINLINE unbalanced #-}
unbalanced = error $ here "unbalanced: touched"

-- | Distribute the given array length over a 'Gang'.
splitLenD :: Gang -> Int -> Dist Int
{-# INLINE splitLenD #-}
splitLenD g n = generateD_cheap g len
  where
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    {-# INLINE [0] len #-}
    len i | i < m     = l+1
          | otherwise = l

splitLenIdxD :: Gang -> Int -> Dist (Int :*: Int)
{-# INLINE splitLenIdxD #-}
splitLenIdxD g n = generateD_cheap g len_idx
  where
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    {-# INLINE [0] len_idx #-}
    len_idx i | i < m     = l+1 :*: i*(l+1)
              | otherwise = l   :*: i*l + m
                                               

-- | Distribute an array over a 'Gang' such that each threads gets the given
-- number of elements.
splitAsD :: UA a => Gang -> Dist Int -> UArr a -> Dist (UArr a)
{-# INLINE_DIST splitAsD #-}
splitAsD g dlen !arr = zipWithD (seqGang g) (sliceU arr) is dlen
  where
    is = fstS $ scanD g (+) 0 dlen

-- lengthD reexported from types

-- | Overall length of a distributed array.
joinLengthD :: UA a => Gang -> Dist (UArr a) -> Int
{-# INLINE joinLengthD #-}
joinLengthD g = sumD g . lengthD


-- NOTE: We need splitD_impl and joinD_impl to avoid introducing loops through
-- rules. Without them, splitJoinD would be a loop breaker.

-- | Distribute an array over a 'Gang'.
splitD_impl :: UA a => Gang -> UArr a -> Dist (UArr a)
{-# INLINE_DIST splitD_impl #-}
splitD_impl g !arr = generateD_cheap g (\i -> sliceU arr (idx i) (len i))
  where
    n = lengthU arr
    !p = gangSize g
    !l = n `quotInt` p
    !m = n `remInt` p

    idx i | i < m     = (l+1)*i
          | otherwise = l*i + m

    len i | i < m     = l+1
          | otherwise = l

    -- slice i | i < m     = sliceU arr ((l+1)*i) (l+1)
    --         | otherwise = sliceU arr ((l+1)*m + l*(i-m)) l
{-
splitD_impl g !arr = zipWithD (seqGang g) (sliceU arr) is dlen
  where
    dlen = splitLengthD (seqGang g) arr
    is   = fstS $ scanD (seqGang g) (+) 0 dlen
-}

-- | Distribute an array over a 'Gang'.
splitD :: UA a => Gang -> Distribution -> UArr a -> Dist (UArr a)
{-# INLINE_DIST splitD #-}
splitD g _ arr = splitD_impl g arr

joinD_impl :: UA a => Gang -> Dist (UArr a) -> UArr a
{-# INLINE_DIST joinD_impl #-}
joinD_impl g !darr = checkGangD (here "joinD") g darr $
                     newU n (\ma -> zipWithDST_ g (copy ma) di darr)
  where
    di :*: n = scanD g (+) 0 $ lengthD darr
    --
    copy ma i arr = stToDistST (copyMU ma i arr)

-- | Join a distributed array.
joinD :: UA a => Gang -> Distribution -> Dist (UArr a) -> UArr a
{-# INLINE CONLIKE [1] joinD #-}
joinD g _ darr  = joinD_impl g darr

splitJoinD :: (UA a, UA b)
           => Gang -> (Dist (UArr a) -> Dist (UArr b)) -> UArr a -> UArr b
{-# INLINE_DIST splitJoinD #-}
splitJoinD g f !xs = joinD_impl g (f (splitD_impl g xs))

-- | Join a distributed array, yielding a mutable global array
joinDM :: UA a => Gang -> Dist (UArr a) -> ST s (MUArr a s)
{-# INLINE joinDM #-}
joinDM g darr = checkGangD (here "joinDM") g darr $
                do
                  marr <- newMU n
                  zipWithDST_ g (copy marr) di darr
                  return marr
  where
    di :*: n = scanD g (+) 0 $ lengthD darr
    --
    copy ma i arr = stToDistST (copyMU ma i arr)

{-# RULES

"splitD[unbalanced]/joinD" forall g b da.
  splitD g unbalanced (joinD g b da) = da

"splitD[balanced]/joinD" forall g da.
  splitD g balanced (joinD g balanced da) = da

"splitD/splitJoinD" forall g b f xs.
  splitD g b (splitJoinD g f xs) = f (splitD g b xs)

"splitJoinD/joinD" forall g b f da.
  splitJoinD g f (joinD g b da) = joinD g b (f da)

"splitJoinD/splitJoinD" forall g f1 f2 xs.
  splitJoinD g f1 (splitJoinD g f2 xs) = splitJoinD g (f1 . f2) xs

{-
"splitD/zipU" forall g b xs ys.
  splitD g b (zipU xs ys) = zipWithD g zipU (splitD g balanced xs)
                                            (splitD g balanced ys)

"splitJoinD/zipU" forall g f xs ys.
  splitJoinD g f (zipU xs ys)
    = joinD g balanced
        (f (zipWithD g zipU (splitD g balanced xs)
                            (splitD g balanced ys)))

"splitAsD/zipU" forall g dlen xs ys.
  splitAsD g dlen (zipU xs ys) = zipWithD g zipU (splitAsD g dlen xs)
                                                 (splitAsD g dlen ys)
-}

"fstU/joinD" forall g b xs.
  fstU (joinD g b xs) = joinD g b (mapD g fstU xs)

"sndU/joinD" forall g b xs.
  sndU (joinD g b xs) = joinD g b (mapD g sndU xs)

"fstU/splitJoinD" forall g f xs.
  fstU (splitJoinD g f xs) = splitJoinD g (mapD g fstU . f) xs

"sndU/splitJoinD" forall g f xs.
  sndU (splitJoinD g f xs) = splitJoinD g (mapD g sndU . f) xs

"zipU/joinD[1]" forall g xs ys.
  zipU (joinD g balanced xs) ys
    = joinD g balanced (zipWithD g zipU xs (splitD g balanced ys))

"zipU/joinD[2]" forall g xs ys.
  zipU xs (joinD g balanced ys)
    = joinD g balanced (zipWithD g zipU (splitD g balanced xs) ys)

"zipU/splitJoinD" forall gang f g xs ys.
  zipU (splitJoinD gang (imapD gang f) xs) (splitJoinD gang (imapD gang g) ys)
    = splitJoinD gang (imapD gang (\i zs -> let (as :*: bs) = unzipU zs
                                            in zipU (f i as) (g i bs)))
                      (zipU xs ys)

  #-}

-- | Permute for distributed arrays.
permuteD :: UA a => Gang -> Dist (UArr a) -> Dist (UArr Int) -> UArr a
{-# INLINE_DIST permuteD #-}
permuteD g darr dis = newU n (\ma -> zipWithDST_ g (permute ma) darr dis)
  where
    n = joinLengthD g darr
    --
    permute ma arr is = stToDistST (permuteMU ma arr is)


-- NOTE: The bang is necessary because the array must be fully evaluated
-- before we pass it to the parallel computation.
bpermuteD :: UA a => Gang -> UArr a -> Dist (UArr Int) -> Dist (UArr a)
{-# INLINE bpermuteD #-}
bpermuteD g !as ds = mapD g (bpermuteU as) ds

-- NB: This does not (and cannot) try to prevent two threads from writing to
-- the same position. We probably want to consider this an (unchecked) user
-- error.
atomicUpdateD :: UA a
             => Gang -> Dist (UArr a) -> Dist (UArr (Int :*: a)) -> UArr a
{-# INLINE atomicUpdateD #-}
atomicUpdateD g darr upd = runST (
  do
    marr <- joinDM g darr
    mapDST_ g (update marr) upd
    unsafeFreezeAllMU marr
  )
  where
    update marr arr = stToDistST (atomicUpdateMU marr arr)

splitSegdD :: Gang -> USegd -> Dist USegd
{-# NOINLINE splitSegdD #-}
splitSegdD g !segd = mapD g lengthsToUSegd
                   $ splitAsD g d lens
  where
    d = sndS
      . mapAccumLD g chunk 0
      . splitLenD g
      $ elementsUSegd segd

    n = lengthUSegd segd
    lens = lengthsUSegd segd

    chunk i k = let j = go i k
                in j :*: (j-i)

    go i k | i >= n    = i
           | m == 0    = go (i+1) k
           | k <= 0    = i
           | otherwise = go (i+1) (k-m)
      where
        m = lens !: i


search :: Int -> UArr Int -> Int
search !x ys = go 0 (lengthU ys)
  where
    go i n | n <= 0        = i
           | (ys!:mid) < x = go (mid+1) (n-half-1)
           | otherwise     = go i half
      where
        half = n `shiftR` 1
        mid  = i + half

chunk :: USegd -> Int -> Int -> Bool -> (# UArr Int, Int, Int #)
chunk !segd !di !dn is_last
  = (# lens', k-left_len, left_off #)
  where
    !lens' = runST (do
                      mlens' <- newMU n'
                      when (left /= 0) $ writeMU mlens' 0 left
                      copyMU mlens' left_len (sliceU lens k (k'-k))
                      when (right /= 0) $ writeMU mlens' (n' - 1) right
                      unsafeFreezeAllMU mlens')

    lens = lengthsUSegd segd
    idxs = indicesUSegd segd
    n    = lengthU lens

    k  = search di idxs
    k' | is_last   = n
       | otherwise = search (di+dn) idxs

    left  | k == n    = dn
          | otherwise = min ((idxs!:k) - di) dn

    right | k' == k   = 0
          | otherwise = di + dn - (idxs !: (k'-1))

    left_len | left == 0   = 0
             | otherwise   = 1

    left_off | left == 0   = 0
             | otherwise   = di - idxs !: (k-1)

    n' = left_len + (k'-k)

splitSegdD' :: Gang -> USegd -> Dist (USegd :*: Int :*: Int)
{-# INLINE splitSegdD' #-}
splitSegdD' g !segd = imapD g mk
                         (splitLenIdxD g
                         (elementsUSegd segd))
  where
    !p = gangSize g

    mk i (dn :*: di) = case chunk segd di dn (i == p-1) of
                         (# lens, l, o #) -> lengthsToUSegd lens :*: l :*: o

joinSegD :: Gang -> Dist USegd -> USegd
{-# INLINE_DIST joinSegD #-}
joinSegD g = lengthsToUSegd
           . joinD g unbalanced
           . mapD g lengthsUSegd

splitSD :: UA a => Gang -> Dist USegd -> UArr a -> Dist (UArr a)
{-# INLINE_DIST splitSD #-}
splitSD g dsegd xs = splitAsD g (elementsUSegdD dsegd) xs

{-# RULES

"splitSD/splitJoinD" forall g d f xs.
  splitSD g d (splitJoinD g f xs) = f (splitSD g d xs)

"splitSD/zipU" forall g d xs ys.
  splitSD g d (zipU xs ys) = zipWithD g zipU (splitSD g d xs)
                                             (splitSD g d ys)

  #-}

