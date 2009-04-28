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
  lengthD, splitLenD, splitLengthD,
  splitAsD, splitD, joinLengthD, joinD, splitJoinD,
  splitSegdD, splitSD,

  permuteD, bpermuteD, atomicUpdateD,

  Distribution, balanced, unbalanced
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, ST, runST)
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize, seqGang)
import Data.Array.Parallel.Unlifted.Distributed.DistST (
  stToDistST)
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, indexD, lengthD, newD, writeMD, zipD, unzipD, fstD, sndD,
  elementsUSegdD,
  checkGangD)
import Data.Array.Parallel.Unlifted.Distributed.Basics
import Data.Array.Parallel.Unlifted.Distributed.Combinators (
  mapD, zipWithD, scanD, mapAccumLD,
  zipWithDST_, mapDST_)
import Data.Array.Parallel.Unlifted.Distributed.Scalars (
  sumD)

here s = "Data.Array.Parallel.Unlifted.Distributed.Arrays." ++ s

data Distribution

balanced :: Distribution
{-# NOINLINE balanced #-}
balanced = error $ here "balanced: touched"

unbalanced :: Distribution
{-# NOINLINE unbalanced #-}
unbalanced = error $ here "unbalanced: touched"

-- | Distribute the length of an array over a 'Gang'.
splitLengthD :: UA a => Gang -> UArr a -> Dist Int
{-# INLINE splitLengthD #-}
splitLengthD g = splitLenD g . lengthU

-- | Distribute the given array length over a 'Gang'.
splitLenD :: Gang -> Int -> Dist Int
{-# NOINLINE PHASE_DIST splitLenD #-}
splitLenD g !n = newD g (`fill` 0)
  where
    p = gangSize g
    l = n `div` p
    m = n `mod` p
    --
    fill md i | i < m     = writeMD md i (l+1) >> fill md (i+1)
              | i < p     = writeMD md i l     >> fill md (i+1)
              | otherwise = return ()



-- | Distribute an array over a 'Gang' such that each threads gets the given
-- number of elements.
splitAsD :: UA a => Gang -> Dist Int -> UArr a -> Dist (UArr a)
{-# INLINE_DIST splitAsD #-}
splitAsD g dlen !arr = zipWithD (seqGang g) (sliceU arr) is dlen
  where
    is = fstS $ scanD g (+) 0 dlen

-- | Distribute an array over a 'Gang'.
splitD :: UA a => Gang -> Distribution -> UArr a -> Dist (UArr a)
{-# INLINE_DIST splitD #-}
splitD g _ !arr = zipWithD (seqGang g) (sliceU arr) is dlen
  where
    dlen = splitLengthD g arr
    is   = fstS $ scanD g (+) 0 dlen

-- lengthD reexported from types

-- | Overall length of a distributed array.
joinLengthD :: UA a => Gang -> Dist (UArr a) -> Int
{-# INLINE joinLengthD #-}
joinLengthD g = sumD g . lengthD

-- | Join a distributed array.
joinD :: UA a => Gang -> Distribution -> Dist (UArr a) -> UArr a
{-# INLINE_DIST joinD #-}
joinD g _ !darr = checkGangD (here "joinD") g darr $
                 newU n (\ma -> zipWithDST_ g (copy ma) di darr)
  where
    di :*: n = scanD g (+) 0 $ lengthD darr
    --
    copy ma i arr = stToDistST (copyMU ma i arr)

splitJoinD :: (UA a, UA b)
           => Gang -> (Dist (UArr a) -> Dist (UArr b)) -> UArr a -> UArr b
{-# INLINE_DIST splitJoinD #-}
splitJoinD g f !xs = joinD g unbalanced (f (splitD g unbalanced xs))

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
    
joinSegD :: Gang -> Dist USegd -> USegd
{-# INLINE_DIST joinSegD #-}
joinSegD g = lengthsToUSegd
           . joinD g unbalanced
           . mapD (seqGang g) lengthsUSegd

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



{- RULES

"splitSD[unbalanced]/joinSD" forall g b da.
  splitSD g unbalanced (joinSD g b da) = da

"splitSD[balanced]/joinSD" forall g da.
  splitSD g balanced (joinSD g balanced da) = da

"splitSD/splitJoinSD" forall g b f xs.
  splitSD g b (splitJoinSD g f xs) = f (splitSD g b xs)

"splitJoinSD/joinSD" forall g b f da.
  splitJoinSD g f (joinSD g b da) = joinSD g b (f da)

"splitJoinSD/splitJoinSD" forall g f1 f2 xs.
  splitJoinSD g f1 (splitJoinSD g f2 xs) = splitJoinSD g (f1 . f2) xs

"fstSU/joinSD" forall g b xs.
  fstSU (joinSD g b xs) = joinSD g b (mapD g fstSU xs)

"sndSU/joinSD" forall g b xs.
  sndSU (joinSD g b xs) = joinSD g b (mapD g sndSU xs)

"splitSD/SUArr" forall g b segd xss.
  splitSD g b (SUArr segd xss) = splitSD' g b segd xss

  -}

