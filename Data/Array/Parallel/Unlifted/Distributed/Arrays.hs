-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed.Arrays
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Operations on distributed arrays.
--

module Data.Array.Parallel.Unlifted.Distributed.Arrays (
  lengthD, splitLenD, splitLengthD, splitD, joinLengthD, joinD, splitJoinD,

  permuteD, bpermuteD, updateD,

  Distribution, balanced, unbalanced
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, ST, runST)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr, MUArr, lengthU, sliceU, bpermuteU, zipU,
  newU, newMU, copyMU, permuteMU, updateMU, unsafeFreezeAllMU)
import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize, seqGang)
import Data.Array.Parallel.Unlifted.Distributed.DistST (
  stToDistST)
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, lengthD, newD, writeMD,
  checkGangD)
import Data.Array.Parallel.Unlifted.Distributed.Combinators (
  mapD, zipWithD, scanD,
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
splitLengthD g = splitLenD g . lengthU

-- | Distribute the given array length over a 'Gang'.
splitLenD :: Gang -> Int -> Dist Int
splitLenD g n = newD g (`fill` 0)
  where
    p = gangSize g
    l = n `div` p
    m = n `mod` p
    --
    fill md i | i < m     = writeMD md i (l+1) >> fill md (i+1)
              | i < p     = writeMD md i l     >> fill md (i+1)
              | otherwise = return ()

-- | Distribute an array over a 'Gang'.
splitD :: UA a => Gang -> Distribution -> UArr a -> Dist (UArr a)
{-# INLINE [1] splitD #-}
splitD g _ !arr = zipWithD (seqGang g) (sliceU arr) is dlen
  where
    dlen = splitLengthD g arr
    is   = fstS $ scanD g (+) 0 dlen

-- lengthD reexported from types

-- | Overall length of a distributed array.
joinLengthD :: UA a => Gang -> Dist (UArr a) -> Int
joinLengthD g = sumD g . lengthD

-- | Join a distributed array.
joinD :: UA a => Gang -> Distribution -> Dist (UArr a) -> UArr a
{-# INLINE [1] joinD #-}
joinD g _ !darr = checkGangD (here "joinD") g darr $
                 newU n (\ma -> zipWithDST_ g (copy ma) di darr)
  where
    di :*: n = scanD g (+) 0 $ lengthD darr
    --
    copy ma i arr = stToDistST (copyMU ma i arr)

splitJoinD :: (UA a, UA b)
           => Gang -> (Dist (UArr a) -> Dist (UArr b)) -> UArr a -> UArr b
{-# INLINE [1] splitJoinD #-}
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

"splitD/zipU" forall g b xs ys.
  splitD g b (zipU xs ys) = zipWithD g zipU (splitD g balanced xs)
                                            (splitD g balanced ys)

"splitJoinD/zipU" forall g f xs ys.
  splitJoinD g f (zipU xs ys)
    = joinD g balanced
        (f (zipWithD g zipU (splitD g balanced xs)
                            (splitD g balanced ys)))

  #-}

-- | Permute for distributed arrays.
permuteD :: UA a => Gang -> Dist (UArr a) -> Dist (UArr Int) -> UArr a
permuteD g darr dis = newU n (\ma -> zipWithDST_ g (permute ma) darr dis)
  where
    n = joinLengthD g darr
    --
    permute ma arr is = stToDistST (permuteMU ma arr is)


-- NOTE: The bang is necessary because the array must be fully evaluated
-- before we pass it to the parallel computation.
bpermuteD :: UA a => Gang -> UArr a -> Dist (UArr Int) -> Dist (UArr a)
{-# INLINE bpermuteD #-}
bpermuteD g !as = mapD g (bpermuteU as)

-- NB: This does not (and cannot) try to prevent two threads from writing to
-- the same position. We probably want to consider this an (unchecked) user
-- error.
updateD :: UA a => Gang -> Dist (UArr a) -> Dist (UArr (Int :*: a)) -> UArr a
updateD g darr upd = runST (
  do
    marr <- joinDM g darr
    mapDST_ g (update marr) upd
    unsafeFreezeAllMU marr
  )
  where
    update marr arr = stToDistST (updateMU marr arr)

