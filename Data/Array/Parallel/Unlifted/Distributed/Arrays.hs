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
  lengthD, splitLenD, splitLengthD, splitD, joinLengthD, joinD,

  permuteD, bpermuteD, updateD
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, ST, runST)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr, MUArr, lengthU, sliceU, bpermuteU,
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
splitD :: UA a => Gang -> UArr a -> Dist (UArr a)
{-# INLINE [1] splitD #-}
splitD g arr = zipWithD (seqGang g) (sliceU arr) is dlen
  where
    dlen = splitLengthD g arr
    is   = fstS $ scanD g (+) 0 dlen

-- lengthD reexported from types

-- | Overall length of a distributed array.
joinLengthD :: UA a => Gang -> Dist (UArr a) -> Int
joinLengthD g = sumD g . lengthD

-- | Join a distributed array.
joinD :: UA a => Gang -> Dist (UArr a) -> UArr a
{-# INLINE [1] joinD #-}
joinD g darr = checkGangD (here "joinD") g darr $
               newU n (\ma -> zipWithDST_ g (copy ma) di darr)
  where
    di :*: n = scanD g (+) 0 $ lengthD darr
    --
    copy ma i arr = stToDistST (copyMU ma i arr)

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

"splitD/joinD" forall gang darr.
  splitD gang (joinD gang darr) = darr

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
bpermuteD g !as = mapD g (bpermuteU as)

updateD :: UA a => Gang -> Dist (UArr a) -> Dist (UArr (Int :*: a)) -> UArr a
updateD g darr upd = runST (
  do
    marr <- joinDM g darr
    mapDST_ g (update marr) upd
    unsafeFreezeAllMU marr
  )
  where
    update marr arr = stToDistST (updateMU marr arr)

