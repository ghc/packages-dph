-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed.Arrays
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Operations on distributed arrays.
--

module Data.Array.Parallel.Distributed.Arrays (
  lengthD, splitLengthD, splitD, joinLengthD, joinD,

  permuteD, bpermuteD
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS)
import Data.Array.Parallel.Unlifted (
  UA, UArr, lengthU, sliceU, bpermuteU)
import Data.Array.Parallel.Unlifted.Flat (
  newU, copyMU, permuteMU)
import Data.Array.Parallel.Distributed.Gang (
  Gang, gangSize, seqGang)
import Data.Array.Parallel.Distributed.DistST (
  stToDistST)
import Data.Array.Parallel.Distributed.Types (
  DT, Dist, lengthD, newD, writeMD,
  checkGangD)
import Data.Array.Parallel.Distributed.Combinators (
  mapD, zipWithD, scanD,
  zipWithDST_)
import Data.Array.Parallel.Distributed.Scalars (
  sumD)

here s = "Data.Array.Parallel.Distributed.Arrays." ++ s

-- | Distribute the length of an array over a 'Gang'.
splitLengthD :: UA a => Gang -> UArr a -> Dist Int
splitLengthD g a = newD g (`fill` 0)
  where
    n = lengthU a
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


bpermuteD :: UA a => Gang -> UArr a -> Dist (UArr Int) -> Dist (UArr a)
bpermuteD g = mapD g . bpermuteU

