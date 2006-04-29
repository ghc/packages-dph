-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed.Operations
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Pure operations on distributed types.
--

module Data.Array.Parallel.Distributed.Operations (
  zipD, unzipD, fstD, sndD,
  mapD, zipWithD, foldD, scanD,
  splitScalarD, splitLengthD, splitD, joinD,
  lengthsD, lengthD,
  permuteD, bpermuteD
) where

import Data.Array.Parallel.Distributed.Basics
import Data.Array.Parallel.Distributed.Types
import Data.Array.Parallel.Distributed.Gang

import Data.Array.Parallel.Unlifted.NeslLike

import Data.Array.Parallel.Monadic.UArr

import Data.Array.Parallel.Base.Hyperstrict

import Control.Monad.ST                         ( runST )

-- | Pairing of distributed values
--
zipD :: (DT a, DT b) => Dist a -> Dist b -> Dist (a :*: b)
zipD = zipDT

-- | Unpairing of distributed values
--
unzipD :: (DT a, DT b) => Dist (a :*: b) -> (Dist a :*: Dist b)
unzipD = unzipDT

-- | First component of a distributed pair.
--
fstD :: (DT a, DT b) => Dist (a :*: b) -> Dist a
fstD = fstDT

-- | Second component of a distributed pair.
--
sndD :: (DT a, DT b) => Dist (a :*: b) -> Dist b
sndD = sndDT

-- | Apply a function to each element of a distributed value.
--
mapD :: (DT a, MDT b) => Gang -> (a -> b) -> Dist a -> Dist b
mapD g f d = runST (mapDT g f d)

-- | Zip two distributed values with a function.
--
zipWithD :: (DT a, DT b, MDT c)
         => Gang -> (a -> b -> c) -> Dist a -> Dist b -> Dist c
zipWithD g f dx dy = mapD g (uncurryS f) (zipD dx dy)

-- | Fold a distributed value.
--
foldD :: MDT a => Gang -> (a -> a -> a) -> Dist a -> a
foldD g f d = runST (foldDT g f d)

-- | Scan a distributed value; the 'Dist' is the result of a prescan.
--
scanD :: MDT a => Gang -> (a -> a -> a) -> a -> Dist a -> Dist a :*: a
scanD g f z d = runST (scanDT g f z d)

-- | Distribute a scalar over a 'Gang'.
--
-- > x /= _|_ ==> mapD g f (splitScalarD g x) == splitScalar g (f x)
--
splitScalarD :: MDT a => Gang -> a -> Dist a
splitScalarD g x = x `seq` mapD g (const x) (unitDT g)

-- | Overall length of a distributed array.
--
-- > lengthD g d == foldD g (+) (lengthsD d)
--
lengthD :: UA a => Gang -> Dist (UArr a) -> Int
lengthD g = foldD g (+) . lengthsD

-- | Distributed length of a distributed array. Equivalent to @mapD lengthU@
-- but faster.
--
-- > lengthsD d == mapD g lengthU d
--
lengthsD :: UA a => Dist (UArr a) -> Dist Int
lengthsD = lengthsDT

-- | Distribute the length of an array over a 'Gang'.
--
-- > foldD g (+) (splitLengthD g n) == n
--
splitLengthD :: Gang -> Int -> Dist Int
splitLengthD g n = runST (splitLengthDT g n)

-- | Distribute an array over a 'Gang'.
-- 
splitD :: UA a => Gang -> UArr a -> Dist (UArr a)
splitD g arr = runST (splitDT g arr)

-- | Join a distributed array.
--
joinD :: UA a => Gang -> Dist (UArr a) -> UArr a
joinD g darr = runST (joinDT g darr)

permuteD :: UA a => Gang -> Dist (UArr a) -> Dist (UArr Int) -> UArr a
permuteD g darr dis =
  runST (do
    n    <- lengthDT g darr
    marr <- newMU n
    zipWithM_DT_ g (permuteMU marr) darr dis
    unsafeFreezeMU marr n
  )

bpermuteD :: UA a => Gang -> UArr a -> Dist (UArr Int) -> Dist (UArr a)
bpermuteD g = mapD g . bpermuteU

