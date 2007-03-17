-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed.Basics
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Basic operations on distributed types.
--


module Data.Array.Parallel.Unlifted.Distributed.Basics (
  eqD, neqD, toD, fromD
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize)
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, indexD, newD, writeMD,
  checkGangD)
import Data.Array.Parallel.Unlifted.Distributed.Combinators (
  zipWithD)
import Data.Array.Parallel.Unlifted.Distributed.Scalars (
  andD, orD)

import Control.Monad ( zipWithM_ )

here s = "Data.Array.Parallel.Unlifted.Distributed.Basics." ++ s

-- | Test whether to distributed values are equal. This requires a 'Gang'
-- and hence can't be defined in terms of 'Eq'.
--
eqD :: (Eq a, DT a) => Gang -> Dist a -> Dist a -> Bool
eqD g dx dy = andD g (zipWithD g (==) dx dy)

-- | Test whether to distributed values are not equal. This requires a 'Gang'
-- and hence can't be defined in terms of 'Eq'.
--
neqD :: (Eq a, DT a) => Gang -> Dist a -> Dist a -> Bool
neqD g dx dy = orD g (zipWithD g (/=) dx dy)

-- | Generate a distributed value from the first @p@ elements of a list.
-- 
-- /NOTE:/ Debugging only.
toD :: DT a => Gang -> [a] -> Dist a
toD g xs = newD g (\md -> zipWithM_ (writeMD md) [0 .. gangSize g - 1] xs)

-- | Yield all elements of a distributed value.
--
-- /NOTE:/ Debugging only.
fromD :: DT a => Gang -> Dist a -> [a]
fromD g dt = checkGangD (here "fromDT") g dt $
             map (indexD dt) [0 .. gangSize g - 1]

