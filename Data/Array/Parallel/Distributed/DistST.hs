-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed.DistST
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Distributed ST computations.
--
-- Computations of type 'DistST' are data-parallel computations which
-- are run on each thread of a gang. At the moment, they can only access the
-- element of a (possibly mutable) distributed value owned by the current
-- thread.
--
-- /TODO:/ Add facilities for implementing parallel scans etc.
--

module Data.Array.Parallel.Distributed.DistST (
  DistST, runDistST_, runDistST,

  distST, myDT, readMyMDT, writeMyMDT
) where

import Data.Array.Parallel.Base (
  ST)
import Data.Array.Parallel.Distributed.Gang
import Data.Array.Parallel.Distributed.Types (
  DT(..), Dist, MDist)

import Monad (liftM)

-- | Data-parallel computations.
newtype DistST s a = DistST { unDistST :: Int -> ST s a }

instance Monad (DistST s) where
  return         = DistST . const . return 
  DistST p >>= f = DistST $ \i -> do
                                    x <- p i
                                    unDistST (f x) i

-- | Yields the index of the current thread within its gang.
myIndex :: DistST s Int
myIndex = DistST return

-- | Lifts an 'ST' computation into the 'DistST' monad. The lifted computation
-- should be data parallel.
distST :: ST s a -> DistST s a
distST p = DistST $ \i -> p

-- | Yields the 'Dist' element owned by the current thread.
myDT :: DT a => Dist a -> DistST s a
myDT dt = liftM (indexDT dt) myIndex

-- | Yields the 'MDist' element owned by the current thread.
readMyMDT :: DT a => MDist a s -> DistST s a
readMyMDT mdt = do
                     i <- myIndex
                     distST $ readMDT mdt i

-- | Writes the 'MDist' element owned by the current thread.
writeMyMDT :: DT a => MDist a s -> a -> DistST s ()
writeMyMDT mdt x = do
                     i <- myIndex
                     distST $ writeMDT mdt i x

-- | Runs a data-parallel computation on a 'Gang'. 
runDistST_ :: Gang -> DistST s () -> ST s ()
runDistST_ g = gangST g . unDistST

-- | Runs a data-parallel computation, yielding the distributed result.
runDistST :: DT a => Gang -> DistST s a -> ST s (Dist a)
runDistST g p =
  do
    mdt <- newMDT g
    runDistST_ g $ writeMyMDT mdt =<< p
    unsafeFreezeMDT mdt

