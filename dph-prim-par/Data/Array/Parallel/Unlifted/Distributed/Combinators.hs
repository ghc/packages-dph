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
-- Standard combinators for distributed types.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Distributed.Combinators (
  mapD, zipD, unzipD, fstD, sndD, zipWithD,
  foldD, scanD, mapAccumLD,

  -- * Monadic combinators
  mapDST_, mapDST, zipWithDST_, zipWithDST
) where

import Data.Array.Parallel.Base (
  (:*:)(..), uncurryS, unsafe_pairS, unsafe_unpairS, ST, runST)
import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize)
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, indexD, zipD, unzipD, fstD, sndD,
  newMD, writeMD, unsafeFreezeMD,
  checkGangD)
import Data.Array.Parallel.Unlifted.Distributed.DistST (
  DistST, distST_, distST, runDistST, myD)

here s = "Data.Array.Parallel.Unlifted.Distributed.Combinators." ++ s

-- | Map a function over a distributed value.
mapD :: (DT a, DT b) => Gang -> (a -> b) -> Dist a -> Dist b
{-# INLINE [1] mapD #-}
mapD g f !d = checkGangD (here "mapD") g d
             (runDistST g (myD d >>= return . f))

{-# RULES

"mapD/mapD" forall gang f g d.
  mapD gang f (mapD gang g d) = mapD gang (\x -> f (g x)) d

"zipD/mapD[1]" forall gang f xs ys.
  zipD (mapD gang f xs) ys
    = mapD gang (unsafe_pairS . (\(xs, ys) -> (f xs, ys)) . unsafe_unpairS)
                (zipD xs ys)

"zipD/mapD[2]" forall gang f xs ys.
  zipD xs (mapD gang f ys)
    = mapD gang (unsafe_pairS . (\(xs, ys) -> (xs, f ys)) . unsafe_unpairS)
                (zipD xs ys)

  #-}

-- zipD, unzipD, fstD, sndD reexported from Types

-- | Combine two distributed values with the given function.
zipWithD :: (DT a, DT b, DT c)
         => Gang -> (a -> b -> c) -> Dist a -> Dist b -> Dist c
{-# INLINE zipWithD #-}
zipWithD g f dx dy = mapD g (uncurry f . unsafe_unpairS) (zipD dx dy)

-- | Fold a distributed value.
foldD :: DT a => Gang -> (a -> a -> a) -> Dist a -> a
-- {-# INLINE_DIST foldD #-}
{-# NOINLINE foldD #-}
foldD g f !d = checkGangD ("here foldD") g d $
              fold 1 (d `indexD` 0)
  where
    n = gangSize g
    --
    fold i x | i == n    = x
             | otherwise = fold (i+1) (f x $ d `indexD` i)

-- | Prefix sum of a distributed value.
scanD :: DT a => Gang -> (a -> a -> a) -> a -> Dist a -> Dist a :*: a
{-# INLINE_DIST scanD #-}
scanD g f z !d = checkGangD (here "scanD") g d $
                 runST (do
                   md <- newMD g
                   s  <- scan md 0 z
                   d' <- unsafeFreezeMD md
                   return (d' :*: s))
  where
    n = gangSize g
    --
    scan md i x | i == n    = return x
                | otherwise = do
                                writeMD md i x
                                scan md (i+1) (f x $ d `indexD` i)

mapAccumLD :: (DT a, DT b) => Gang -> (acc -> a -> acc :*: b)
                                   -> acc -> Dist a -> acc :*: Dist b
{-# INLINE_DIST mapAccumLD #-}
mapAccumLD g f acc !d = checkGangD (here "mapAccumLD") g d $
                        runST (do
                          md   <- newMD g
                          acc' <- go md 0 acc
                          d'   <- unsafeFreezeMD md
                          return (acc' :*: d'))
  where
    n = gangSize g

    go md i acc | i == n    = return acc
                | otherwise = case f acc (d `indexD` i) of
                                acc' :*: b -> do
                                                writeMD md i b
                                                go md (i+1) acc'
                                
-- NOTE: The following combinators must be strict in the Dists because if they
-- are not, the Dist might be evaluated (in parallel) when it is requested in
-- the current computation which, again, is parallel. This would break our
-- model andlead to a deadlock. Hence the bangs.

mapDST_ :: DT a => Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
mapDST_ g p !d = checkGangD (here "mapDST_") g d $
                 distST_ g (myD d >>= p)

mapDST :: (DT a, DT b) => Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
mapDST g p !d = checkGangD (here "mapDST_") g d $
                distST g (myD d >>= p)

zipWithDST_ :: (DT a, DT b)
            => Gang -> (a -> b -> DistST s ()) -> Dist a -> Dist b -> ST s ()
zipWithDST_ g p !dx !dy = mapDST_ g (uncurryS p) (zipD dx dy)

zipWithDST :: (DT a, DT b, DT c)
           => Gang
           -> (a -> b -> DistST s c) -> Dist a -> Dist b -> ST s (Dist c)
zipWithDST g p !dx !dy = mapDST g (uncurryS p) (zipD dx dy)

