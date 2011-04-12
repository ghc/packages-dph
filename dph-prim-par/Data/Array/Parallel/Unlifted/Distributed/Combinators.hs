{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
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

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Distributed.Combinators (
  generateD, generateD_cheap,
  imapD, mapD, zipD, unzipD, fstD, sndD, zipWithD, izipWithD,
  foldD, scanD, mapAccumLD,

  -- * Monadic combinators
  mapDST_, mapDST, zipWithDST_, zipWithDST
) where

import Data.Array.Parallel.Base ( ST, runST)
import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, gangSize)
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, MDist, indexD, zipD, unzipD, fstD, sndD, deepSeqD,
  newMD, writeMD, unsafeFreezeMD,
  checkGangD, measureD, debugD)
import Data.Array.Parallel.Unlifted.Distributed.DistST

import Debug.Trace

here s = "Data.Array.Parallel.Unlifted.Distributed.Combinators." ++ s

-- | Create a distributed value, given a function that makes the value in each thread.
generateD :: DT a => Gang -> (Int -> a) -> Dist a
{-# NOINLINE generateD #-}
generateD g f 
 	= trace "generate full")
 	$ runDistST g (myIndex >>= return . f)


-- | Create a distributed value, but run it sequentially (I think?)
generateD_cheap :: DT a => Gang -> (Int -> a) -> Dist a
{-# NOINLINE generateD_cheap #-}
generateD_cheap g f 
	= trace "generate cheap"
	$ runDistST_seq g (myIndex >>= return . f)


-- Mapping --------------------------------------------------------------------
-- | Map a function across all elements of a distributed value.
--   The worker function also gets the current thread index.
--   As opposed to `imapD'` this version also deepSeqs each element before
--   passing it to the function.
imapD :: (DT a, DT b) => Gang -> (Int -> a -> b) -> Dist a -> Dist b
{-# INLINE [0] imapD #-}
imapD g f d = imapD' g (\i x -> x `deepSeqD` f i x) d


-- | Map a function across all elements of a distributed value.
--   The worker function also gets the current thread index.
imapD' :: (DT a, DT b) => Gang -> (Int -> a -> b) -> Dist a -> Dist b
{-# NOINLINE imapD' #-}
imapD' g f !d = checkGangD (here "imapD") g d
                (runDistST g (do
                                i <- myIndex
                                x <- myD d
                                return (f i x)))


-- | Map a function over a distributed value.
mapD :: (DT a, DT b) => Gang -> (a -> b) -> Dist a -> Dist b
{-# INLINE mapD #-}
mapD g = imapD g . const

{-# RULES

"imapD/generateD" forall gang f g.
  imapD gang f (generateD gang g) = generateD gang (\i -> f i (g i))

"imapD/generateD_cheap" forall gang f g.
  imapD gang f (generateD_cheap gang g) = generateD gang (\i -> f i (g i))

"imapD/imapD" forall gang f g d.
  imapD gang f (imapD gang g d) = imapD gang (\i x -> f i (g i x)) d

  #-}

{- RULES

"mapD/generateD"
  mapD gang f (generateD gang g) = generateD gang (\x -> f (g x))

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
  -}


-- Zipping --------------------------------------------------------------------
-- | Combine two distributed values with the given function.
zipWithD :: (DT a, DT b, DT c)
         => Gang -> (a -> b -> c) -> Dist a -> Dist b -> Dist c
{-# INLINE zipWithD #-}
zipWithD g f dx dy = mapD g (uncurry f) (zipD dx dy)


-- | Combine two distributed values with the given function.
--   The worker function also gets the index of the current thread.
izipWithD :: (DT a, DT b, DT c)
          => Gang -> (Int -> a -> b -> c) -> Dist a -> Dist b -> Dist c
{-# INLINE izipWithD #-}
izipWithD g f dx dy = imapD g (\i -> uncurry (f i)) (zipD dx dy)

{-# RULES
"zipD/imapD[1]" forall gang f xs ys.
  zipD (imapD gang f xs) ys
    = imapD gang (\i (x,y) -> (f i x,y)) (zipD xs ys)

"zipD/imapD[2]" forall gang f xs ys.
  zipD xs (imapD gang f ys)
    = imapD gang (\i (x,y) -> (x, f i y)) (zipD xs ys)

"zipD/generateD[1]" forall gang f xs.
  zipD (generateD gang f) xs
    = imapD gang (\i x -> (f i, x)) xs

"zipD/generateD[2]" forall gang f xs.
  zipD xs (generateD gang f)
    = imapD gang (\i x -> (x, f i)) xs

  #-}


-- Folding --------------------------------------------------------------------
-- | Fold a distributed value.
foldD :: DT a => Gang -> (a -> a -> a) -> Dist a -> a
-- {-# INLINE_DIST foldD #-}
{-# NOINLINE foldD #-}
foldD g f !d = checkGangD ("here foldD") g d $
              fold 1 (d `indexD` 0)
  where
    !n = gangSize g
    --
    fold i x | i == n    = x
             | otherwise = fold (i+1) (f x $ d `indexD` i)


-- | Prefix sum of a distributed value.
scanD :: forall a. DT a => Gang -> (a -> a -> a) -> a -> Dist a -> (Dist a, a)
{-# NOINLINE scanD #-}
scanD g f z !d = checkGangD (here "scanD") g d $
                 runST (do
                   md <- newMD g
                   s  <- scan md 0 z
                   d' <- unsafeFreezeMD md
                   return (d',s))
  where
    !n = gangSize g
    scan :: forall s. MDist a s -> Int -> a -> ST s a
    scan md i !x | i == n    = return x
                 | otherwise = do
                                 writeMD md i x
                                 scan md (i+1) (f x $ d `indexD` i)

-- | Combination of map and fold.
mapAccumLD :: forall a b acc. (DT a, DT b)
           => Gang -> (acc -> a -> (acc,b))
                   -> acc -> Dist a -> (acc,Dist b)
{-# INLINE_DIST mapAccumLD #-}
mapAccumLD g f acc !d = checkGangD (here "mapAccumLD") g d $
                        runST (do
                          md   <- newMD g
                          acc' <- go md 0 acc
                          d'   <- unsafeFreezeMD md
                          return (acc',d'))
  where
    !n = gangSize g
    go :: MDist b s -> Int -> acc -> ST s acc
    go md i acc | i == n    = return acc
                | otherwise = case f acc (d `indexD` i) of
                                (acc',b) -> do
                                              writeMD md i b
                                              go md (i+1) acc'
                                

-- Versions that work on DistST -----------------------------------------------
-- NOTE: The following combinators must be strict in the Dists because if they
-- are not, the Dist might be evaluated (in parallel) when it is requested in
-- the current computation which, again, is parallel. This would break our
-- model andlead to a deadlock. Hence the bangs.

mapDST_ :: DT a => Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
{-# INLINE mapDST_ #-}
mapDST_ g p d = mapDST_' g (\x -> x `deepSeqD` p x) d

mapDST_' :: DT a => Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
mapDST_' g p !d = checkGangD (here "mapDST_") g d $
                  distST_ g (myD d >>= p)

mapDST :: (DT a, DT b) => Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
{-# INLINE mapDST #-}
mapDST g p !d = mapDST' g (\x -> x `deepSeqD` p x) d

mapDST' :: (DT a, DT b) => Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
mapDST' g p !d = checkGangD (here "mapDST_") g d $
                 distST g (myD d >>= p)

zipWithDST_ :: (DT a, DT b)
            => Gang -> (a -> b -> DistST s ()) -> Dist a -> Dist b -> ST s ()
{-# INLINE zipWithDST_ #-}
zipWithDST_ g p !dx !dy = mapDST_ g (uncurry p) (zipD dx dy)

zipWithDST :: (DT a, DT b, DT c)
           => Gang
           -> (a -> b -> DistST s c) -> Dist a -> Dist b -> ST s (Dist c)
{-# INLINE zipWithDST #-}
zipWithDST g p !dx !dy = mapDST g (uncurry p) (zipD dx dy)

