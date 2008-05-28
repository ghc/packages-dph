{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Prim
where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances

import Data.Array.Parallel.Base (fstS, pairS, unpairS)

unsafe_map :: (PrimPA a, PrimPA b) => (a -> b) -> PArray a -> PArray b
{-# INLINE_PA unsafe_map #-}
unsafe_map f xs = fromUArrPA (prim_lengthPA xs)
                . mapU f
                $ toUArrPA xs

unsafe_zipWith :: (PrimPA a, PrimPA b, PrimPA c)
               => (a -> b -> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA unsafe_zipWith #-}
unsafe_zipWith f xs ys = fromUArrPA (prim_lengthPA xs)
                       $ zipWithU f (toUArrPA xs) (toUArrPA ys)

unsafe_fold :: PrimPA a => (a -> a -> a) -> a -> PArray a -> a
{-# INLINE_PA unsafe_fold #-}
unsafe_fold f z = foldU f z . toUArrPA

unsafe_fold1 :: PrimPA a => (a -> a -> a) -> PArray a -> a
{-# INLINE_PA unsafe_fold1 #-}
unsafe_fold1 f = fold1U f . toUArrPA

unsafe_folds :: PrimPA a => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a
{-# INLINE_PA unsafe_folds #-}
unsafe_folds f z xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                     . foldSU f z
                     $ toSUArrPA xss

unsafe_fold1s :: PrimPA a => (a -> a -> a) -> PArray (PArray a) -> PArray a
{-# INLINE_PA unsafe_fold1s #-}
unsafe_fold1s f xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                    . fold1SU f
                    $ toSUArrPA xss

unsafe_fold1Index :: PrimPA a
                  => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int
{-# INLINE_PA unsafe_fold1Index #-}
unsafe_fold1Index f = fstS . fold1U f' . indexedU . toUArrPA
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)

unsafe_fold1sIndex :: PrimPA a
                   => ((Int, a) -> (Int, a) -> (Int, a))
                   -> PArray (PArray a) -> PArray Int
{-# INLINE_PA unsafe_fold1sIndex #-}
unsafe_fold1sIndex f xss = fromUArrPA (nested_lengthPA xss) 
                         . fstU
                         . fold1SU f'
                         . indexedSU
                         $ toSUArrPA xss
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)



unsafe_enumFromTo:: Int -> Int -> PArray Int
{-# INLINE_PA unsafe_enumFromTo #-}
unsafe_enumFromTo s e = fromUArrPA  (max 0 (e-s+1)) $ enumFromToU s e

unsafe_enumFromTos:: PArray Int -> PArray Int -> PArray (PArray Int)
{-# INLINE_PA unsafe_enumFromTos #-}
unsafe_enumFromTos ss es = fromSUArrPA  flatLen nestedLen  $ enumFromToSU (toUArrPA ss) (toUArrPA es)
  where
    flatLen   = prim_lengthPA ss
    nestedLen = unsafe_fold (+) 0 (unsafe_map (\x -> max (x+1) 0) $  unsafe_zipWith (-) es ss)
