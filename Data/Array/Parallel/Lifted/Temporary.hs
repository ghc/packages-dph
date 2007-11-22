module Data.Array.Parallel.Lifted.Temporary (
  unsafe_zipWith_Int,
  unsafe_fold_Int,
  upto_Int,

  unsafe_zipWith_Double, unsafe_fold_Double
) where

import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Lifted.PArray

import Data.Array.Parallel.Unlifted

import GHC.Exts (Int(..))

unsafe_zipWith_Int# :: (Int -> Int -> Int)
                    -> PArray_Int# -> PArray_Int# -> PArray_Int#
{-# INLINE unsafe_zipWith_Int# #-}
unsafe_zipWith_Int# f (PInt# ms) (PInt# ns) = PInt# (zipWithU f ms ns)

unsafe_zipWith_Int :: (Int -> Int -> Int)
                   -> PArray Int -> PArray Int -> PArray Int
{-# INLINE unsafe_zipWith_Int #-}
unsafe_zipWith_Int f (PInt m# ms) (PInt n# ns)
  = PInt m# (unsafe_zipWith_Int# f ms ns)

unsafe_fold_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> Int
{-# INLINE unsafe_fold_Int# #-}
unsafe_fold_Int# f z (PInt# ns) = foldU f z ns

unsafe_fold_Int :: (Int -> Int -> Int) -> Int -> PArray Int -> Int
{-# INLINE unsafe_fold_Int #-}
unsafe_fold_Int f z (PInt n# ns) = unsafe_fold_Int# f z ns

upto_Int :: Int -> PArray Int
{-# INLINE upto_Int #-}
upto_Int (I# n#) = PInt n# (upToPA_Int# n#)


unsafe_zipWith_Double# :: (Double -> Double -> Double)
                    -> PArray_Double# -> PArray_Double# -> PArray_Double#
{-# INLINE unsafe_zipWith_Double# #-}
unsafe_zipWith_Double# f (PDouble# ms) (PDouble# ns) = PDouble# (zipWithU f ms ns)

unsafe_zipWith_Double :: (Double -> Double -> Double)
                   -> PArray Double -> PArray Double -> PArray Double
{-# INLINE unsafe_zipWith_Double #-}
unsafe_zipWith_Double f (PDouble m# ms) (PDouble n# ns)
  = PDouble m# (unsafe_zipWith_Double# f ms ns)

unsafe_fold_Double# :: (Double -> Double -> Double) -> Double -> PArray_Double# -> Double
{-# INLINE unsafe_fold_Double# #-}
unsafe_fold_Double# f z (PDouble# ns) = foldU f z ns

unsafe_fold_Double :: (Double -> Double -> Double) -> Double -> PArray Double -> Double
{-# INLINE unsafe_fold_Double #-}
unsafe_fold_Double f z (PDouble n# ns) = unsafe_fold_Double# f z ns

