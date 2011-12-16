{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Enum-related parallel operations on unlifted arrays
module Data.Array.Parallel.Unlifted.Parallel.Enum (
  enumFromToUP, enumFromThenToUP, enumFromStepLenUP, enumFromStepLenEachUP    
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP)
import GHC.Base ( divInt )


delay_inline :: a -> a
{-# INLINE [0] delay_inline #-}
delay_inline x = x


enumFromToUP :: (Unbox a, Enum a) => a -> a -> Vector a
{-# INLINE_UP enumFromToUP #-}
enumFromToUP start end 
 = mapUP toEnum (enumFromStepLenUP start' 1 len)
 where  start' = fromEnum start
        end'   = fromEnum end
        len    = delay_inline max (end' - start' + 1) 0


enumFromThenToUP :: (Unbox a, Enum a) => a -> a -> a -> Vector a
{-# INLINE_UP enumFromThenToUP #-}
enumFromThenToUP start next end 
 = mapUP toEnum (enumFromStepLenUP start' delta len)
 where  start' = fromEnum start
        next'  = fromEnum next
        end'   = fromEnum end
        delta  = next' - start'

        -- distance between start' and end' expressed in deltas
        dist   = (end' - start' + delta) `divInt` delta
        len    = max dist 0


enumFromStepLenUP :: Int -> Int -> Int -> Vector Int
{-# INLINE_UP enumFromStepLenUP #-}
enumFromStepLenUP start delta len =
  joinD theGang balanced
  (mapD theGang gen
  (splitLenIdxD theGang len))
  where
    gen (n,i) = Seq.enumFromStepLen (i * delta + start) delta n


enumFromStepLenEachUP :: Int -> Vector Int -> Vector Int -> Vector Int -> Vector Int
{-# INLINE_UP enumFromStepLenEachUP #-}
enumFromStepLenEachUP _n starts steps lens
  = joinD theGang unbalanced
  $ mapD theGang enum
  $ splitD theGang unbalanced (Seq.zip (Seq.zip starts steps) lens)
  where
    enum ps = let (qs, llens) = Seq.unzip ps
                  (lstarts, lsteps) = Seq.unzip qs
              in Seq.enumFromStepLenEach (Seq.sum llens) lstarts lsteps llens

