{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.Combinators (
  foldlSU, foldSU, foldl1SU, fold1SU, {-scanSU,-} {-scan1SU,-}
  foldlRU,
  combineSU
) where
import Data.Array.Parallel.Stream (
  foldSS, fold1SS, combineSS, foldValuesR )
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Debug.Trace


-- | Segmented array reduction proceeding from the left
foldlSU :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> USegd -> Vector a -> Vector b
{-# INLINE_U foldlSU #-}
foldlSU f z segd xs 
        = unstream
         $ foldSS f z (stream (lengthsUSegd segd)) (stream xs)


-- | Segmented array reduction that requires an associative combination
--   function with its unit
foldSU :: Unbox a => (a -> a -> a) -> a -> USegd -> Vector a -> Vector a
foldSU = foldlSU


-- | Segmented array reduction from left to right with non-empty subarrays only
foldl1SU :: Unbox a => (a -> a -> a) -> USegd -> Vector a -> Vector a
{-# INLINE_U foldl1SU #-}
foldl1SU f segd xs 
        = unstream
        $ fold1SS f (stream (lengthsUSegd segd)) (stream xs)


-- | Segmented array reduction with non-empty subarrays and an associative
--   combination function
fold1SU :: Unbox a => (a -> a -> a) -> USegd -> Vector a -> Vector a
fold1SU = foldl1SU


-- | Merge two segmented arrays according to flag array
combineSU :: Unbox a => Vector Bool -> USegd -> Vector a -> USegd -> Vector a -> Vector a
{-# INLINE_U combineSU #-}
combineSU bs xd xs yd ys
        = unstream
        $ combineSS (stream bs)
                    (stream (lengthsUSegd xd)) (stream xs)
                    (stream (lengthsUSegd yd)) (stream ys)


-- | Regular arrar reduction 
foldlRU :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
{-# INLINE_U foldlRU #-}
foldlRU f z segSize
        = unstream . foldValuesR f z segSize . stream

