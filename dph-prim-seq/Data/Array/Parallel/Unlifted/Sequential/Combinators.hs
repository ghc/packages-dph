{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Combinators (
  foldlSU,      foldlSSU,
  foldSU,       foldSSU,
  foldl1SU,     foldl1SSU,
  fold1SU,      fold1SSU,
  foldlRU,
  combineSU,
  
  unsafeExtractsFromNestedUSSegd,
  unsafeExtractsFromVectorsUSSegd
) where
import Data.Array.Parallel.Stream
import Data.Array.Parallel.Unlifted.Stream
import Data.Array.Parallel.Unlifted.Vectors                     as US
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Vector                                    as V
import qualified Data.Vector.Generic                            as G


-- foldl ----------------------------------------------------------------------
-- | Segmented array reduction proceeding from the left
foldlSU  :: (Unbox a, Unbox b)
         => (b -> a -> b) -> b -> USegd -> Vector a -> Vector b
{-# INLINE_U foldlSU #-}
foldlSU f z segd xs 
        = unstream
        $ foldSS f z    (stream (USegd.takeLengths segd))
                        (stream xs)

-- | Segmented array reduction proceeding from the left.
--   For scattered segments.
foldlSSU :: (Unbox a, Unboxes a, Unbox b)
         => (b -> a -> b) -> b -> USSegd -> Vectors a -> Vector b
{-# INLINE_U foldlSSU #-}
foldlSSU f z ssegd xss
        = unstream
        $ foldSS f z    (stream (USSegd.takeLengths ssegd))
                        (unsafeStreamSegsFromVectorsUSSegd xss ssegd)


-- fold -----------------------------------------------------------------------
-- | Segmented array reduction that requires an associative combination
--   function with its unit
foldSU  :: Unbox a
        => (a -> a -> a) -> a -> USegd -> Vector a -> Vector a
{-# INLINE_U foldSU #-}
foldSU = foldlSU


-- | Segmented array reduction that requires an associative combination
--   function with its unit. For scattered segments.
foldSSU :: (Unbox a, Unboxes a)
        => (a -> a -> a) -> a -> USSegd -> Vectors a -> Vector a
{-# INLINE_U foldSSU #-}
foldSSU = foldlSSU       


-- foldl1 ---------------------------------------------------------------------
-- | Segmented array reduction from left to right with non-empty subarrays only
foldl1SU :: Unbox a
         => (a -> a -> a) -> USegd -> Vector a -> Vector a
{-# INLINE_U foldl1SU #-}
foldl1SU f segd xs 
        = unstream
        $ fold1SS f     (stream (USegd.takeLengths segd))
                        (stream xs)


-- | Segmented array reduction from left to right with non-empty subarrays only.
--   For scattered segments.
foldl1SSU :: (Unbox a, Unboxes a)
          => (a -> a -> a) -> USSegd -> Vectors a -> Vector a
{-# INLINE_U foldl1SSU #-}
foldl1SSU f ssegd xxs
        = unstream
        $ fold1SS f     (stream (USSegd.takeLengths ssegd))
                        (unsafeStreamSegsFromVectorsUSSegd xxs ssegd)


-- fold1 ----------------------------------------------------------------------
-- | Segmented array reduction with non-empty subarrays and an associative
--   combination function.
fold1SU :: Unbox a
        => (a -> a -> a) -> USegd -> Vector a -> Vector a
{-# INLINE_U fold1SU #-}
fold1SU = foldl1SU


-- | Segmented array reduction with non-empty subarrays and an associative
--   combination function. For scattered segments.
fold1SSU :: (Unbox a, Unboxes a)
        => (a -> a -> a) -> USSegd -> Vectors a -> Vector a
{-# INLINE_U fold1SSU #-}
fold1SSU = foldl1SSU



-- foldlR ---------------------------------------------------------------------
-- | Regular arrar reduction 
foldlRU :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
{-# INLINE_U foldlRU #-}
foldlRU f z segSize
        = unstream . foldValuesR f z segSize . stream


-- | Merge two segmented arrays according to flag array
combineSU :: Unbox a => Vector Bool -> USegd -> Vector a -> USegd -> Vector a -> Vector a
{-# INLINE_U combineSU #-}
combineSU bs xd xs yd ys
        = unstream
        $ combineSS (stream bs)
                    (stream (USegd.takeLengths xd)) (stream xs)
                    (stream (USegd.takeLengths yd)) (stream ys)



-- Extracts wrappers ---------------------------------------------------------
-- | Copy segments from a `Vectors` and concatenate them into a new array.
unsafeExtractsFromNestedUSSegd
        :: (U.Unbox a)
        => USSegd -> V.Vector (Vector a) -> U.Vector a

unsafeExtractsFromNestedUSSegd ussegd vectors
        = G.unstream $ unsafeStreamSegsFromNestedUSSegd vectors ussegd
{-# INLINE_U unsafeExtractsFromNestedUSSegd #-}


-- | Copy segments from a `Vectors` and concatenate them into a new array.
unsafeExtractsFromVectorsUSSegd
        :: (Unboxes a, U.Unbox a)
        => USSegd -> Vectors a -> U.Vector a

unsafeExtractsFromVectorsUSSegd ussegd vectors
        = G.unstream $ unsafeStreamSegsFromVectorsUSSegd vectors ussegd
{-# INLINE_U unsafeExtractsFromVectorsUSSegd #-}

