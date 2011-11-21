{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Basic segmented operations on unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Basics (
  replicateSU, replicateRSU, appendSU, indicesSU, indicesSU'
) where
import Data.Array.Parallel.Stream
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Data.Array.Parallel.Unlifted.Sequential.USegd            (USegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Vector.Fusion.Stream as S


-- | Segmented replicate of a vector based on the lengths of the segments
--   of the provided `USegd`.
replicateSU :: Unbox a => USegd -> Vector a -> Vector a
{-# INLINE_U replicateSU #-}
replicateSU segd xs 
        = unstream
             (replicateEachS (USegd.takeElements segd)
             (S.zip (stream (USegd.takeLengths segd)) (stream xs)))


-- | Regular sgemented replicate.
replicateRSU :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE_U replicateRSU #-}
replicateRSU n xs
        = unstream
        . replicateEachRS n
        $ stream xs
                  

-- | Segmented append.
appendSU :: Unbox a 
         => USegd -> Vector a   -- segd/data of first array
         -> USegd -> Vector a   -- segd/data of second array
         -> Vector a
{-# INLINE_U appendSU #-}
appendSU xd xs yd ys
        = unstream
        $ appendSS (stream (USegd.takeLengths xd)) (stream xs)
                   (stream (USegd.takeLengths yd)) (stream ys)


-- | Segmented indices.
indicesSU :: USegd -> Vector Int
{-# INLINE_U indicesSU #-}
indicesSU = indicesSU' 0

indicesSU' :: Int -> USegd -> Vector Int
{-# INLINE_U indicesSU' #-}
indicesSU' i segd
        = unstream
        . indicesSS (USegd.takeElements segd) i
        . stream
        $ USegd.takeLengths segd

