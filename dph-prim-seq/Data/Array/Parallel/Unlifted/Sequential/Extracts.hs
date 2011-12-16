{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Extracts
        ( -- * Scattered indexing.
          indexsFromVectorsUVSegd

          -- * Scattered extracts
        , extractsFromNestedUSSegd
        , extractsFromVectorsUSSegd
        , extractsFromVectorsUVSegd)
where
import Data.Array.Parallel.Unlifted.Stream                      as US
import Data.Array.Parallel.Unlifted.Vectors                     as US
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.UVSegd           (UVSegd)
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd as UVSegd
import qualified Data.Vector                                    as V


-- Indexs --------------------------------------------------------------------
-- | Lookup elements from some `Vectors` through a `UPVSegd`.
indexsFromVectorsUVSegd 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UVSegd -> Vector (Int, Int) -> Vector a

indexsFromVectorsUVSegd vectors uvsegd vsrcixs
 = let  -- Because we're just doing indexing here, we don't need the culled
        -- vsegids or ussegd, and can just use the redundant version.
        !vsegids  = UVSegd.takeVSegidsRedundant uvsegd
        !ussegd   = UVSegd.takeUSSegdRedundant  uvsegd
   in   U.unstream
         $ US.streamElemsFromVectors     vectors
         $ US.streamSrcIxsThroughUSSegd  ussegd
         $ US.streamSrcIxsThroughVSegids vsegids
         $ U.stream vsrcixs
{-# INLINE_U indexsFromVectorsUVSegd #-}


-- Extracts wrappers ---------------------------------------------------------
-- | Copy segments from a `Vectors`, concatenating them into a new array.
extractsFromNestedUSSegd
        :: (U.Unbox a)
        => USSegd -> V.Vector (Vector a) -> U.Vector a

extractsFromNestedUSSegd ussegd vectors
        = U.unstream $ streamSegsFromNestedUSSegd vectors ussegd
{-# INLINE_U extractsFromNestedUSSegd #-}


-- | Copy segments from a `Vectors`, concatenating them into a new array.
extractsFromVectorsUSSegd
        :: (Unboxes a, U.Unbox a)
        => USSegd -> Vectors a -> U.Vector a

extractsFromVectorsUSSegd ussegd vectors
        = U.unstream $ streamSegsFromVectorsUSSegd vectors ussegd
{-# INLINE_U extractsFromVectorsUSSegd #-}


-- | Copy segments from a `Vectors`, concatenating them into a new array.
extractsFromVectorsUVSegd
        :: (Unbox a, US.Unboxes a)
        => UVSegd
        -> Vectors a
        -> Vector a

extractsFromVectorsUVSegd uvsegd vectors
        = U.unstream  $ US.streamSegsFromVectorsUVSegd vectors uvsegd
{-# INLINE_U extractsFromVectorsUVSegd #-}
