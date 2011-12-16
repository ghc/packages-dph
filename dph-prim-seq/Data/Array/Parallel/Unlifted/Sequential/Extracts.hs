{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Extracts
        ( extractsFromNestedUSSegd
        , extractsFromVectorsUSSegd)
where
import Data.Array.Parallel.Unlifted.Stream
import Data.Array.Parallel.Unlifted.Vectors                     as US
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import qualified Data.Vector                                    as V
import qualified Data.Vector.Generic                            as G


-- Extracts wrappers ---------------------------------------------------------
-- | Copy segments from a `Vectors` and concatenate them into a new array.
extractsFromNestedUSSegd
        :: (U.Unbox a)
        => USSegd -> V.Vector (Vector a) -> U.Vector a

extractsFromNestedUSSegd ussegd vectors
        = G.unstream $ streamSegsFromNestedUSSegd vectors ussegd
{-# INLINE_U extractsFromNestedUSSegd #-}


-- | Copy segments from a `Vectors` and concatenate them into a new array.
extractsFromVectorsUSSegd
        :: (Unboxes a, U.Unbox a)
        => USSegd -> Vectors a -> U.Vector a

extractsFromVectorsUSSegd ussegd vectors
        = G.unstream $ streamSegsFromVectorsUSSegd vectors ussegd
{-# INLINE_U extractsFromVectorsUSSegd #-}
