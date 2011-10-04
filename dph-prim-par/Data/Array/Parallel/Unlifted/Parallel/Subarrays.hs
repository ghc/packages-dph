{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Subarrays of flat unlifted arrays.
module Data.Array.Parallel.Unlifted.Parallel.Subarrays (
  dropUP
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq


-- | Drop a the element at the provided index from a vector.
dropUP :: Unbox e => Int -> Vector e -> Vector e
{-# INLINE_U dropUP #-}
dropUP n xs 
        = Seq.slice xs  (min (max 0 n)       (Seq.length xs))
                        (min (Seq.length xs) (Seq.length xs - n)) 
