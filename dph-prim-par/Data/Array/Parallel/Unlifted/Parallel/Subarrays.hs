{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Subarrays of flat unlifted arrays.
module Data.Array.Parallel.Unlifted.Parallel.Subarrays (
  dropUP
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Array.Parallel.Unlifted.Distributed


dropUP :: Unbox e => Int -> Vector e -> Vector e
dropUP n xs = Seq.slice xs (min (max 0 n) (Seq.length xs)) (min (Seq.length xs) (Seq.length xs - n)) 
{-# INLINE_U dropUP #-}
