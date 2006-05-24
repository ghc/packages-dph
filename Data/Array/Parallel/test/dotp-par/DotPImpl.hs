module DotPImpl where

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel

-- Benchmarked code
-- ----------------

type Vector = UArr Double

-- !!!FIXME: Fusion doesn't seem to work properly; it's a factor 2 too slow.
dotp :: Vector -> Vector -> Double
{-# NOINLINE dotp #-}
dotp v w = sumUP (zipWithUP (*) v w)

{-
dotp_fused :: Vector -> Vector -> Double
dotp_fused v w =   loopAcc
                 . loopU (\a (x:*:y) -> (a + x * y :*: (Nothing::Maybe ()))) 0
	         $ zipU v w
-}

