module DotPSeq where

import Data.Array.Parallel.Unlifted

dotp :: UArr Double -> UArr Double -> Double
{-# NOINLINE dotp #-}
dotp v w = sumU (zipWithU (*) v w)

