module DotPPar where

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel

dotp :: UArr Double -> UArr Double -> Double
{-# NOINLINE dotp #-}
dotp v w = sumUP (zipWithUP (*) v w)

