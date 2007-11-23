{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module DotPVect where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double

dotp :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp #-}
dotp v w = dotp' (fromPArrayP v) (fromPArrayP w)

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' v w = sumP (zipWithP mult v w)

