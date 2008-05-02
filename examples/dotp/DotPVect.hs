{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module DotPVect where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double

import qualified Prelude

dotp :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp #-}
dotp v w = dotp' (fromPArrayP v) (fromPArrayP w)

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' v w = doubleSumP (zipWithP (*) v w)

