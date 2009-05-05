{-# OPTIONS -fvectorise #-}
module Data.Array.Parallel.Prelude.Int (
  Int,
  (==), (/=), (<=), (<), (>=), (>), min, max,
  minimumP, maximumP, minIndexP, maxIndexP,
  (+), (-), (*), negate, abs, sumP, productP,
  div, mod, sqrt, enumFromToP
) where

import Data.Array.Parallel.Prelude.Base.Int
import qualified Data.Array.Parallel.Prelude.Base
import Prelude (Int)

