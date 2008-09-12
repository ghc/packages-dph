{-# OPTIONS -fvectorise #-}

module Data.Array.Parallel.Prelude.Double (
  Double,
  (==), (/=), (<=), (<), (>=), (>), min, max,
  minimumP, maximumP, minIndexP, maxIndexP,
  (+), (-), (*), negate, abs, sumP, productP,
  (/), recip,
  pi, exp, sqrt, log, (**), logBase,
  sin, tan, cos, asin, atan, acos,
  sinh, tanh, cosh, asinh, atanh, acosh,

  fromInt, truncate, round, ceiling, floor
) where

import Data.Array.Parallel.Prelude.Base.Double
import qualified Data.Array.Parallel.Prelude.Base
import Prelude (Double)

