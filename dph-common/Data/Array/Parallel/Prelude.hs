module Data.Array.Parallel.Prelude (
    module Data.Array.Parallel

  , module Data.Array.Parallel.Prelude.Bool

  , PArray, Scalar(..), toUArrPA 
                      , fromUArrPA', fromUArrPA_2', fromUArrPA_3, fromUArrPA_3'
                      , nestUSegdPA'
) where

import Data.Array.Parallel hiding (sumP, productP, maximumP, minimumP, enumFromToP,
  enumFromThenToP)

import Data.Array.Parallel.Prelude.Bool

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Scalar

