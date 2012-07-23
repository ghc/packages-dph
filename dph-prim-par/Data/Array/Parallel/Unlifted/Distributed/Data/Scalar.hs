-- | Distribution of values of primitive types.
module Data.Array.Parallel.Unlifted.Distributed.Data.Scalar
        ( DT(..), Dist(..)
        , scalarD
        , sumD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.Scalar.Base
import Data.Array.Parallel.Unlifted.Distributed.Data.Unit
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Prelude as P

-- | Distribute a scalar.
--   Each thread gets its own copy of the same value.
--   Example:  scalarD theGangN4 10 = [10, 10, 10, 10] 
scalarD :: DT a => Gang -> a -> Dist a
scalarD gang x 
        = mapD WhatScalar gang (const x) (unitD gang)


-- | Sum all instances of a distributed number.
sumD :: (Num a, DT a) => Gang -> Dist a -> a
sumD g  = foldD g (+)
