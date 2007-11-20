module Data.Array.Parallel.Prelude (
    module Data.Array.Parallel.Prelude.Base.PArr
  -- , module Data.Array.Parallel.Prelude.Base.Int
  
  , PArray, fromPArrayP, fromPArrayPA
) where

import Data.Array.Parallel.Prelude.Base.PArr
import Data.Array.Parallel.Prelude.Base.Int

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators


