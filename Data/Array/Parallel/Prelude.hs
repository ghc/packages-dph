module Data.Array.Parallel.Prelude (
    module Data.Array.Parallel.Prelude.Base.PArr

  , module Data.Array.Parallel.Prelude.Bool
  
  , PArray, PrimPA(..), fromSUArrPA, fromSUArrPA_2
) where

import Data.Array.Parallel.Prelude.Bool

import Data.Array.Parallel.Prelude.Base.PArr
import Data.Array.Parallel.Prelude.Base.Int

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances


