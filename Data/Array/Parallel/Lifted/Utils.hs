module Data.Array.Parallel.Lifted.Utils (
  intEqPA
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Instances

import Data.Array.Parallel.Unlifted

intEqPA :: PArray Int -> Int -> UArr Bool
intEqPA is !n = mapU (n==) (intPayload is)

