
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module SMVMVectorised (smvmPA) where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int    as I

import qualified Prelude as P

smvmPA :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
{-# NOINLINE smvmPA #-}
smvmPA m v = toPArrayP (smvm (fromNestedPArrayP m) (fromPArrayP v))

smvm :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm m v = [: D.sumP [: x D.* (v !: i) | (i,x) <- row :] | row <- m :]

