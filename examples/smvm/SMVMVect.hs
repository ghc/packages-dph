{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module SMVMVect (smvm) where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import Data.Array.Parallel.Prelude.Int (Int)

import qualified Prelude

smvm :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
{-# NOINLINE smvm #-}
smvm m v = toPArrayP (smvm' (fromNestedPArrayP m) (fromPArrayP v))

smvm' :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm' m v = [: doubleSumP [: x * (v !: i) | (i,x) <- row :] | row <- m :]

