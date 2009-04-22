{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module SMVMVect (smvm) where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int    as I

import qualified Prelude as P

smvm :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
{-# NOINLINE smvm #-}
smvm m v = toPArrayP (smvm' (fromNestedPArrayP m) (fromPArrayP v))

smvm' :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm' m v = [: D.sumP [: x D.* (v !: i) | (i,x) <- row :] | row <- m :]
--smvm' m v = mapP (\row -> D.sumP (mapP (\(i, x) -> x D.* (v !: i)) row)) m

{-
dotp :: PArray Double -> PArray (Int, Double) -> Double
{-# NOINLINE dotp #-}
dotp v row = dotp' (fromPArrayP v) (fromPArrayP row)

dotp' :: [:Double:] -> [:(Int,Double):] -> Double
dotp' v row = D.sumP [: x D.* (v !: i) | (i,x) <- row :]
-}

