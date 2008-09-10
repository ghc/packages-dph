module Data.Array.Parallel.Prelude.Base.Random (
  Random(..)
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.PArray ( PArray, PrimPA, fromUArrPA' )
import Data.Array.Parallel.Lifted.Prim   ()

import qualified System.Random as R

class Random a where
  randoms  :: R.RandomGen g => Int -> g -> PArray a
  randomRs :: R.RandomGen g => Int -> (a, a) -> g -> PArray a

prim_randoms :: (PrimPA a, R.Random a, R.RandomGen g) => Int -> g -> PArray a
prim_randoms n = fromUArrPA' . U.randoms n

prim_randomRs :: (PrimPA a, R.Random a, R.RandomGen g) => Int -> (a, a) -> g -> PArray a
prim_randomRs n r = fromUArrPA' . U.randomRs n r

instance Random Int where
  randoms = prim_randoms
  randomRs = prim_randomRs

instance Random Double where
  randoms = prim_randoms
  randomRs = prim_randomRs

