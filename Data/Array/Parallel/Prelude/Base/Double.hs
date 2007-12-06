{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Double (
  -- eq, eqV, neq, neqV,
  plus, plusV,
  minus, minusV,
  mult, multV,
  sumP, sumPA
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

{-
eqV :: Double :-> Double :-> Bool
{-# INLINE eqV #-}
eqV = closure2 dPA_Double (==) (unsafe_zipWith_Double (==))

eq :: Double -> Double -> Bool
eq = (==)

neqV :: Double :-> Double :-> Bool
{-# INLINE neqV #-}
neqV = closure2 dPA_Double (/=) (unsafe_zipWith_Double (/=))

neq :: Double -> Double -> Bool
neq = (==)
-}

plusV :: Double :-> Double :-> Double
{-# INLINE plusV #-}
plusV = closure2 dPA_Double (+) (unsafe_zipWithPA_Double (+))

plus :: Double -> Double -> Double
plus = (+)

minusV :: Double :-> Double :-> Double
{-# INLINE minusV #-}
minusV = closure2 dPA_Double (-) (unsafe_zipWithPA_Double (-))

minus :: Double -> Double -> Double
minus = (-)

multV :: Double :-> Double :-> Double
{-# INLINE multV #-}
multV = closure2 dPA_Double (*) (unsafe_zipWithPA_Double (*))

mult :: Double -> Double -> Double
mult = (*)

sumPA :: PArray Double :-> Double
{-# INLINE sumPA #-}
sumPA = closure1 (unsafe_foldPA_Double (+) 0) sumPAs_Double

sumP :: [:Double:] -> Double
{-# NOINLINE sumP #-}
sumP _ = error "Double.sumP"

