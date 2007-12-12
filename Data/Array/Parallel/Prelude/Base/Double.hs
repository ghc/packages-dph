{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Double (
  eq, eqV, neq, neqV, le, leV, lt, ltV, ge, geV, gt, gtV,
  plus, plusV,
  minus, minusV,
  mult, multV,
  sumP, sumPA
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

eqV, neqV, leV, ltV, geV, gtV :: Double :-> Double :-> Bool
{-# INLINE eqV #-}
{-# INLINE neqV #-}
{-# INLINE leV #-}
{-# INLINE ltV #-}
{-# INLINE geV #-}
{-# INLINE gtV #-}
eqV = closure2 dPA_Double (==) (unsafe_zipWith (==))
neqV = closure2 dPA_Double (/=) (unsafe_zipWith (/=))
leV = closure2 dPA_Double (<=) (unsafe_zipWith (<=))
ltV = closure2 dPA_Double (<) (unsafe_zipWith (<))
geV = closure2 dPA_Double (>=) (unsafe_zipWith (>=))
gtV = closure2 dPA_Double (>) (unsafe_zipWith (>))

eq, neq, le, lt, ge, gt :: Double -> Double -> Bool
eq = (==)
neq = (/=)
le = (<=)
lt = (<)
ge = (>=)
gt = (>)

plusV :: Double :-> Double :-> Double
{-# INLINE plusV #-}
plusV = closure2 dPA_Double (+) (unsafe_zipWith (+))

plus :: Double -> Double -> Double
plus = (+)

minusV :: Double :-> Double :-> Double
{-# INLINE minusV #-}
minusV = closure2 dPA_Double (-) (unsafe_zipWith (-))

minus :: Double -> Double -> Double
minus = (-)

multV :: Double :-> Double :-> Double
{-# INLINE multV #-}
multV = closure2 dPA_Double (*) (unsafe_zipWith (*))

mult :: Double -> Double -> Double
mult = (*)

sumPA :: PArray Double :-> Double
{-# INLINE sumPA #-}
sumPA = closure1 (unsafe_fold (+) 0)
                 (unsafe_folds (+) 0)

sumP :: [:Double:] -> Double
{-# NOINLINE sumP #-}
sumP _ = error "Double.sumP"

minimumPA :: PArray Double :-> Double
{-# INLINE minimumPA #-}
minimumPA = closure1 (unsafe_fold1 min) (unsafe_fold1s min)

