{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Int (
  eq, eqV, neq, neqV, le, leV, lt, ltV, ge, geV, gt, gtV,
  plus, plusV,
  minus, minusV,
  mult, multV,
  intDiv, intDivV, 
  sumP, sumPA,
  upToP, upToPA
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

eqV, neqV, leV, ltV, geV, gtV :: Int :-> Int :-> Bool
{-# INLINE eqV #-}
{-# INLINE neqV #-}
{-# INLINE leV #-}
{-# INLINE ltV #-}
{-# INLINE geV #-}
{-# INLINE gtV #-}
eqV = closure2 dPA_Int (==) (unsafe_zipWith (==))
neqV = closure2 dPA_Int (/=) (unsafe_zipWith (/=))
leV = closure2 dPA_Int (<=) (unsafe_zipWith (<=))
ltV = closure2 dPA_Int (<) (unsafe_zipWith (<))
geV = closure2 dPA_Int (>=) (unsafe_zipWith (>=))
gtV = closure2 dPA_Int (>) (unsafe_zipWith (>))

eq, neq, le, lt, ge, gt :: Int -> Int -> Bool
eq = (==)
neq = (/=)
le = (<=)
lt = (<)
ge = (>=)
gt = (>)

plusV :: Int :-> Int :-> Int
{-# INLINE plusV #-}
plusV = closure2 dPA_Int (+) (unsafe_zipWith (+))

plus :: Int -> Int -> Int
plus = (+)

minusV :: Int :-> Int :-> Int
{-# INLINE minusV #-}
minusV = closure2 dPA_Int (-) (unsafe_zipWith (-))

minus :: Int -> Int -> Int
minus = (-)

multV :: Int :-> Int :-> Int
{-# INLINE multV #-}
multV = closure2 dPA_Int (*) (unsafe_zipWith (*))

mult :: Int -> Int -> Int
mult = (*)

intDivV :: Int :-> Int :-> Int
{-# INLINE intDivV #-}
intDivV = closure2 dPA_Int div (unsafe_zipWith div)

intDiv :: Int -> Int -> Int
intDiv = div


sumPA :: PArray Int :-> Int
{-# INLINE sumPA #-}
sumPA = closure1 (unsafe_fold (+) 0) (\_ -> error "Int.sumV lifted")

sumP :: [:Int:] -> Int
{-# NOINLINE sumP #-}
sumP _ = error "PArr.sumP"

upToPA :: Int :-> PArray Int
{-# INLINE upToPA #-}
upToPA = closure1 upToPA_Int (\_ -> error "Int.upToPA lifted")

upToP :: Int -> [:Int:]
{-# NOINLINE upToP #-}
upToP _ = error "PArr.upToP"

