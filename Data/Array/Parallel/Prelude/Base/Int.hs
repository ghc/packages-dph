{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Int (
  plus, plusV,
  minus, minusV,
  sumP, sumPA,
  upToP, upToPA
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Temporary
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

plusV :: Int :-> Int :-> Int
{-# INLINE plusV #-}
plusV = closure2 dPA_Int (+) (unsafe_zipWith_Int (+))

plus :: Int -> Int -> Int
plus = (+)

minusV :: Int :-> Int :-> Int
{-# INLINE minusV #-}
minusV = closure2 dPA_Int (-) (unsafe_zipWith_Int (-))

minus :: Int -> Int -> Int
minus = (-)

sumPA :: PArray Int :-> Int
{-# INLINE sumPA #-}
sumPA = closure1 (unsafe_fold_Int (+) 0) (\_ -> error "Int.sumV lifted")

sumP :: [:Int:] -> Int
{-# NOINLINE sumP #-}
sumP _ = error "PArr.sumP"

upToPA :: Int :-> PArray Int
{-# INLINE upToPA #-}
upToPA = closure1 upto_Int (\_ -> error "Int.upToPA lifted")

upToP :: Int -> [:Int:]
{-# NOINLINE upToP #-}
upToP _ = error "PArr.upToP"

