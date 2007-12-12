{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Int (
  -- eq, eqV, neq, neqV,
  plus, plusV,
  minus, minusV,
  mult, multV,
  sumP, sumPA,
  upToP, upToPA
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

{-
eqV :: Int :-> Int :-> Bool
{-# INLINE eqV #-}
eqV = closure2 dPA_Int (==) (unsafe_zipWith_Int (==))

eq :: Int -> Int -> Bool
eq = (==)

neqV :: Int :-> Int :-> Bool
{-# INLINE neqV #-}
neqV = closure2 dPA_Int (/=) (unsafe_zipWith_Int (/=))

neq :: Int -> Int -> Bool
neq = (==)
-}

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

