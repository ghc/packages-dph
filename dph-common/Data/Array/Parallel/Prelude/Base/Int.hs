{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Int (
  -- Ord
  (==), (/=), (<), (<=), (>), (>=), min, max,
  eqV, neqV, ltV, leV, gtV, geV, minV, maxV,

  minimumP, maximumP, minIndexP, maxIndexP,
  minimumPA, maximumPA, minIndexPA, maxIndexPA,

  -- Num
  (+), (-), (*), negate, abs,
  plusV, minusV, multV, negateV, absV,

  sumP, productP,
  sumPA, productPA,

  div, divV, 
  mod, modV, 
  sqrt, sqrtV,

  enumFromToPA, enumFromToP
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

import Prelude (Int, Bool)
import qualified Prelude as P
import qualified GHC.PArr

infixl 7 *
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

eqV, neqV, leV, ltV, geV, gtV :: Int :-> Int :-> Bool
{-# INLINE eqV #-}
{-# INLINE neqV #-}
{-# INLINE leV #-}
{-# INLINE ltV #-}
{-# INLINE geV #-}
{-# INLINE gtV #-}
eqV = closure2 dPA_Int (P.==) (scalar_zipWith (P.==))
neqV = closure2 dPA_Int (P./=) (scalar_zipWith (P./=))
leV = closure2 dPA_Int (P.<=) (scalar_zipWith (P.<=))
ltV = closure2 dPA_Int (P.<) (scalar_zipWith (P.<))
geV = closure2 dPA_Int (P.>=) (scalar_zipWith (P.>=))
gtV = closure2 dPA_Int (P.>) (scalar_zipWith (P.>))

(==), (/=), (<), (<=), (>), (>=) :: Int -> Int -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)

minV, maxV :: Int :-> Int :-> Int
{-# INLINE minV #-}
{-# INLINE maxV #-}
minV = closure2 dPA_Int P.min (scalar_zipWith P.min)
maxV = closure2 dPA_Int P.max (scalar_zipWith P.max)

min, max :: Int -> Int -> Int
min = P.min
max = P.max

minimumPA, maximumPA :: PArray Int :-> Int
{-# INLINE minimumPA #-}
{-# INLINE maximumPA #-}
minimumPA = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min)
maximumPA = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max)

minimumP, maximumP :: [:Int:] -> Int
minimumP = GHC.PArr.minimumP
maximumP = GHC.PArr.maximumP

minIndexPA :: PArray Int :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (scalar_fold1Index min') (scalar_fold1sIndex min')
  where
    min' (i,x) (j,y) | x P.<= y    = (i,x)
                     | P.otherwise = (j,y)

minIndexP :: [:Int:] -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0

maxIndexPA :: PArray Int :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (scalar_fold1Index max') (scalar_fold1sIndex max')
  where
    max' (i,x) (j,y) | x P.>= y    = (i,x)
                     | P.otherwise = (j,y)

maxIndexP :: [:Int:] -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0


plusV, minusV, multV :: Int :-> Int :-> Int
{-# INLINE plusV #-}
{-# INLINE minusV #-}
{-# INLINE multV #-}
plusV = closure2 dPA_Int (P.+) (scalar_zipWith (P.+))
minusV = closure2 dPA_Int (P.-) (scalar_zipWith (P.-))
multV = closure2 dPA_Int (P.*) (scalar_zipWith (P.*))

(+), (-), (*) :: Int -> Int -> Int
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)

negateV, absV :: Int :-> Int
{-# INLINE negateV #-}
{-# INLINE absV #-}
negateV = closure1 P.negate (scalar_map P.negate)
absV = closure1 P.abs (scalar_map P.abs)

negate, abs :: Int -> Int
negate = P.negate
abs = P.abs

sumPA, productPA :: PArray Int :-> Int
{-# INLINE sumPA #-}
{-# INLINE productPA #-}
sumPA = closure1 (scalar_fold (+) 0) (scalar_folds (+) 0)
productPA = closure1 (scalar_fold (*) 1) (scalar_folds (*) 1)

sumP, productP :: [:Int:] -> Int
sumP = GHC.PArr.sumP
productP = GHC.PArr.productP


divV, modV :: Int :-> Int :-> Int
{-# INLINE divV #-}
{-# INLINE modV #-}
divV = closure2 dPA_Int P.div (scalar_zipWith P.div)
modV = closure2 dPA_Int P.mod (scalar_zipWith P.mod)

div, mod :: Int -> Int -> Int
div = P.div
mod = P.mod

sqrt ::  Int -> Int
sqrt n = P.floor (P.sqrt (P.fromIntegral n))

sqrtV :: Int :-> Int
{-# INLINE sqrtV #-}
sqrtV = closure1  (sqrt) (scalar_map sqrt)

enumFromToPA :: Int :-> Int :->  PArray Int 
{-# INLINE enumFromToPA #-}
enumFromToPA = enumFromToPA_Int

enumFromToP :: Int -> Int ->  [:Int:]
{-# NOINLINE enumFromToP #-}
enumFromToP n m = [:n..m:]

