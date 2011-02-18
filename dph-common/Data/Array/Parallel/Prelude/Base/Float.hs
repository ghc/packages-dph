{-# LANGUAGE ParallelArrays #-}
module Data.Array.Parallel.Prelude.Base.Float (
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

  -- Fractional
  (/), recip,
  divideV, recipV,

  -- Floating
  pi, exp, sqrt, log, (**), logBase,
  sin, tan, cos, asin, atan, acos,
  sinh, tanh, cosh, asinh, atanh, acosh,

  expV, sqrtV, logV, powV, logBaseV,
  sinV, tanV, cosV, asinV, atanV, acosV,
  sinhV, tanhV, coshV, asinhV, atanhV, acoshV,

  -- RealFrac and similar
  fromInt,
  fromIntV,

  truncate, round, ceiling, floor,
  truncateV, roundV, ceilingV, floorV
) where

import qualified Data.Array.Parallel as PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

import Prelude (Float, Int, Bool)
import qualified Prelude as P

infixr 8 **
infixl 7 *, /
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=

eqV, neqV, leV, ltV, geV, gtV :: Float :-> Float :-> Bool
{-# INLINE eqV #-}
{-# INLINE neqV #-}
{-# INLINE leV #-}
{-# INLINE ltV #-}
{-# INLINE geV #-}
{-# INLINE gtV #-}
eqV = closure2 (P.==) (scalar_zipWith (P.==))
neqV = closure2 (P./=) (scalar_zipWith (P./=))
leV = closure2 (P.<=) (scalar_zipWith (P.<=))
ltV = closure2 (P.<) (scalar_zipWith (P.<))
geV = closure2 (P.>=) (scalar_zipWith (P.>=))
gtV = closure2 (P.>) (scalar_zipWith (P.>))

(==), (/=), (<), (<=), (>), (>=) :: Float -> Float -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)

minV, maxV :: Float :-> Float :-> Float
{-# INLINE minV #-}
{-# INLINE maxV #-}
minV = closure2 P.min (scalar_zipWith P.min)
maxV = closure2 P.max (scalar_zipWith P.max)

min, max :: Float -> Float -> Float
min = P.min
max = P.max

minimumPA, maximumPA :: PArray Float :-> Float
{-# INLINE minimumPA #-}
{-# INLINE maximumPA #-}
minimumPA = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min)
maximumPA = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max)

minimumP, maximumP :: [:Float:] -> Float
minimumP = PArr.minimumP
maximumP = PArr.maximumP

minIndexPA :: PArray Float :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (scalar_fold1Index min') (scalar_fold1sIndex min')
  where
    min' (i,x) (j,y) | x P.<= y    = (i,x)
                     | P.otherwise = (j,y)

minIndexP :: [:Float:] -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0

maxIndexPA :: PArray Float :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (scalar_fold1Index max') (scalar_fold1sIndex max')
  where
    max' (i,x) (j,y) | x P.>= y    = (i,x)
                     | P.otherwise = (j,y)

maxIndexP :: [:Float:] -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0

plusV, minusV, multV :: Float :-> Float :-> Float
{-# INLINE plusV #-}
{-# INLINE minusV #-}
{-# INLINE multV #-}
plusV = closure2 (P.+) (scalar_zipWith (P.+))
minusV = closure2 (P.-) (scalar_zipWith (P.-))
multV = closure2 (P.*) (scalar_zipWith (P.*))

(+), (-), (*) :: Float -> Float -> Float
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)

negateV, absV :: Float :-> Float
{-# INLINE negateV #-}
{-# INLINE absV #-}
negateV = closure1 P.negate (scalar_map P.negate)
absV = closure1 P.abs (scalar_map P.abs)

negate, abs :: Float -> Float
negate = P.negate
abs = P.abs

sumPA, productPA :: PArray Float :-> Float
{-# INLINE sumPA #-}
{-# INLINE productPA #-}
sumPA = closure1 (scalar_fold (+) 0) (scalar_folds (+) 0)
productPA = closure1 (scalar_fold (*) 1) (scalar_folds (*) 1)

sumP, productP :: [:Float:] -> Float
sumP = PArr.sumP
productP = PArr.productP

(/) :: Float -> Float -> Float
(/) = (P./)

divideV :: Float :-> Float :-> Float
{-# INLINE divideV #-}
divideV = closure2 (P./) (scalar_zipWith (P./))

recip :: Float -> Float
recip = P.recip

recipV :: Float :-> Float
{-# INLINE recipV #-}
recipV = closure1 P.recip (scalar_map P.recip)


pi :: Float
pi = P.pi

exp, sqrt, log, sin, tan, cos, asin, atan, acos, sinh, tanh, cosh,
  asinh, atanh, acosh :: Float -> Float
exp = P.exp
sqrt = P.sqrt
log = P.log
sin = P.sin
tan = P.tan
cos = P.cos
asin = P.asin
atan = P.atan
acos = P.acos
sinh = P.sinh
tanh = P.tanh
cosh = P.cosh
asinh = P.asinh
atanh = P.atanh
acosh = P.acosh

expV, sqrtV, logV, sinV, tanV, cosV, asinV, atanV, acosV, sinhV, tanhV, coshV,
  asinhV, atanhV, acoshV :: Float :-> Float
{-# INLINE expV #-}
{-# INLINE sqrtV #-}
{-# INLINE logV #-}
{-# INLINE sinV #-}
{-# INLINE tanV #-}
{-# INLINE cosV #-}
{-# INLINE asinV #-}
{-# INLINE atanV #-}
{-# INLINE acosV #-}
{-# INLINE sinhV #-}
{-# INLINE tanhV #-}
{-# INLINE coshV #-}
{-# INLINE asinhV #-}
{-# INLINE atanhV #-}
{-# INLINE acoshV #-}
expV = closure1 P.exp (scalar_map P.exp)
sqrtV = closure1 P.sqrt (scalar_map P.sqrt)
logV = closure1 P.log (scalar_map P.log)
sinV = closure1 P.sin (scalar_map P.sin)
tanV = closure1 P.tan (scalar_map P.tan)
cosV = closure1 P.cos (scalar_map P.cos)
asinV = closure1 P.asin (scalar_map P.asin)
atanV = closure1 P.atan (scalar_map P.atan)
acosV = closure1 P.acos (scalar_map P.acos)
sinhV = closure1 P.sinh (scalar_map P.sinh)
tanhV = closure1 P.tanh (scalar_map P.tanh)
coshV = closure1 P.cosh (scalar_map P.cosh)
asinhV = closure1 P.asinh (scalar_map P.asinh)
atanhV = closure1 P.atanh (scalar_map P.atanh)
acoshV = closure1 P.acosh (scalar_map P.acosh)

(**), logBase :: Float -> Float -> Float
(**) = (P.**)
logBase = P.logBase

powV, logBaseV :: Float :-> Float :-> Float
{-# INLINE powV #-}
{-# INLINE logBaseV #-}
powV = closure2 (P.**) (scalar_zipWith (P.**))
logBaseV = closure2 P.logBase (scalar_zipWith P.logBase)


fromIntV :: Int :-> Float
{-# INLINE fromIntV #-}
fromIntV = closure1 P.fromIntegral (scalar_map P.fromIntegral)

fromInt :: Int -> Float
fromInt = P.fromIntegral

truncateV, roundV, ceilingV, floorV :: Float :-> Int
{-# INLINE truncateV #-}
{-# INLINE roundV #-}
{-# INLINE ceilingV #-}
{-# INLINE floorV #-}
truncateV = closure1 P.truncate (scalar_map P.truncate)
roundV = closure1 P.round (scalar_map P.round)
ceilingV = closure1 P.ceiling (scalar_map P.ceiling)
floorV = closure1 P.floor (scalar_map P.floor)

truncate, round, ceiling, floor :: Float -> Int
truncate = P.truncate
round = P.round
ceiling = P.ceiling
floor = P.floor

