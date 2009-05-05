{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Double (
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

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

import Prelude (Double, Int, Bool)
import qualified Prelude as P
import qualified GHC.PArr

infixr 8 **
infixl 7 *, /
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=

eqV, neqV, leV, ltV, geV, gtV :: Double :-> Double :-> Bool
{-# INLINE eqV #-}
{-# INLINE neqV #-}
{-# INLINE leV #-}
{-# INLINE ltV #-}
{-# INLINE geV #-}
{-# INLINE gtV #-}
eqV = closure2 dPA_Double (P.==) (scalar_zipWith (P.==))
neqV = closure2 dPA_Double (P./=) (scalar_zipWith (P./=))
leV = closure2 dPA_Double (P.<=) (scalar_zipWith (P.<=))
ltV = closure2 dPA_Double (P.<) (scalar_zipWith (P.<))
geV = closure2 dPA_Double (P.>=) (scalar_zipWith (P.>=))
gtV = closure2 dPA_Double (P.>) (scalar_zipWith (P.>))

(==), (/=), (<), (<=), (>), (>=) :: Double -> Double -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)

minV, maxV :: Double :-> Double :-> Double
{-# INLINE minV #-}
{-# INLINE maxV #-}
minV = closure2 dPA_Double P.min (scalar_zipWith P.min)
maxV = closure2 dPA_Double P.max (scalar_zipWith P.max)

min, max :: Double -> Double -> Double
min = P.min
max = P.max

minimumPA, maximumPA :: PArray Double :-> Double
{-# INLINE minimumPA #-}
{-# INLINE maximumPA #-}
minimumPA = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min)
maximumPA = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max)

minimumP, maximumP :: [:Double:] -> Double
minimumP = GHC.PArr.minimumP
maximumP = GHC.PArr.maximumP

minIndexPA :: PArray Double :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (scalar_fold1Index min') (scalar_fold1sIndex min')
  where
    min' (i,x) (j,y) | x P.<= y    = (i,x)
                     | P.otherwise = (j,y)

minIndexP :: [:Double:] -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0

maxIndexPA :: PArray Double :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (scalar_fold1Index max') (scalar_fold1sIndex max')
  where
    max' (i,x) (j,y) | x P.>= y    = (i,x)
                     | P.otherwise = (j,y)

maxIndexP :: [:Double:] -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0

plusV, minusV, multV :: Double :-> Double :-> Double
{-# INLINE plusV #-}
{-# INLINE minusV #-}
{-# INLINE multV #-}
plusV = closure2 dPA_Double (P.+) (scalar_zipWith (P.+))
minusV = closure2 dPA_Double (P.-) (scalar_zipWith (P.-))
multV = closure2 dPA_Double (P.*) (scalar_zipWith (P.*))

(+), (-), (*) :: Double -> Double -> Double
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)

negateV, absV :: Double :-> Double
{-# INLINE negateV #-}
{-# INLINE absV #-}
negateV = closure1 P.negate (scalar_map P.negate)
absV = closure1 P.abs (scalar_map P.abs)

negate, abs :: Double -> Double
negate = P.negate
abs = P.abs

sumPA, productPA :: PArray Double :-> Double
{-# INLINE sumPA #-}
{-# INLINE productPA #-}
sumPA = closure1 (scalar_fold (+) 0) (scalar_folds (+) 0)
productPA = closure1 (scalar_fold (*) 1) (scalar_folds (*) 1)

sumP, productP :: [:Double:] -> Double
sumP = GHC.PArr.sumP
productP = GHC.PArr.productP

(/) :: Double -> Double -> Double
(/) = (P./)

divideV :: Double :-> Double :-> Double
{-# INLINE divideV #-}
divideV = closure2 dPA_Double (P./) (scalar_zipWith (P./))

recip :: Double -> Double
recip = P.recip

recipV :: Double :-> Double
{-# INLINE recipV #-}
recipV = closure1 P.recip (scalar_map P.recip)


pi :: Double
pi = P.pi

exp, sqrt, log, sin, tan, cos, asin, atan, acos, sinh, tanh, cosh,
  asinh, atanh, acosh :: Double -> Double
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
  asinhV, atanhV, acoshV :: Double :-> Double
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

(**), logBase :: Double -> Double -> Double
(**) = (P.**)
logBase = P.logBase

powV, logBaseV :: Double :-> Double :-> Double
{-# INLINE powV #-}
{-# INLINE logBaseV #-}
powV = closure2 dPA_Double (P.**) (scalar_zipWith (P.**))
logBaseV = closure2 dPA_Double P.logBase (scalar_zipWith P.logBase)


fromIntV :: Int :-> Double
{-# INLINE fromIntV #-}
fromIntV = closure1 P.fromIntegral (scalar_map P.fromIntegral)

fromInt :: Int -> Double
fromInt = P.fromIntegral

truncateV, roundV, ceilingV, floorV :: Double :-> Int
{-# INLINE truncateV #-}
{-# INLINE roundV #-}
{-# INLINE ceilingV #-}
{-# INLINE floorV #-}
truncateV = closure1 P.truncate (scalar_map P.truncate)
roundV = closure1 P.round (scalar_map P.round)
ceilingV = closure1 P.ceiling (scalar_map P.ceiling)
floorV = closure1 P.floor (scalar_map P.floor)

truncate, round, ceiling, floor :: Double -> Int
truncate = P.truncate
round = P.round
ceiling = P.ceiling
floor = P.floor

