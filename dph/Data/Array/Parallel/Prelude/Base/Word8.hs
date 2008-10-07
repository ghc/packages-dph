{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Word8 (
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
  mod, modV
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

import Prelude (Int, Bool)
import Data.Word (Word8)
import qualified Prelude as P
import qualified GHC.PArr

infixl 7 *
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

eqV, neqV, leV, ltV, geV, gtV :: Word8 :-> Word8 :-> Bool
{-# INLINE eqV #-}
{-# INLINE neqV #-}
{-# INLINE leV #-}
{-# INLINE ltV #-}
{-# INLINE geV #-}
{-# INLINE gtV #-}
eqV = closure2 dPA_Word8 (P.==) (unsafe_zipWith (P.==))
neqV = closure2 dPA_Word8 (P./=) (unsafe_zipWith (P./=))
leV = closure2 dPA_Word8 (P.<=) (unsafe_zipWith (P.<=))
ltV = closure2 dPA_Word8 (P.<) (unsafe_zipWith (P.<))
geV = closure2 dPA_Word8 (P.>=) (unsafe_zipWith (P.>=))
gtV = closure2 dPA_Word8 (P.>) (unsafe_zipWith (P.>))

(==), (/=), (<), (<=), (>), (>=) :: Word8 -> Word8 -> Bool
(==) = (P.==)
(/=) = (P./=)
(<=) = (P.<=)
(<)  = (P.<)
(>=) = (P.>=)
(>)  = (P.>)

minV, maxV :: Word8 :-> Word8 :-> Word8
{-# INLINE minV #-}
{-# INLINE maxV #-}
minV = closure2 dPA_Word8 P.min (unsafe_zipWith P.min)
maxV = closure2 dPA_Word8 P.max (unsafe_zipWith P.max)

min, max :: Word8 -> Word8 -> Word8
min = P.min
max = P.max

minimumPA, maximumPA :: PArray Word8 :-> Word8
{-# INLINE minimumPA #-}
{-# INLINE maximumPA #-}
minimumPA = closure1 (unsafe_fold1 P.min) (unsafe_fold1s P.min)
maximumPA = closure1 (unsafe_fold1 P.max) (unsafe_fold1s P.max)

minimumP, maximumP :: [:Word8:] -> Word8
minimumP = GHC.PArr.minimumP
maximumP = GHC.PArr.maximumP

minIndexPA :: PArray Word8 :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (unsafe_fold1Index min') (unsafe_fold1sIndex min')
  where
    min' (i,x) (j,y) | x P.<= y    = (i,x)
                     | P.otherwise = (j,y)

minIndexP :: [:Word8:] -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0

maxIndexPA :: PArray Word8 :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (unsafe_fold1Index max') (unsafe_fold1sIndex max')
  where
    max' (i,x) (j,y) | x P.>= y    = (i,x)
                     | P.otherwise = (j,y)

maxIndexP :: [:Word8:] -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0

plusV, minusV, multV :: Word8 :-> Word8 :-> Word8
{-# INLINE plusV #-}
{-# INLINE minusV #-}
{-# INLINE multV #-}
plusV = closure2 dPA_Word8 (P.+) (unsafe_zipWith (P.+))
minusV = closure2 dPA_Word8 (P.-) (unsafe_zipWith (P.-))
multV = closure2 dPA_Word8 (P.*) (unsafe_zipWith (P.*))

(+), (-), (*) :: Word8 -> Word8 -> Word8
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)

negateV, absV :: Word8 :-> Word8
{-# INLINE negateV #-}
{-# INLINE absV #-}
negateV = closure1 P.negate (unsafe_map P.negate)
absV = closure1 P.abs (unsafe_map P.abs)

negate, abs :: Word8 -> Word8
negate = P.negate
abs = P.abs

sumPA, productPA :: PArray Word8 :-> Word8
{-# INLINE sumPA #-}
{-# INLINE productPA #-}
sumPA = closure1 (unsafe_fold (+) 0) (unsafe_folds (+) 0)
productPA = closure1 (unsafe_fold (*) 1) (unsafe_folds (*) 1)

sumP, productP :: [:Word8:] -> Word8
sumP = GHC.PArr.sumP
productP = GHC.PArr.productP

divV, modV :: Word8 :-> Word8 :-> Word8
{-# INLINE divV #-}
{-# INLINE modV #-}
divV = closure2 dPA_Word8 P.div (unsafe_zipWith P.div)
modV = closure2 dPA_Word8 P.mod (unsafe_zipWith P.mod)

div, mod :: Word8 -> Word8 -> Word8
div = P.div
mod = P.mod

