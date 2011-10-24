{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
  -- NB: Cannot use any parallel array syntax except the type constructor

module Data.Array.Parallel.Prelude.Word8 (
  Word8,
  
  -- Ord
  (==), (/=), (<), (<=), (>), (>=), min, max,

  minimumP, maximumP, minIndexP, maxIndexP,

  -- Num
  (+), (-), (*), negate, abs,

  sumP, productP,

  -- Integral
  div, mod, sqrt,
  
  toInt, fromInt
) where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Prelude.Int    (Int)  -- get the vectorised version
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Prelude.Bool

import Data.Word (Word8)
import qualified Prelude as P

infixl 7 *
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

{-# VECTORISE SCALAR type Word8 #-}

(==), (/=), (<), (<=), (>), (>=) :: Word8 -> Word8 -> Bool
(==) = (P.==)
{-# VECTORISE SCALAR (==) #-}
(/=) = (P./=)
{-# VECTORISE SCALAR (/=) #-}
(<=) = (P.<=)
{-# VECTORISE SCALAR (<=) #-}
(<)  = (P.<)
{-# VECTORISE SCALAR (<) #-}
(>=) = (P.>=)
{-# VECTORISE SCALAR (>=) #-}
(>)  = (P.>)
{-# VECTORISE SCALAR (>) #-}

min, max :: Word8 -> Word8 -> Word8
min = P.min
{-# VECTORISE SCALAR min #-}
max = P.max
{-# VECTORISE SCALAR max #-}

minimumP, maximumP :: [:Word8:] -> Word8
{-# NOINLINE minimumP #-}
minimumP a = a `indexPArr` 0
{-# VECTORISE minimumP = minimumP_v #-}
{-# NOINLINE maximumP #-}
maximumP a = a `indexPArr` 0
{-# VECTORISE maximumP = maximumP_v #-}

minimumP_v, maximumP_v:: PArray Word8 :-> Word8
{-# INLINE minimumP_v #-}
minimumP_v = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min)
{-# NOVECTORISE minimumP_v #-}
{-# INLINE maximumP_v #-}
maximumP_v = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max)
{-# NOVECTORISE maximumP_v #-}

minIndexP :: [:Word8:] -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0   -- FIXME: add proper implementation
{-# VECTORISE minIndexP = minIndexPA #-}

minIndexPA :: PArray Word8 :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (scalar_fold1Index min') (scalar_fold1sIndex min')
{-# NOVECTORISE minIndexPA #-}

min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}

maxIndexP :: [:Word8:] -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0   -- FIXME: add proper implementation
{-# VECTORISE maxIndexP = maxIndexPA #-}

maxIndexPA :: PArray Word8 :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (scalar_fold1Index max') (scalar_fold1sIndex max')
{-# NOVECTORISE maxIndexPA #-}

max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}

(+), (-), (*) :: Word8 -> Word8 -> Word8
(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}
(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}
(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}

negate, abs :: Word8 -> Word8
negate = P.negate
{-# VECTORISE SCALAR negate #-}
abs = P.abs
{-# VECTORISE SCALAR abs #-}

sumP, productP :: [:Word8:] -> Word8
{-# NOINLINE sumP #-}
sumP a = a `indexPArr` 0
{-# VECTORISE sumP = sumP_v #-}
{-# NOINLINE productP #-}
productP a = a `indexPArr` 0
{-# VECTORISE productP = productP_v #-}

sumP_v, productP_v:: PArray Word8 :-> Word8
{-# INLINE sumP_v #-}
sumP_v     = closure1 (scalar_fold (+) 0) (scalar_folds (+) 0)
{-# NOVECTORISE sumP_v #-}
{-# INLINE productP_v #-}
productP_v = closure1 (scalar_fold (*) 1) (scalar_folds (*) 1)
{-# NOVECTORISE productP_v #-}

div, mod :: Word8 -> Word8 -> Word8
div = P.div
{-# VECTORISE SCALAR div #-}
mod = P.mod
{-# VECTORISE SCALAR mod #-}

sqrt ::  Word8 -> Word8
sqrt n = P.floor (P.sqrt (P.fromIntegral n) :: P.Double)
{-# VECTORISE SCALAR sqrt #-}

toInt :: Word8 -> Int
toInt = P.fromIntegral
{-# VECTORISE SCALAR toInt #-}

fromInt :: Int -> Word8
fromInt = P.fromIntegral
{-# VECTORISE SCALAR fromInt #-}
