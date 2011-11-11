{-# OPTIONS_GHC -fvectorise #-}
  -- NB: Cannot use any parallel array syntax except the type constructor

module Data.Array.Parallel.Prelude.Int (
  Int,
  
  -- Ord
  (==), (/=), (<), (<=), (>), (>=), min, max,

  minimumP, maximumP, minIndexP, maxIndexP,

  -- Num
  (+), (-), (*), negate, abs,

  sumP, productP,

  -- Integral
  div, mod, sqrt,
  
  -- Enum
  enumFromToP
) where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Prelude.Bool

import Prelude (Int)
import qualified Prelude as P

infixl 7 *
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

{-# VECTORISE SCALAR type Int #-}

(==), (/=), (<), (<=), (>), (>=) :: Int -> Int -> Bool
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

min, max :: Int -> Int -> Int
min = P.min
{-# VECTORISE SCALAR min #-}
max = P.max
{-# VECTORISE SCALAR max #-}

minimumP, maximumP :: PArr Int -> Int
{-# NOINLINE minimumP #-}
minimumP a = a `indexPArr` 0
{-# VECTORISE minimumP = minimumP_v #-}
{-# NOINLINE maximumP #-}
maximumP a = a `indexPArr` 0
{-# VECTORISE maximumP = maximumP_v #-}

minimumP_v, maximumP_v:: PArray Int :-> Int
{-# INLINE minimumP_v #-}
minimumP_v = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min)
{-# NOVECTORISE minimumP_v #-}
{-# INLINE maximumP_v #-}
maximumP_v = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max)
{-# NOVECTORISE maximumP_v #-}

minIndexP :: PArr Int -> Int
{-# NOINLINE minIndexP #-}
minIndexP _ = 0   -- FIXME: add proper implementation
{-# VECTORISE minIndexP = minIndexPA #-}

minIndexPA :: PArray Int :-> Int
{-# INLINE minIndexPA #-}
minIndexPA = closure1 (scalar_fold1Index min') (scalar_fold1sIndex min')
{-# NOVECTORISE minIndexPA #-}

min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}

maxIndexP :: PArr Int -> Int
{-# NOINLINE maxIndexP #-}
maxIndexP _ = 0   -- FIXME: add proper implementation
{-# VECTORISE maxIndexP = maxIndexPA #-}

maxIndexPA :: PArray Int :-> Int
{-# INLINE maxIndexPA #-}
maxIndexPA = closure1 (scalar_fold1Index max') (scalar_fold1sIndex max')
{-# NOVECTORISE maxIndexPA #-}

max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}

(+), (-), (*) :: Int -> Int -> Int
(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}
(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}
(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}

negate, abs :: Int -> Int
negate = P.negate
{-# VECTORISE SCALAR negate #-}
abs = P.abs
{-# VECTORISE SCALAR abs #-}

sumP, productP :: PArr Int -> Int
{-# NOINLINE sumP #-}
sumP a = a `indexPArr` 0
{-# VECTORISE sumP = sumP_v #-}
{-# NOINLINE productP #-}
productP a = a `indexPArr` 0
{-# VECTORISE productP = productP_v #-}

sumP_v, productP_v:: PArray Int :-> Int
{-# INLINE sumP_v #-}
sumP_v     = closure1 (scalar_fold (P.+) 0) (scalar_folds (P.+) 0)
{-# NOVECTORISE sumP_v #-}
{-# INLINE productP_v #-}
productP_v = closure1 (scalar_fold (P.*) 1) (scalar_folds (P.*) 1)
{-# NOVECTORISE productP_v #-}

div, mod :: Int -> Int -> Int
div = P.div
{-# VECTORISE SCALAR div #-}
mod = P.mod
{-# VECTORISE SCALAR mod #-}

sqrt ::  Int -> Int
sqrt n = P.floor (P.sqrt (P.fromIntegral n) :: P.Double)
{-# VECTORISE SCALAR sqrt #-}

enumFromToP :: Int -> Int ->  PArr Int
{-# NOINLINE enumFromToP #-}
enumFromToP x y = singletonPArr (x P.+ y)
{-# VECTORISE enumFromToP = enumFromToPA_Int #-}
