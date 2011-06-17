{-# LANGUAGE ParallelArrays #-}
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

import Data.Array.Parallel.VectDepend ()  -- see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure

import Prelude (Int, Bool)
import qualified Prelude as P

infixl 7 *
infixl 6 +, -
infix 4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

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

minimumP, maximumP :: [:Int:] -> Int
{-# NOINLINE minimumP #-}
minimumP a = a `indexPArr` 0
{-# VECTORISE minimumP
  = closure1 (scalar_fold1 P.min) (scalar_fold1s P.min) :: PArray Int :-> Int #-}
{-# NOINLINE maximumP #-}
maximumP a = a `indexPArr` 0
{-# VECTORISE maximumP
  = closure1 (scalar_fold1 P.max) (scalar_fold1s P.max) :: PArray Int :-> Int #-}

minIndexP :: [:Int:] -> Int
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

maxIndexP :: [:Int:] -> Int
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

sumP, productP :: [:Int:] -> Int
{-# NOINLINE sumP #-}
sumP a = a `indexPArr` 0
{-# VECTORISE sumP 
  = closure1 (scalar_fold (+) 0) (scalar_folds (+) 0) :: PArray Int :-> Int #-}
{-# NOINLINE productP #-}
productP a = a `indexPArr` 0
{-# VECTORISE productP 
  = closure1 (scalar_fold (*) 1) (scalar_folds (*) 1) :: PArray Int :-> Int #-}

div, mod :: Int -> Int -> Int
div = P.div
{-# VECTORISE SCALAR div #-}
mod = P.mod
{-# VECTORISE SCALAR mod #-}

sqrt ::  Int -> Int
sqrt n = P.floor (P.sqrt (P.fromIntegral n) :: P.Double)
{-# VECTORISE SCALAR sqrt #-}

enumFromToP :: Int -> Int ->  [:Int:]
{-# NOINLINE enumFromToP #-}
enumFromToP x y = singletonPArr (x + y)
{-# VECTORISE enumFromToP = enumFromToPA_Int #-}
