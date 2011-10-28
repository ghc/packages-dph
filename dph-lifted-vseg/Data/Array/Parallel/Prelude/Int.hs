{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Int 
        ( Int
          
        -- * Ord
        , (==), (/=), (<), (<=), (>), (>=), min, max
        , maximumP,  minimumP
        , maxIndexP, minIndexP
     
        -- * Num
        , (+), (-), (*)
        , negate, abs
        , sumP, productP
        
        -- * Integral
        , div, mod
        
        -- * Enum
        , enumFromToP)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Lifted                       ((:->)(..))
import qualified Data.Array.Parallel.Lifted             as L
import qualified Data.Array.Parallel.PArray.Scalar      as SC
import qualified Prelude as P
import Prelude (Int)
        
{-# VECTORISE SCALAR type Int #-}

infixl 7 *
infixl 6 +, -
infix  4 ==, /=, <, <=, >, >=
-- infixl 7 `div`, `mod`

-- Ord ------------------------------------------------------------------------
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


-- min/max ----------------------------
min, max :: Int -> Int -> Int

min = P.min
{-# VECTORISE SCALAR min #-}

max = P.max
{-# VECTORISE SCALAR max #-}


-- minimum/maximum --------------------
minimumP, maximumP :: [:Int:] -> Int

minimumP arr    = headPArr arr
{-# NOINLINE  minimumP #-}
{-# VECTORISE minimumP = minimumPP #-}

maximumP arr    = headPArr arr
{-# NOINLINE  maximumP #-}
{-# VECTORISE maximumP = maximumPP #-}

minimumPP, maximumPP :: PArray Int :-> Int
minimumPP      = L.closure1' (SC.fold1 P.min) (SC.fold1s P.min)
{-# INLINE      minimumPP #-}
{-# NOVECTORISE minimumPP #-}

maximumPP      = L.closure1' (SC.fold1 P.max) (SC.fold1s P.max)
{-# INLINE      maximumPP #-}
{-# NOVECTORISE maximumPP #-}


-- minIndex/maxIndex ------------------
minIndexP :: [:Int:] -> Int
minIndexP !_    = 0 
{-# NOINLINE  minIndexP #-}
{-# VECTORISE minIndexP = minIndexPP #-}

minIndexPP :: PArray Int :-> Int
minIndexPP      = L.closure1' (SC.fold1Index min') (SC.fold1sIndex min')
{-# INLINE      minIndexPP #-}
{-# NOVECTORISE minIndexPP #-}

min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}


maxIndexP :: [:Int:] -> Int
maxIndexP _     = 0
{-# NOINLINE  maxIndexP #-}
{-# VECTORISE maxIndexP = maxIndexPP #-}

maxIndexPP :: PArray Int :-> Int
maxIndexPP      = L.closure1' (SC.fold1Index max') (SC.fold1sIndex max')
{-# INLINE      maxIndexPP #-}
{-# NOVECTORISE maxIndexPP #-}

max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}


-- Num ------------------------------------------------------------------------
(+), (-), (*) :: Int -> Int -> Int

(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}

(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}

(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}


-- negate/abs -------------------------
negate, abs :: Int -> Int

negate  = P.negate
{-# VECTORISE SCALAR negate #-}

abs     = P.abs
{-# VECTORISE SCALAR abs #-}


-- sum/product ------------------------
sumP, productP :: [:Int:] -> Int

sumP arr        = headPArr arr
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP      = sumPP #-}

productP arr    = headPArr arr
{-# NOINLINE  productP #-}
{-# VECTORISE productP  = productPP #-}

sumPP, productPP :: PArray Int :-> Int
sumPP          = L.closure1' (SC.fold (+) 0) (SC.folds (+) 0)
{-# INLINE      sumPP #-}
{-# NOVECTORISE sumPP #-}

productPP      = L.closure1' (SC.fold (*) 1) (SC.folds (*) 1)
{-# INLINE      productPP #-}
{-# NOVECTORISE productPP #-}


-- Integral -------------------------------------------------------------------
div, mod :: Int -> Int -> Int

div = P.div
{-# VECTORISE SCALAR div #-}

mod = P.mod
{-# VECTORISE SCALAR mod #-}


-- Enum -----------------------------------------------------------------------
enumFromToP :: Int -> Int -> [:Int:]
enumFromToP !_ !_       = emptyPArr
{-# NOINLINE  enumFromToP #-}
{-# VECTORISE enumFromToP = enumFromToPP #-}

enumFromToPP :: Int :-> Int :-> PArray Int
enumFromToPP    = L.closure2' SC.enumFromTo SC.enumFromTol
{-# INLINE      enumFromToPP #-}
{-# NOVECTORISE enumFromToPP #-}
