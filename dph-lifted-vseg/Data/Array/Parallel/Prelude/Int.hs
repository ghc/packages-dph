{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Int (
        Int,
     
        -- Ord
        (==), (/=), (<), (<=), (>), (>=), min, max,
     
        -- Num
        (+), (-), (*),
        sumP,
        
        -- Integral
        div, mod
)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted
import Data.Array.Parallel.Prelude.Bool
import qualified Prelude as P
import Prelude (Int)
        
{-# VECTORISE SCALAR type Int #-}

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


min, max :: Int -> Int -> Int
min = P.min
{-# VECTORISE SCALAR min #-}

max = P.max
{-# VECTORISE SCALAR max #-}


-- Num ------------------------------------------------------------------------
(+), (-), (*) :: Int -> Int -> Int

(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}

(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}

(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}


sumP    :: [:Int:] -> Int
sumP !arr       = indexPArr arr 0
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP = sumPP_int #-}


-- Integral -------------------------------------------------------------------
div, mod :: Int -> Int -> Int

div = P.div
{-# VECTORISE SCALAR div #-}

mod = P.mod
{-# VECTORISE SCALAR mod #-}

