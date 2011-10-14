{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Int (
        Int,
        (+), (-), (*),
        sumP
)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted
import qualified Prelude as P
import Prelude (Int)
        
{-# VECTORISE SCALAR type Int #-}

-- Basics ---------------------------------------------------------------------
(+), (-), (*) :: Int -> Int -> Int

(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}

(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}

(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}


-- Folds ----------------------------------------------------------------------
sumP    :: [:Int:] -> Int
sumP !arr       = indexPArr arr 0
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP = sumPP_int #-}


