{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Double (
        Double,
        (+), (-), (*),
        sumP
)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted
import qualified Prelude as P
import Prelude (Double)
        
{-# VECTORISE SCALAR type Double #-}

-- Basics ---------------------------------------------------------------------
(+), (-), (*) :: Double -> Double -> Double

(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}

(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}

(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}


-- Folds ----------------------------------------------------------------------
sumP    :: [:Double:] -> Double
sumP !arr       = indexPArr arr 0
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP = sumPP_double #-}


