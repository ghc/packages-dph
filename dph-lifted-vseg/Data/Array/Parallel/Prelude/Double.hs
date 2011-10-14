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

import Data.Array.Parallel.Lifted
import qualified Prelude as P
import Prelude (Double)
        
{-# VECTORISE SCALAR type Double #-}

nope    = P.error "Data.Array.Parallel.Prelude.Int: can't use unvectorised definition"
{-# NOVECTORISE nope #-}

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
sumP    = nope
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP = sumPP_double #-}


