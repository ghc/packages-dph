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

import Data.Array.Parallel.Lifted
import qualified Prelude as P
import Prelude (Int)
        
{-# VECTORISE SCALAR type Int #-}

nope    = P.error "Data.Array.Parallel.Prelude.Int: can't use unvectorised definition"
{-# NOVECTORISE nope #-}

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
sumP    = nope
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP = sumPP_int #-}


