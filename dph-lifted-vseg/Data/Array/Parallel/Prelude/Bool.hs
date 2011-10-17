{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
-- NB: Cannot use any parallel array syntax except the type constructor

module Data.Array.Parallel.Prelude.Bool (
        Bool(..),
        otherwise
)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.Lifted
        
{-# VECTORISE type Bool = Bool #-}

