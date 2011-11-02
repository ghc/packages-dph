{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
-- NB: Cannot use any parallel array syntax except the type constructor

module Data.Array.Parallel.Prelude.Bool (
        Bool(..),
        otherwise
)
where
-- Primitives needed by the vectoriser.
import Data.Array.Parallel.Prim
        
{-# VECTORISE type Bool = Bool #-}

