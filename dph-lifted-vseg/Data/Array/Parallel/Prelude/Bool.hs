{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
-- NB: Cannot use any parallel array syntax except the type constructor

module Data.Array.Parallel.Prelude.Bool (
        Bool,
)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.Lifted
import qualified Prelude as P
import Prelude (Bool)
        
{-# VECTORISE type Bool = Bool #-}

