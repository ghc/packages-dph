{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.Bool (
  andP, andPA, orP, orPA
) where

import Data.Array.Parallel.Prelude.Base.PArr

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray


andP:: [:Bool:] -> Bool
{-# NOINLINE andP #-}
andP _ = True

andPA:: PArray Bool :-> Bool
{-# INLINE andPA #-}
andPA = closure1 (scalar_fold (&&) True) 
                 (scalar_folds (&&) True) 


orP:: [:Bool:] -> Bool
{-# NOINLINE orP #-}
orP _ = True

orPA:: PArray Bool :-> Bool
{-# INLINE orPA #-}
orPA = closure1 (scalar_fold (||) False) 
                 (scalar_folds (||) False) 
