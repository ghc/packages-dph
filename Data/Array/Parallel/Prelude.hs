{-# LANGUAGE PArr #-}

module Data.Array.Parallel.Prelude (
    module Data.Array.Parallel.Prelude.PArr
  , module Data.Array.Parallel.Prelude.Int
  
  , PArray, fromPArrayP, fromPArrayPA
) where

import Data.Array.Parallel.Prelude.PArr
import Data.Array.Parallel.Prelude.Int

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators

fromPArrayP :: PArray a -> [:a:]
{-# NOINLINE fromPArrayP #-}
fromPArrayP _ = error "fromPArray"

fromPArrayPA :: PA a -> PArray a :-> PArray a
{-# INLINE fromPArrayPA #-}
fromPArrayPA pa = closure1 (\x -> x) (\xs -> xs)
 
