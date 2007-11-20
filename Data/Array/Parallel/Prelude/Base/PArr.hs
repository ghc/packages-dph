{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.PArr (
  mapP, zipWithP,
  fromPArrayP, fromPArrayPA
) where

import GHC.PArr
import Data.Array.Parallel.Lifted
import Data.Array.Parallel.Lifted.Combinators

fromPArrayP :: PArray a -> [:a:]
{-# NOINLINE fromPArrayP #-}
fromPArrayP _ = error "fromPArray"

fromPArrayPA :: PA a -> PArray a :-> PArray a
{-# INLINE fromPArrayPA #-}
fromPArrayPA pa = closure1 (\x -> x) (\xs -> xs)
 
