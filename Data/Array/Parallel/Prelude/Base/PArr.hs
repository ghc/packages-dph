{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.PArr (
  mapP, zipWithP,
  fromPArrayP, fromPArrayPA,

  PrimPA(..)
) where

import GHC.PArr
import Data.Array.Parallel.Lifted
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Unlifted (UArr)

fromPArrayP :: PArray a -> [:a:]
{-# NOINLINE fromPArrayP #-}
fromPArrayP _ = error "fromPArray"

fromPArrayPA :: PA a -> PArray a :-> PArray a
{-# INLINE fromPArrayPA #-}
fromPArrayPA pa = closure1 (\x -> x) (\xs -> xs)

class PrimPA a where
  fromUArrPA :: UArr a -> PArray a
  toUArrPA   :: PArray a -> UArr a

instance PrimPA Double where
  fromUArrPA = fromUArrPA_Double
  toUArrPA   = toUArrPA_Double

