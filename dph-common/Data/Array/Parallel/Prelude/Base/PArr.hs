{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.PArr (
  mapP, filterP, combineP, emptyP, zipWithP, (!:), lengthP, concatP, zipP, unzipP, singletonP, (+:+), replicateP,
  sliceP,
  fromPArrayP, fromPArrayPA,
  toPArrayP, toPArrayPA,
  fromNestedPArrayP, fromNestedPArrayPA
) where

import GHC.PArr
import Data.Array.Parallel.Lifted
import Data.Array.Parallel.Lifted.Closure

fromPArrayP :: PArray a -> [:a:]
{-# NOINLINE fromPArrayP #-}
fromPArrayP _ 
    = singletonP (error "dph-common uses of fromPArrayP must be vectorised")

fromPArrayPA :: PA a => PArray a :-> PArray a
{-# INLINE fromPArrayPA #-}
fromPArrayPA = closure1 (\x -> x) (\xs -> xs)

toPArrayP :: [:a:] -> PArray a
{-# NOINLINE toPArrayP #-}
toPArrayP _ 
    = error "dph-common: uses of toPArrayP must be vectorised"

toPArrayPA :: PA a => PArray a :-> PArray a
{-# INLINE toPArrayPA #-}
toPArrayPA = closure1 (\x -> x) (\xs -> xs)

fromNestedPArrayP :: PArray (PArray a) -> [:[:a:]:]
{-# NOINLINE fromNestedPArrayP #-}
fromNestedPArrayP _ 
    = singletonP (error "dph-common: uses of fromNestedPArrayP must be vectorised")

fromNestedPArrayPA :: PA a => (PArray (PArray a) :-> PArray (PArray a))
{-# INLINE fromNestedPArrayPA #-}
fromNestedPArrayPA = closure1 (\xs -> xs) (\xss -> xss)

combineP:: [:a:] -> [:a:] -> [:Int:] -> [:a:]
combineP xs _ _ = xs
