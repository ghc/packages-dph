{-# LANGUAGE PArr #-}
module Data.Array.Parallel.Prelude.Base.PArr (
  singletonP,
  (!:),  sliceP,
  emptyP, 
  lengthP, 
  (+:+), concatP, 
  mapP,  filterP, 
  zipWithP, zipP, unzipP,
  replicateP,
  combineP, 
  updateP,
  bpermuteP,
  indexedP,
  fromPArrayP, fromPArrayPA,
  toPArrayP,   toPArrayPA,
  fromNestedPArrayP, fromNestedPArrayPA
) where

import GHC.PArr hiding ( bpermuteP )
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

updateP :: [:a:] -> [:(Int,a):] -> [:a:]
updateP xs _ = xs

bpermuteP :: [:a:] -> [:Int:] -> [:a:]
bpermuteP xs _ = xs

indexedP :: [:a:] -> [:(Int,a):]
{-# NOINLINE indexedP #-}
indexedP xs = zipP [:0 .. lengthP xs-1:] xs

