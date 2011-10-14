
{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"

module Data.Array.Parallel.PArray.Scalar 
        ( Scalar(..)
        , scalar_map
        , scalar_zipWith
        , scalar_zipWith3)
where
import Data.Array.Parallel.PArray.PData
import qualified Data.Array.Parallel.Unlifted   as U


class U.Elt a => Scalar a where
  fromScalarPData :: PData a -> U.Array a
  toScalarPData   :: U.Array a -> PData a


instance Scalar Int where
  fromScalarPData (PInt xs)     = xs
  toScalarPData                 = PInt

instance Scalar Double where
  fromScalarPData (PDouble xs)  = xs
  toScalarPData                 = PDouble


-- Shorthands used in this module only
from    = fromScalarPData
to      = toScalarPData


-- | Apply a worker function to every element of an array, yielding a new array.
scalar_map 
        :: (Scalar a, Scalar b) 
        => (a -> b) -> PArray a -> PArray b

{-# INLINE_PA scalar_map #-}
scalar_map f (PArray len xs)
        = PArray len $ to $ U.map f (from xs)


-- | Zip two arrays, yielding a new array.
scalar_zipWith
        :: (Scalar a, Scalar b, Scalar c)
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c

{-# INLINE_PA scalar_zipWith #-}
scalar_zipWith f (PArray len xs) (PArray _ ys)
        = PArray len $ to $ U.zipWith f (from xs) (from ys)


-- | Zip three arrays, yielding a new array.
scalar_zipWith3
        :: (Scalar a, Scalar b, Scalar c, Scalar d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d

{-# INLINE_PA scalar_zipWith3 #-}
scalar_zipWith3 f (PArray len xs) (PArray _ ys) (PArray _ zs)
        = PArray len $ to $ U.zipWith3 f (from xs) (from ys) (from zs)
