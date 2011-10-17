#include "fusion-phases.h"

module Data.Array.Parallel.PArray.Scalar 
        ( Scalar(..)
        , toUArrayPA,   fromUArrayPA
        , fromUArrayPA_2

        , scalar_map
        , scalar_zipWith
        , scalar_zipWith3)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.Base
import qualified Data.Array.Parallel.Unlifted   as U
import GHC.Exts

class U.Elt a => Scalar a where
  fromScalarPData :: PData a -> U.Array a
  toScalarPData   :: U.Array a -> PData a

-- Shorthands used in this module only
from    = fromScalarPData
to      = toScalarPData


-- Instances --------------------------------------------------------------
instance Scalar Int where
  fromScalarPData (PInt xs)     = xs
  toScalarPData                 = PInt

instance Scalar Double where
  fromScalarPData (PDouble xs)  = xs
  toScalarPData                 = PDouble

instance Scalar Bool where
  {-# INLINE toScalarPData #-}
  toScalarPData bs
    = PBool (U.tagsToSel2 (U.map fromBool bs))

  {-# INLINE fromScalarPData #-}
  fromScalarPData (PBool sel) = U.map toBool (U.tagsSel2 sel)



-- Array Conversions -------------------------------------------------------
{-# INLINE_PA fromUArrayPA #-}
fromUArrayPA  :: (Scalar a, U.Elt a) => U.Array a -> PArray a
fromUArrayPA uarr
 = let  I# n#    = U.length uarr
   in   PArray n# (toScalarPData uarr) 
 
 
{-# INLINE_PA toUArrayPA #-}
toUArrayPA    :: (PA a, Scalar a) => PArray a -> U.Array a
toUArrayPA (PArray n# pdata)
        = fromScalarPData pdata
 

-- Tuple Conversions ----------------------------------------------------------
-- | Convert an U.Array of pairs to a PArray.
fromUArrayPA_2
        :: (Scalar a, Scalar b)
        => U.Array (a,b) -> PArray (a,b)
{-# INLINE fromUArrayPA_2 #-}
fromUArrayPA_2 ps
 = let  I# n#   = U.length ps
        (xs,ys) = U.unzip ps
    in  PArray n# (PTuple2 (toScalarPData xs) (toScalarPData  ys))
    

-- Scalar Operators -----------------------------------------------------------
-- These work on PArrays of scalar elements.
-- TODO: Why do we need these versions as well as the standard ones?

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
