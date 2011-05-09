{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Scalar
where
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.PArray.PReprInstances
import Data.Array.Parallel.PArray.PDataInstances
import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base (fromBool, toBool)
import GHC.Exts ( Int(..), (-#) )
import GHC.Word ( Word8 )


-- Pretend Bools are scalars --------------------------------------------------
instance Scalar Bool where
  {-# INLINE toScalarPData #-}
  toScalarPData bs
    = PBool (U.tagsToSel2 (U.map fromBool bs))

  {-# INLINE fromScalarPData #-}
  fromScalarPData (PBool sel) = U.map toBool (U.tagsSel2 sel)


-- Projections ----------------------------------------------------------------
prim_lengthPA :: Scalar a => PArray a -> Int
{-# INLINE prim_lengthPA #-}
prim_lengthPA xs = I# (lengthPA# xs)


-- Conversion -----------------------------------------------------------------
-- | Create a PArray out of a scalar U.Array, 
--   the first argument is the array length.
--
--   TODO: ditch this version, just use fromUArrPA'
--
fromUArrPA :: Scalar a => Int -> U.Array a -> PArray a
{-# INLINE fromUArrPA #-}
fromUArrPA (I# n#) xs = PArray n# (toScalarPData xs)


-- | Create a PArray out of a scalar U.Array,
--   reading the length directly from the U.Array.
fromUArrPA' :: Scalar a => U.Array a -> PArray a
{-# INLINE fromUArrPA' #-}
fromUArrPA' xs = fromUArrPA (U.length xs) xs


-- | Convert a PArray back to a plain U.Array.
toUArrPA :: Scalar a => PArray a -> U.Array a
{-# INLINE toUArrPA #-}
toUArrPA (PArray _ xs) = fromScalarPData xs


-- Tuple Conversions ----------------------------------------------------------
-- | Convert an U.Array of pairs to a PArray.
fromUArrPA_2
        :: (Scalar a, Scalar b)
        => Int -> U.Array (a,b) -> PArray (a,b)
{-# INLINE fromUArrPA_2 #-}
fromUArrPA_2 (I# n#) ps
  = PArray n# (P_2 (toScalarPData xs) (toScalarPData  ys))
  where
    (xs,ys) = U.unzip ps


-- | Convert a U.Array of pairs to a PArray,
--   reading the length directly from the U.Array.
fromUArrPA_2'
        :: (Scalar a, Scalar b)
        => U.Array (a,b) -> PArray (a, b)
{-# INLINE fromUArrPA_2' #-}
fromUArrPA_2' ps 
        = fromUArrPA_2 (U.length ps) ps


-- | Convert a U.Array of triples to a PArray.
fromUArrPA_3
        :: (Scalar a, Scalar b, Scalar c)
        => Int -> U.Array ((a,b),c) -> PArray (a,b,c)
{-# INLINE fromUArrPA_3 #-}
fromUArrPA_3 (I# n#) ps
  = PArray n# (P_3 (toScalarPData xs)
                   (toScalarPData ys)
                   (toScalarPData zs))
  where
    (qs,zs) = U.unzip ps
    (xs,ys) = U.unzip qs


-- | Convert a U.Array of triples to a PArray,
--   reading the length directly from the U.Array.
fromUArrPA_3' 
        :: (Scalar a, Scalar b, Scalar c)
        => U.Array ((a,b),c) -> PArray (a, b, c)
{-# INLINE fromUArrPA_3' #-}
fromUArrPA_3' ps = fromUArrPA_3 (U.length ps) ps


-- Nesting arrays -------------------------------------------------------------
-- | O(1). Create a nested array.
nestUSegdPA
        :: Int                  -- ^ total number of elements in the nested array
        -> U.Segd               -- ^ segment descriptor
        -> PArray a             -- ^ array of data elements.
        -> PArray (PArray a)

{-# INLINE nestUSegdPA #-}
nestUSegdPA (I# n#) segd (PArray _ xs)
        = PArray n# (PNested segd xs)


-- | O(1). Create a nested array,
--         using the same length as the source array.
nestUSegdPA'
        :: U.Segd               -- ^ segment descriptor
        -> PArray a             -- ^ array of data elements
        -> PArray (PArray a)

{-# INLINE nestUSegdPA' #-}
nestUSegdPA' segd xs 
        = nestUSegdPA (U.lengthSegd segd) segd xs


-- Scalar Operators -----------------------------------------------------------
-- These work on PArrays of scalar elements.
-- TODO: Why do we need these versions as well as the standard ones?

-- | Apply a worker function to every element of an array, yielding a new array.
scalar_map 
        :: (Scalar a, Scalar b) 
        => (a -> b) -> PArray a -> PArray b

{-# INLINE_PA scalar_map #-}
scalar_map f xs 
        = fromUArrPA (prim_lengthPA xs)
        . U.map f
        $ toUArrPA xs


-- | Zip two arrays, yielding a new array.
scalar_zipWith
        :: (Scalar a, Scalar b, Scalar c)
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c

{-# INLINE_PA scalar_zipWith #-}
scalar_zipWith f xs ys
        = fromUArrPA (prim_lengthPA xs)
        $ U.zipWith f (toUArrPA xs) (toUArrPA ys)


-- | Zip three arrays, yielding a new array.
scalar_zipWith3
        :: (Scalar a, Scalar b, Scalar c, Scalar d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d

{-# INLINE_PA scalar_zipWith3 #-}
scalar_zipWith3 f xs ys zs
        = fromUArrPA (prim_lengthPA xs)
        $ U.zipWith3 f (toUArrPA xs) (toUArrPA ys) (toUArrPA zs)


-- | Left fold over an array.
scalar_fold 
        :: Scalar a
        => (a -> a -> a) -> a -> PArray a -> a

{-# INLINE_PA scalar_fold #-}
scalar_fold f z
        = U.fold f z . toUArrPA


-- | Left fold over an array, using the first element to initialise the state.
scalar_fold1 
        :: Scalar a
        => (a -> a -> a) -> PArray a -> a

{-# INLINE_PA scalar_fold1 #-}
scalar_fold1 f
        = U.fold1 f . toUArrPA


-- | Segmented fold of an array of arrays.
--   Each segment is folded individually, yielding an array of the fold results.
scalar_folds 
        :: Scalar a
        => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a

{-# INLINE_PA scalar_folds #-}
scalar_folds f z xss
        = fromUArrPA (prim_lengthPA (concatPA# xss))
        . U.fold_s f z (segdPA# xss)
        . toUArrPA
        $ concatPA# xss


-- | Segmented fold of an array of arrays, using the first element of each
--   segment to initialse the state for that segment.
--   Each segment is folded individually, yielding an array of all the fold results.
scalar_fold1s
        :: Scalar a
        => (a -> a -> a) -> PArray (PArray a) -> PArray a

{-# INLINE_PA scalar_fold1s #-}
scalar_fold1s f xss
        = fromUArrPA (prim_lengthPA (concatPA# xss))
        . U.fold1_s f (segdPA# xss)
        . toUArrPA
         $ concatPA# xss


-- | Left fold over an array, also passing the index of each element
--   to the parameter function.
scalar_fold1Index
        :: Scalar a
        => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int

{-# INLINE_PA scalar_fold1Index #-}
scalar_fold1Index f
        = fst . U.fold1 f . U.indexed . toUArrPA


-- | Segmented fold over an array, also passing the index of each 
--   element to the parameter function.
scalar_fold1sIndex
        :: Scalar a
        => ((Int, a) -> (Int, a) -> (Int, a))
        -> PArray (PArray a) -> PArray Int

{-# INLINE_PA scalar_fold1sIndex #-}
scalar_fold1sIndex f (PArray m# (PNested segd xs))
        = PArray m#
        $ toScalarPData
        $ U.fsts
        $ U.fold1_s f segd
        $ U.zip (U.indices_s segd)
        $ fromScalarPData xs

