{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Prim
where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances

import qualified Data.Array.Parallel.Unlifted as U

import Data.Array.Parallel.Base ((:*:)(..), fstS, pairS, unpairS)

import GHC.Exts ( Int(..), (-#) )

unsafe_map :: (PrimPA a, PrimPA b) => (a -> b) -> PArray a -> PArray b
{-# INLINE_PA unsafe_map #-}
unsafe_map f xs = fromUArrPA (prim_lengthPA xs)
                . U.map f
                $ toUArrPA xs

unsafe_zipWith :: (PrimPA a, PrimPA b, PrimPA c)
               => (a -> b -> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA unsafe_zipWith #-}
unsafe_zipWith f xs ys = fromUArrPA (prim_lengthPA xs)
                       $ U.zipWith f (toUArrPA xs) (toUArrPA ys)

unsafe_fold :: PrimPA a => (a -> a -> a) -> a -> PArray a -> a
{-# INLINE_PA unsafe_fold #-}
unsafe_fold f z = U.fold f z . toUArrPA

unsafe_fold1 :: PrimPA a => (a -> a -> a) -> PArray a -> a
{-# INLINE_PA unsafe_fold1 #-}
unsafe_fold1 f = U.fold1 f . toUArrPA

unsafe_folds :: PrimPA a => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a
{-# INLINE_PA unsafe_folds #-}
unsafe_folds f z xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                     . U.fold_s f z
                     $ toSUArrPA xss

unsafe_fold1s :: PrimPA a => (a -> a -> a) -> PArray (PArray a) -> PArray a
{-# INLINE_PA unsafe_fold1s #-}
unsafe_fold1s f xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                    . U.fold1_s f
                    $ toSUArrPA xss

unsafe_fold1Index :: PrimPA a
                  => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int
{-# INLINE_PA unsafe_fold1Index #-}
unsafe_fold1Index f = fstS . U.fold1 f' . U.indexed . toUArrPA
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)

unsafe_fold1sIndex :: PrimPA a
                   => ((Int, a) -> (Int, a) -> (Int, a))
                   -> PArray (PArray a) -> PArray Int
{-# INLINE_PA unsafe_fold1sIndex #-}
unsafe_fold1sIndex f xss = fromUArrPA (nested_lengthPA xss) 
                         . U.fsts
                         . U.fold1_s f'
                         . U.indexed_s
                         $ toSUArrPA xss
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)

instance PrimPA Int where
  fromUArrPA (I# n#) xs  = PInt n# xs
  toUArrPA   (PInt _ xs) = xs
  primPA = dPA_Int

instance PrimPA Double where
  fromUArrPA (I# n#) xs     = PDouble n# xs
  toUArrPA   (PDouble _ xs) = xs
  primPA = dPA_Double

instance PrimPA Bool where
  {-# INLINE fromUArrPA #-}
  fromUArrPA (I# n#) bs
    = PBool n# ts is
            (PVoid (n# -# m#))
            (PVoid m#)
    where
      ts = U.map (\b -> if b then 1 else 0) bs

      is = U.zipWith3 if_ ts (U.scan (+) 0 ts) (U.scan (+) 0 $ U.map not_ ts)

      m# = case U.sum ts of I# m# -> m#

      {-# INLINE if_ #-}
      if_ 0 x y = y
      if_ _ x y = x

      {-# INLINE not_ #-}
      not_ 0 = 1
      not_ _ = 0

  {-# INLINE toUArrPA #-}
  toUArrPA (PBool _ ts _ _ _) = U.map (/= 0) ts

  primPA = dPA_Bool


fromUArrPA_2 :: (PrimPA a, PrimPA b) => Int -> U.Array (a :*: b) -> PArray (a,b)
{-# INLINE fromUArrPA_2 #-}
fromUArrPA_2 (I# n#) ps = P_2 n# (fromUArrPA (I# n#) xs) (fromUArrPA (I# n#) ys)
  where
    xs :*: ys = U.unzip ps



fromUArrPA_2' :: (PrimPA a, PrimPA b) => U.Array (a :*: b) -> PArray (a, b)
{-# INLINE fromUArrPA_2' #-}
fromUArrPA_2' ps = fromUArrPA_2 (U.length ps) ps

fromUArrPA_3 :: (PrimPA a, PrimPA b, PrimPA c) => Int -> U.Array (a :*: b :*: c) -> PArray (a,b,c)
{-# INLINE fromUArrPA_3 #-}
fromUArrPA_3 (I# n#) ps = P_3 n# (fromUArrPA (I# n#) xs) (fromUArrPA (I# n#) ys) (fromUArrPA (I# n#) zs)
  where
    xs :*: ys :*: zs = U.unzip3 ps

fromUArrPA_3' :: (PrimPA a, PrimPA b, PrimPA c) => U.Array (a :*: b :*: c) -> PArray (a, b, c)
{-# INLINE fromUArrPA_3' #-}
fromUArrPA_3' ps = fromUArrPA_3 (U.length ps) ps

fromSUArrPA :: PrimPA a => Int -> Int -> U.SArray a -> PArray (PArray a)
{-# INLINE fromSUArrPA #-}
fromSUArrPA (I# m#) n xss
  = PNested m# (U.lengths_s xss)
               (U.indices_s xss)
               (fromUArrPA n (U.concat xss))

toSUArrPA :: PrimPA a => PArray (PArray a) -> U.SArray a
{-# INLINE toSUArrPA #-}
toSUArrPA (PNested _ lens idxs xs) = U.toSegd (U.zip lens idxs) U.>: toUArrPA xs

fromSUArrPA_2 :: (PrimPA a, PrimPA b)
              => Int -> Int -> U.SArray (a :*: b) -> PArray (PArray (a, b))
{-# INLINE fromSUArrPA_2 #-}
fromSUArrPA_2 (I# m#) n pss = PNested m# (U.lengths_s pss)
                                         (U.indices_s pss)
                                         (fromUArrPA_2 n (U.concat pss))

fromSUArrPA' :: PrimPA a => U.SArray a -> PArray (PArray a)
{-# INLINE fromSUArrPA' #-}
fromSUArrPA' xss = fromSUArrPA (U.length_s xss)
                               (U.length (U.concat xss))
                               xss

fromSUArrPA_2' :: (PrimPA a, PrimPA b)
                => U.SArray (a :*: b) -> PArray (PArray (a, b))
{-# INLINE fromSUArrPA_2' #-}
fromSUArrPA_2' pss = fromSUArrPA_2 (U.length_s pss)
                                   (U.length (U.concat pss))
                                   pss

