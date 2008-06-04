{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Prim
where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances

import Data.Array.Parallel.Base ((:*:)(..), fstS, pairS, unpairS)

import GHC.Exts ( Int(..), (-#) )

unsafe_map :: (PrimPA a, PrimPA b) => (a -> b) -> PArray a -> PArray b
{-# INLINE_PA unsafe_map #-}
unsafe_map f xs = fromUArrPA (prim_lengthPA xs)
                . mapU f
                $ toUArrPA xs

unsafe_zipWith :: (PrimPA a, PrimPA b, PrimPA c)
               => (a -> b -> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA unsafe_zipWith #-}
unsafe_zipWith f xs ys = fromUArrPA (prim_lengthPA xs)
                       $ zipWithU f (toUArrPA xs) (toUArrPA ys)

unsafe_fold :: PrimPA a => (a -> a -> a) -> a -> PArray a -> a
{-# INLINE_PA unsafe_fold #-}
unsafe_fold f z = foldU f z . toUArrPA

unsafe_fold1 :: PrimPA a => (a -> a -> a) -> PArray a -> a
{-# INLINE_PA unsafe_fold1 #-}
unsafe_fold1 f = fold1U f . toUArrPA

unsafe_folds :: PrimPA a => (a -> a -> a) -> a -> PArray (PArray a) -> PArray a
{-# INLINE_PA unsafe_folds #-}
unsafe_folds f z xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                     . foldSU f z
                     $ toSUArrPA xss

unsafe_fold1s :: PrimPA a => (a -> a -> a) -> PArray (PArray a) -> PArray a
{-# INLINE_PA unsafe_fold1s #-}
unsafe_fold1s f xss = fromUArrPA (prim_lengthPA (concatPA# xss))
                    . fold1SU f
                    $ toSUArrPA xss

unsafe_fold1Index :: PrimPA a
                  => ((Int, a) -> (Int, a) -> (Int, a)) -> PArray a -> Int
{-# INLINE_PA unsafe_fold1Index #-}
unsafe_fold1Index f = fstS . fold1U f' . indexedU . toUArrPA
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)

unsafe_fold1sIndex :: PrimPA a
                   => ((Int, a) -> (Int, a) -> (Int, a))
                   -> PArray (PArray a) -> PArray Int
{-# INLINE_PA unsafe_fold1sIndex #-}
unsafe_fold1sIndex f xss = fromUArrPA (nested_lengthPA xss) 
                         . fstU
                         . fold1SU f'
                         . indexedSU
                         $ toSUArrPA xss
  where
    {-# INLINE f' #-}
    f' p q = pairS $ f (unpairS p) (unpairS q)



unsafe_enumFromTo:: Int -> Int -> PArray Int
{-# INLINE_PA unsafe_enumFromTo #-}
unsafe_enumFromTo s e = fromUArrPA  (max 0 (e-s+1)) $ enumFromToU s e

unsafe_enumFromTos:: PArray Int -> PArray Int -> PArray (PArray Int)
{-# INLINE_PA unsafe_enumFromTos #-}
unsafe_enumFromTos ss es = fromSUArrPA  flatLen nestedLen  $ enumFromToSU (toUArrPA ss) (toUArrPA es)
  where
    flatLen   = prim_lengthPA ss
    nestedLen = unsafe_fold (+) 0 (unsafe_map (\x -> max (x+1) 0) $  unsafe_zipWith (-) es ss)

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
      ts = mapU (\b -> if b then 1 else 0) bs

      is = zipWith3U if_ ts (scanU (+) 0 ts) (scanU (+) 0 $ mapU not_ ts)

      m# = case sumU ts of I# m# -> m#

      {-# INLINE if_ #-}
      if_ 0 x y = y
      if_ _ x y = x

      {-# INLINE not_ #-}
      not_ 0 = 1
      not_ _ = 0

  {-# INLINE toUArrPA #-}
  toUArrPA (PBool _ ts _ _ _) = mapU (/= 0) ts

  primPA = dPA_Bool


fromUArrPA_2 :: (PrimPA a, PrimPA b) => Int -> UArr (a :*: b) -> PArray (a,b)
{-# INLINE fromUArrPA_2 #-}
fromUArrPA_2 (I# n#) ps = P_2 n# (fromUArrPA (I# n#) xs) (fromUArrPA (I# n#) ys)
  where
    xs :*: ys = unzipU ps



fromUArrPA_2' :: (PrimPA a, PrimPA b) => UArr (a :*: b) -> PArray (a, b)
{-# INLINE fromUArrPA_2' #-}
fromUArrPA_2' ps = fromUArrPA_2 (lengthU ps) ps

fromUArrPA_3 :: (PrimPA a, PrimPA b, PrimPA c) => Int -> UArr (a :*: b :*: c) -> PArray (a,b,c)
{-# INLINE fromUArrPA_3 #-}
fromUArrPA_3 (I# n#) ps = P_3 n# (fromUArrPA (I# n#) xs) (fromUArrPA (I# n#) ys) (fromUArrPA (I# n#) zs)
  where
    xs :*: ys :*: zs = unzip3U ps

fromUArrPA_3' :: (PrimPA a, PrimPA b, PrimPA c) => UArr (a :*: b :*: c) -> PArray (a, b, c)
{-# INLINE fromUArrPA_3' #-}
fromUArrPA_3' ps = fromUArrPA_3 (lengthU ps) ps

fromSUArrPA :: PrimPA a => Int -> Int -> SUArr a -> PArray (PArray a)
{-# INLINE fromSUArrPA #-}
fromSUArrPA (I# m#) n xss
  = PNested m# (lengthsSU xss)
               (indicesSU xss)
               (fromUArrPA n (concatSU xss))

toSUArrPA :: PrimPA a => PArray (PArray a) -> SUArr a
{-# INLINE toSUArrPA #-}
toSUArrPA (PNested _ lens idxs xs) = toUSegd (zipU lens idxs) >: toUArrPA xs

fromSUArrPA_2 :: (PrimPA a, PrimPA b)
              => Int -> Int -> SUArr (a :*: b) -> PArray (PArray (a, b))
{-# INLINE fromSUArrPA_2 #-}
fromSUArrPA_2 (I# m#) n pss = PNested m# (lengthsSU pss)
                                         (indicesSU pss)
                                         (fromUArrPA_2 n (concatSU pss))

fromSUArrPA' :: PrimPA a => SUArr a -> PArray (PArray a)
{-# INLINE fromSUArrPA' #-}
fromSUArrPA' xss = fromSUArrPA (lengthSU xss)
                               (lengthU (concatSU xss))
                               xss

fromSUArrPA_2' :: (PrimPA a, PrimPA b)
                => SUArr (a :*: b) -> PArray (PArray (a, b))
{-# INLINE fromSUArrPA_2' #-}
fromSUArrPA_2' pss = fromSUArrPA_2 (lengthSU pss)
                                   (lengthU (concatSU pss))
                                   pss

