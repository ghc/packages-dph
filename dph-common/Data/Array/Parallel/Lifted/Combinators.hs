{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Combinators (
  lengthPA, replicatePA, singletonPA, mapPA, crossMapPA,
  zipWithPA, zipPA, unzipPA, 
  packPA, filterPA, combine2PA, indexPA, concatPA, appPA, enumFromToPA_Int,

  lengthPA_v, replicatePA_v, singletonPA_v, zipPA_v, unzipPA_v,
  indexPA_v, appPA_v, enumFromToPA_v
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Lifted.Selector

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base ( fromBool )

import GHC.Exts (Int(..), (+#), (-#), Int#, (<#))

lengthPA_v :: PA a -> PArray a -> Int
{-# INLINE_PA lengthPA_v #-}
lengthPA_v pa xs = I# (lengthPA# xs)

lengthPA_l :: PA a -> PArray (PArray a) -> PArray Int
{-# INLINE_PA lengthPA_l #-}
lengthPA_l pa xss = fromUArrPA (U.elementsSegd segd) (U.lengthsSegd segd)
  where
    segd = segdPA# xss

lengthPA :: PA a -> (PArray a :-> Int)
{-# INLINE lengthPA #-}
lengthPA pa = closure1 (lengthPA_v pa) (lengthPA_l pa)

replicatePA_v :: PA a -> Int -> a -> PArray a
{-# INLINE_PA replicatePA_v #-}
replicatePA_v pa (I# n#) x = replicatePA# pa n# x

replicatePA_l :: PA a -> PArray Int -> PArray a -> PArray (PArray a)
{-# INLINE_PA replicatePA_l #-}
replicatePA_l pa (PArray n# (PInt ns)) (PArray _ xs)
  = PArray n# (PNested (U.lengthsToSegd ns) xs)

replicatePA :: PA a -> (Int :-> a :-> PArray a)
{-# INLINE replicatePA #-}
replicatePA pa = closure2 dPA_Int (replicatePA_v pa) (replicatePA_l pa)

singletonPA_v :: PA a -> a -> PArray a
{-# INLINE_PA singletonPA_v #-}
singletonPA_v pa x = replicatePA_v pa 1 x

singletonPA_l :: PA a -> PArray a -> PArray (PArray a)
{-# INLINE_PA singletonPA_l #-}
singletonPA_l pa (PArray n# xs)
  = PArray n# (PNested (U.mkSegd (U.replicate (I# n#) 1)
                                 (U.enumFromStepLen 0 1 (I# n#))
                                 (I# n#))
                       xs)

singletonPA :: PA a -> (a :-> PArray a)
{-# INLINE singletonPA #-}
singletonPA pa = closure1 (singletonPA_v pa) (singletonPA_l pa)

mapPA_v :: PA a -> PA b -> (a :-> b) -> PArray a -> PArray b
{-# INLINE_PA mapPA_v #-}
mapPA_v pa pb f as = replicatePA# (dPA_Clo pa pb) (lengthPA# as) f
                     $:^ as

mapPA_l :: PA a -> PA b
        -> PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
{-# INLINE_PA mapPA_l #-}
mapPA_l pa pb fs xss
  = copySegdPA# xss
      (replicatelPA# (dPA_Clo pa pb) (segdPA# xss) fs $:^ concatPA# xss)

mapPA :: PA a -> PA b -> ((a :-> b) :-> PArray a :-> PArray b)
{-# INLINE mapPA #-}
mapPA pa pb = closure2 (dPA_Clo pa pb) (mapPA_v pa pb) (mapPA_l pa pb)

crossMapPA_v :: PA a -> PA b -> PArray a -> (a :-> PArray b) -> PArray (a,b)
{-# INLINE_PA crossMapPA_v #-}
crossMapPA_v pa pb as f
  = zipPA# (replicatelPA# pa (segdPA# bss) as) (concatPA# bss)
  where
    bss = mapPA_v pa (dPA_PArray pb) f as

crossMapPA_l :: PA a -> PA b
             -> PArray (PArray a)
             -> PArray (a :-> PArray b)
             -> PArray (PArray (a,b))
{-# INLINE_PA crossMapPA_l #-}
crossMapPA_l pa pb ass fs = copySegdPA# bss (zipPA# as' (concatPA# bss))
  where
    bsss = mapPA_l pa (dPA_PArray pb) fs ass
    bss  = concatPA_l pb bsss
    as' = replicatelPA# pa (segdPA# (concatPA# bsss)) (concatPA# ass)

crossMapPA :: PA a -> PA b -> (PArray a :-> (a :-> PArray b) :-> PArray (a,b))
{-# INLINE crossMapPA #-}
crossMapPA pa pb = closure2 (dPA_PArray pa) (crossMapPA_v pa pb)
                                            (crossMapPA_l pa pb)

zipPA_v :: PA a -> PA b -> PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA_v #-}
zipPA_v pa pb xs ys = zipPA# xs ys

zipPA_l :: PA a -> PA b
        -> PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a,b))
{-# INLINE_PA zipPA_l #-}
zipPA_l pa pb xss yss = copySegdPA# xss (zipPA# (concatPA# xss) (concatPA# yss))

zipPA :: PA a -> PA b -> (PArray a :-> PArray b :-> PArray (a,b))
{-# INLINE zipPA #-}
zipPA pa pb = closure2 (dPA_PArray pa) (zipPA_v pa pb) (zipPA_l pa pb)

zipWithPA_v :: PA a -> PA b -> PA c
            -> (a :-> b :-> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA zipWithPA_v #-}
zipWithPA_v pa pb pc f as bs = replicatePA# (dPA_Clo pa (dPA_Clo pb pc))
                                            (lengthPA# as)
                                            f
                                $:^ as $:^ bs

zipWithPA_l :: PA a -> PA b -> PA c
            -> PArray (a :-> b :-> c) -> PArray (PArray a) -> PArray (PArray b)
            -> PArray (PArray c)
{-# INLINE_PA zipWithPA_l #-}
zipWithPA_l pa pb pc fs ass bss
  = copySegdPA# ass
      (replicatelPA# (dPA_Clo pa (dPA_Clo pb pc))
                     (segdPA# ass) fs $:^ concatPA# ass $:^ concatPA# bss)

zipWithPA :: PA a -> PA b -> PA c
          -> ((a :-> b :-> c) :-> PArray a :-> PArray b :-> PArray c)
{-# INLINE zipWithPA #-}
zipWithPA pa pb pc = closure3 (dPA_Clo pa (dPA_Clo pb pc)) (dPA_PArray pa)
                              (zipWithPA_v pa pb pc)
                              (zipWithPA_l pa pb pc)

unzipPA_v:: PA a -> PA b -> PArray (a,b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA_v #-}
unzipPA_v pa pb abs = unzipPA# abs

unzipPA_l:: PA a -> PA b -> PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
{-# INLINE_PA unzipPA_l #-}
unzipPA_l pa pb xyss = zipPA# (copySegdPA# xyss xs) (copySegdPA# xyss ys)
  where
    (xs, ys) = unzipPA# (concatPA# xyss)

unzipPA:: PA a -> PA b -> (PArray (a, b) :-> (PArray a, PArray b))  
{-# INLINE unzipPA #-}
unzipPA pa pb =  closure1 (unzipPA_v pa pb) (unzipPA_l pa pb)

packPA_v :: PA a -> PArray a -> PArray Bool -> PArray a
{-# INLINE_PA packPA_v #-}
packPA_v pa xs bs
  = case U.count (toUArrPA bs) True of I# n# -> packPA# pa xs n# (toUArrPA bs)

packPA_l :: PA a
         -> PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
{-# INLINE_PA packPA_l #-}
packPA_l pa xss bss
  = segmentPA# (lengthPA# xss) (segdPA# xss)
  $ packPA# pa (concatPA# xss) (elementsSegd# segd') (toUArrPA (concatPA# bss))
  where
    segd' = U.lengthsToSegd
          . U.sum_s (segdPA# xss)
          . U.map fromBool
          $ toUArrPA (concatPA# bss)

packPA :: PA a -> (PArray a :-> PArray Bool :-> PArray a)
{-# INLINE packPA #-}
packPA pa = closure2 (dPA_PArray pa) (packPA_v pa) (packPA_l pa)


-- TODO: should the selector be a boolean array?
combine2PA_v:: PA a -> PArray a -> PArray a -> PArray Int -> PArray a
{-# INLINE_PA combine2PA_v #-}
combine2PA_v pa xs ys bs
  = combine2PA# pa (lengthPA# xs +# lengthPA# ys)
                   (tagsToSel2 (toUArrPA bs))
                   xs ys

combine2PA_l:: PA a -> PArray (PArray a) -> PArray (PArray a) -> PArray (PArray Int) -> PArray (PArray a)
{-# INLINE_PA combine2PA_l #-}
combine2PA_l _ _ _ _ = error "combinePA_l nyi"
    

combine2PA:: PA a -> (PArray a :-> PArray a :-> PArray Int :-> PArray a)
{-# INLINE_PA combine2PA #-}
combine2PA pa = closure3 (dPA_PArray pa) (dPA_PArray pa) (combine2PA_v pa) (combine2PA_l pa)


filterPA_v :: PA a -> (a :-> Bool) -> PArray a -> PArray a
{-# INLINE_PA filterPA_v #-}
filterPA_v pa p xs = packPA_v pa xs (mapPA_v pa dPA_Bool p xs)

filterPA_l :: PA a
           -> PArray (a :-> Bool) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE_PA filterPA_l #-}
filterPA_l pa ps xss = packPA_l pa xss (mapPA_l pa dPA_Bool ps xss)

filterPA :: PA a -> ((a :-> Bool) :-> PArray a :-> PArray a)
{-# INLINE filterPA #-}
filterPA pa = closure2 (dPA_Clo pa dPA_Bool) (filterPA_v pa) (filterPA_l pa)

indexPA_v :: PA a -> PArray a -> Int -> a
{-# INLINE_PA indexPA_v #-}
indexPA_v pa xs (I# i#) = indexPA# pa xs i#

indexPA_l :: PA a -> PArray (PArray a) -> PArray Int -> PArray a
{-# INLINE_PA indexPA_l #-}
indexPA_l pa xss is
  = bpermutePA# pa (concatPA# xss)
                   (lengthPA# xss)
                   (U.zipWith (+) (U.indicesSegd (segdPA# xss)) (toUArrPA is))

indexPA :: PA a -> (PArray a :-> Int :-> a)
{-# INLINE indexPA #-}
indexPA pa = closure2 (dPA_PArray pa) (indexPA_v pa) (indexPA_l pa)

concatPA_v :: PA a -> PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA_v #-}
concatPA_v pa xss = concatPA# xss

concatPA_l :: PA a -> PArray (PArray (PArray a)) -> PArray (PArray a)
{-# INLINE_PA concatPA_l #-}
concatPA_l pa (PArray m# (PNested segd1 (PNested segd2 xs)))
  = PArray m#
      (PNested (U.mkSegd (U.sum_s segd1 (U.lengthsSegd segd2))
                         (U.bpermute (U.indicesSegd segd2) (U.indicesSegd segd1))
                         (U.elementsSegd segd2))
               xs)

concatPA :: PA a -> (PArray (PArray a) :-> PArray a)
{-# INLINE concatPA #-}
concatPA pa = closure1 (concatPA_v pa) (concatPA_l pa)

appPA_v :: PA a -> PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA_v #-}
appPA_v pa xs ys = appPA# pa xs ys

appPA_l :: PA a -> PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE_PA appPA_l #-}
appPA_l pa xss yss
  = segmentPA# (lengthPA# xss +# lengthPA# yss)
               segd
               xys
  where
    xsegd = segdPA# xss
    ysegd = segdPA# yss

    segd = U.mkSegd (U.zipWith (+) (U.lengthsSegd xsegd) (U.lengthsSegd ysegd))
                    (U.zipWith (+) (U.indicesSegd xsegd) (U.indicesSegd ysegd))
                    (U.elementsSegd xsegd + U.elementsSegd ysegd)

    xys  = applPA# pa xsegd (concatPA# xss) ysegd (concatPA# yss) 

appPA :: PA a -> (PArray a :-> PArray a :-> PArray a)
{-# INLINE appPA #-}
appPA pa = closure2 (dPA_PArray pa) (appPA_v pa) (appPA_l pa)


enumFromToPA_v :: Int -> Int -> PArray Int
{-# INLINE_PA enumFromToPA_v #-}
enumFromToPA_v m n = fromUArrPA (distance m n) (U.enumFromTo m n)

distance :: Int -> Int -> Int
{-# INLINE_STREAM distance #-}
distance m n = max 0 (n - m + 1)

enumFromToPA_l :: PArray Int -> PArray Int -> PArray (PArray Int)
{-# INLINE_PA enumFromToPA_l #-}
enumFromToPA_l ms ns
  = segmentPA# (lengthPA# ms) segd
  . fromUArrPA (I# (lengthPA# ms))
  . U.enumFromToEach (U.elementsSegd segd)
  $ U.zip (toUArrPA ms) (toUArrPA ns)
  where
    segd = U.lengthsToSegd
         $ U.zipWith distance (toUArrPA ms) (toUArrPA ns)

enumFromToPA_Int :: Int :-> Int :-> PArray Int
{-# INLINE enumFromToPA_Int #-}
enumFromToPA_Int = closure2 dPA_Int enumFromToPA_v enumFromToPA_l

