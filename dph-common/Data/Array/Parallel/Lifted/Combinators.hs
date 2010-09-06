{-# LANGUAGE CPP, BangPatterns #-}

#include "fusion-phases.h"

-- | Array combinators that do not have corresponding primitive operators
--	in dph-common Data.Array.Parallel.Lifted.Combinators are defined here.
--
module Data.Array.Parallel.Lifted.Combinators (
  lengthPA, replicatePA, singletonPA, mapPA, crossMapPA,
  zipWithPA, zipPA, unzipPA, 
  packPA, filterPA, combine2PA, indexPA, concatPA, appPA, enumFromToPA_Int,
  indexedPA, slicePA, updatePA, bpermutePA,

  lengthPA_v, replicatePA_v, singletonPA_v, zipPA_v, unzipPA_v,
  indexPA_v, appPA_v, enumFromToPA_v
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd#, elementsSel2_0#,
                                            elementsSel2_1# )
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Scalar

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base ( fromBool )

import GHC.Exts (Int(..), (+#), (-#), Int#, (<#))


-- length ---------------------------------------------------------------------
lengthPA_v :: PA a => PArray a -> Int
{-# INLINE_PA lengthPA_v #-}
lengthPA_v xs = I# (lengthPA# xs)

lengthPA_l :: PA a => PArray (PArray a) -> PArray Int
{-# INLINE_PA lengthPA_l #-}
lengthPA_l xss = fromUArrPA (U.elementsSegd segd) (U.lengthsSegd segd)
  where
    segd = segdPA# xss

lengthPA :: PA a => PArray a :-> Int
{-# INLINE lengthPA #-}
lengthPA = closure1 lengthPA_v lengthPA_l


-- replicate ------------------------------------------------------------------
replicatePA_v :: PA a => Int -> a -> PArray a
{-# INLINE_PA replicatePA_v #-}
replicatePA_v (I# n#) x = replicatePA# n# x

replicatePA_l :: PA a => PArray Int -> PArray a -> PArray (PArray a)
{-# INLINE_PA replicatePA_l #-}
replicatePA_l (PArray n# (PInt ns)) (PArray _ xs)
  = PArray n# (PNested segd (replicatelPD segd xs))
  where
    segd = U.lengthsToSegd ns

replicatePA :: PA a => Int :-> a :-> PArray a
{-# INLINE replicatePA #-}
replicatePA = closure2 replicatePA_v replicatePA_l


-- singleton ------------------------------------------------------------------
singletonPA_v :: PA a => a -> PArray a
{-# INLINE_PA singletonPA_v #-}
singletonPA_v x = replicatePA_v 1 x

singletonPA_l :: PA a => PArray a -> PArray (PArray a)
{-# INLINE_PA singletonPA_l #-}
singletonPA_l (PArray n# xs)
  = PArray n# (PNested (U.mkSegd (U.replicate (I# n#) 1)
                                 (U.enumFromStepLen 0 1 (I# n#))
                                 (I# n#))
                       xs)

singletonPA :: PA a => a :-> PArray a
{-# INLINE singletonPA #-}
singletonPA = closure1 singletonPA_v singletonPA_l


-- map ------------------------------------------------------------------------
mapPA_v :: (PA a, PA b) => (a :-> b) -> PArray a -> PArray b
{-# INLINE_PA mapPA_v #-}
mapPA_v f as = replicatePA# (lengthPA# as) f $:^ as

mapPA_l :: (PA a, PA b)
        => PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
{-# INLINE_PA mapPA_l #-}
mapPA_l (PArray n# clo) (PArray _ xss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    PNested segd
  $ liftedApply (case U.elementsSegd segd of { I# k# -> k# })
                (replicatelPD segd clo)
                xs }
  
{-
mapPA_l fs xss
  = copySegdPA# xss
      (replicatelPA# (segdPA# xss) fs $:^ concatPA# xss)
-}

mapPA :: (PA a, PA b) => (a :-> b) :-> PArray a :-> PArray b
{-# INLINE mapPA #-}
mapPA = closure2 mapPA_v mapPA_l


-- crossMap -------------------------------------------------------------------
crossMapPA_v :: (PA a, PA b) => PArray a -> (a :-> PArray b) -> PArray (a,b)
{-# INLINE_PA crossMapPA_v #-}
crossMapPA_v as f
  = zipPA# (replicatelPA# (segdPA# bss) as) (concatPA# bss)
  where
    bss = mapPA_v f as

crossMapPA_l :: (PA a, PA b)
             => PArray (PArray a)
             -> PArray (a :-> PArray b)
             -> PArray (PArray (a,b))
{-# INLINE_PA crossMapPA_l #-}
crossMapPA_l ass fs = copySegdPA# bss (zipPA# as' (concatPA# bss))
  where
    bsss = mapPA_l fs ass
    bss  = concatPA_l bsss
    as' = replicatelPA# (segdPA# (concatPA# bsss)) (concatPA# ass)

crossMapPA :: (PA a, PA b) => (PArray a :-> (a :-> PArray b) :-> PArray (a,b))
{-# INLINE crossMapPA #-}
crossMapPA = closure2 crossMapPA_v crossMapPA_l


-- zip ------------------------------------------------------------------------
zipPA_v :: (PA a, PA b) => PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA_v #-}
zipPA_v xs ys = zipPA# xs ys

zipPA_l :: (PA a, PA b)
        => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a,b))
{-# INLINE_PA zipPA_l #-}
zipPA_l (PArray n# (PNested segd xs)) (PArray _ (PNested _ ys))
  = PArray n# (PNested segd (P_2 xs ys))
-- zipPA_l xss yss = copySegdPA# xss (zipPA# (concatPA# xss) (concatPA# yss))

zipPA :: (PA a, PA b) => PArray a :-> PArray b :-> PArray (a,b)
{-# INLINE zipPA #-}
zipPA = closure2 zipPA_v zipPA_l


-- zipWith --------------------------------------------------------------------
zipWithPA_v :: (PA a, PA b, PA c)
            => (a :-> b :-> c) -> PArray a -> PArray b -> PArray c
{-# INLINE_PA zipWithPA_v #-}
zipWithPA_v f as bs = replicatePA# (lengthPA# as) f $:^ as $:^ bs

zipWithPA_l :: (PA a, PA b, PA c)
            => PArray (a :-> b :-> c) -> PArray (PArray a) -> PArray (PArray b)
            -> PArray (PArray c)
{-# INLINE_PA zipWithPA_l #-}
zipWithPA_l fs ass bss
  = copySegdPA# ass
      (replicatelPA# (segdPA# ass) fs $:^ concatPA# ass $:^ concatPA# bss)

zipWithPA :: (PA a, PA b, PA c)
          => (a :-> b :-> c) :-> PArray a :-> PArray b :-> PArray c
{-# INLINE zipWithPA #-}
zipWithPA = closure3 zipWithPA_v zipWithPA_l


-- unzip ----------------------------------------------------------------------
unzipPA_v:: (PA a, PA b) => PArray (a,b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA_v #-}
unzipPA_v abs = unzipPA# abs

unzipPA_l:: (PA a, PA b) => PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
{-# INLINE_PA unzipPA_l #-}
unzipPA_l xyss = zipPA# (copySegdPA# xyss xs) (copySegdPA# xyss ys)
  where
    (xs, ys) = unzipPA# (concatPA# xyss)

unzipPA:: (PA a, PA b) => PArray (a, b) :-> (PArray a, PArray b)
{-# INLINE unzipPA #-}
unzipPA = closure1 unzipPA_v unzipPA_l


-- packPA ---------------------------------------------------------------------
boolSel :: PArray Bool -> U.Sel2
{-# INLINE boolSel #-}
boolSel (PArray _ (PBool sel)) = sel

packPA_v :: PA a => PArray a -> PArray Bool -> PArray a
{-# INLINE_PA packPA_v #-}
packPA_v xs bs
  = packByTagPA# xs (elementsSel2_1# sel) (U.tagsSel2 sel) 1#
  -- = case U.count (toUArrPA bs) True of I# n# -> packPA# xs n# (toUArrPA bs)
  where
    sel = boolSel bs

packPA_l :: PA a
         => PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
{-# INLINE_PA packPA_l #-}
packPA_l (PArray n# xss) (PArray _ bss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    case bss of { PNested _ (PBool sel) ->
    PNested (U.lengthsToSegd $ U.count_s segd (U.tagsSel2 sel) 1)
  $ packByTagPD  xs (elementsSel2_1# sel) (U.tagsSel2 sel) 1# }}

{-
packPA_l !xss !bss
  = segmentPA# (lengthPA# xss) segd'
  $ packByTagPA# (concatPA# xss) (elementsSel2_1# sel) (tagsSel2 sel) 1#
  where
    sel   = boolSel (concatPA# bss)
    segd' = U.lengthsToSegd
          $ U.count_s (segdPA# xss) (tagsSel2 sel) 1
-}
{-
  = segmentPA# (lengthPA# xss) (segdPA# xss)
  $ packPA# (concatPA# xss) (elementsSegd# segd') (toUArrPA (concatPA# bss))
  where
    segd' = U.lengthsToSegd
          . U.sum_s (segdPA# xss)
          . U.map fromBool
          $ toUArrPA (concatPA# bss)
-}

packPA :: PA a => PArray a :-> PArray Bool :-> PArray a
{-# INLINE packPA #-}
packPA = closure2 packPA_v packPA_l


-- combine --------------------------------------------------------------------
-- TODO: should the selector be a boolean array?
combine2PA_v:: PA a => PArray a -> PArray a -> PArray Int -> PArray a
{-# INLINE_PA combine2PA_v #-}
combine2PA_v xs ys bs
  = combine2PA# (lengthPA# xs +# lengthPA# ys)
                (U.tagsToSel2 (toUArrPA bs))
                xs ys

combine2PA_l:: PA a
            => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray Int)
               -> PArray (PArray a)
{-# INLINE_PA combine2PA_l #-}
combine2PA_l _ _ _ = error "combinePA_l nyi"
    

combine2PA:: PA a => PArray a :-> PArray a :-> PArray Int :-> PArray a
{-# INLINE_PA combine2PA #-}
combine2PA = closure3 combine2PA_v combine2PA_l


-- filter ---------------------------------------------------------------------
filterPA_v :: PA a => (a :-> Bool) -> PArray a -> PArray a
{-# INLINE_PA filterPA_v #-}
filterPA_v p xs = packPA_v xs (mapPA_v p xs)

filterPA_l :: PA a
           => PArray (a :-> Bool) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE_PA filterPA_l #-}
filterPA_l ps xss = packPA_l xss (mapPA_l ps xss)

filterPA :: PA a => (a :-> Bool) :-> PArray a :-> PArray a
{-# INLINE filterPA #-}
filterPA = closure2 filterPA_v filterPA_l


-- index ----------------------------------------------------------------------
indexPA_v :: PA a => PArray a -> Int -> a
{-# INLINE_PA indexPA_v #-}
indexPA_v xs (I# i#) = indexPA# xs i#

indexPA_l :: PA a => PArray (PArray a) -> PArray Int -> PArray a
{-# INLINE_PA indexPA_l #-}
indexPA_l (PArray _ (PNested segd xs)) (PArray n# is)
  = PArray n#
  $ bpermutePD xs n#
                  (U.zipWith (+) (U.indicesSegd segd)
                                 (fromScalarPData is))

{-
indexPA_l xss is
  = bpermutePA# (concatPA# xss)
                (lengthPA# xss)
                (U.zipWith (+) (U.indicesSegd (segdPA# xss)) (toUArrPA is))
-}

indexPA :: PA a => PArray a :-> Int :-> a
{-# INLINE indexPA #-}
indexPA = closure2 indexPA_v indexPA_l


-- concat ---------------------------------------------------------------------
concatPA_v :: PA a => PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA_v #-}
concatPA_v xss = concatPA# xss

concatPA_l :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
{-# INLINE_PA concatPA_l #-}
concatPA_l (PArray m# (PNested segd1 (PNested segd2 xs)))
  = PArray m#
      (PNested (U.mkSegd (U.sum_s segd1 (U.lengthsSegd segd2))
                         (U.bpermute (U.indicesSegd segd2) (U.indicesSegd segd1))
                         (U.elementsSegd segd2))
               xs)

concatPA :: PA a => PArray (PArray a) :-> PArray a
{-# INLINE concatPA #-}
concatPA = closure1 concatPA_v concatPA_l


-- app (append) ---------------------------------------------------------------
appPA_v :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA_v #-}
appPA_v xs ys = appPA# xs ys

appPA_l :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE_PA appPA_l #-}
appPA_l (PArray m# pxss) (PArray n# pyss)
  = PArray (m# +# n#)
  $ case pxss of { PNested xsegd xs ->
    case pyss of { PNested ysegd ys ->
    let
      segd = U.plusSegd xsegd ysegd
    in
    PNested segd (applPD segd xsegd xs ysegd ys) }}

appPA :: PA a => PArray a :-> PArray a :-> PArray a
{-# INLINE appPA #-}
appPA = closure2 appPA_v appPA_l


-- enumFromTo -----------------------------------------------------------------
enumFromToPA_v :: Int -> Int -> PArray Int
{-# INLINE_PA enumFromToPA_v #-}
enumFromToPA_v m n = fromUArrPA (distance m n) (U.enumFromTo m n)

distance :: Int -> Int -> Int
{-# INLINE_STREAM distance #-}
distance m n = max 0 (n - m + 1)

enumFromToPA_l :: PArray Int -> PArray Int -> PArray (PArray Int)
{-# INLINE_PA enumFromToPA_l #-}
enumFromToPA_l (PArray m# ms) (PArray n# ns)
  = PArray m#
  $ PNested segd
  $ toScalarPData
  $ U.enumFromStepLenEach (U.elementsSegd segd)
        (fromScalarPData ms) (U.replicate (U.elementsSegd segd) 1) lens
  where
    lens = U.zipWith distance (fromScalarPData ms) (fromScalarPData ns)
    segd = U.lengthsToSegd lens

enumFromToPA_Int :: Int :-> Int :-> PArray Int
{-# INLINE enumFromToPA_Int #-}
enumFromToPA_Int = closure2 enumFromToPA_v enumFromToPA_l


-- indexed --------------------------------------------------------------------
indexedPA_v :: PA a => PArray a -> PArray (Int,a)
{-# INLINE indexedPA_v #-}
indexedPA_v (PArray n# xs)
  = PArray n# (P_2 (toScalarPData $ U.enumFromStepLen 0 1 (I# n#)) xs)

indexedPA_l :: PA a => PArray (PArray a) -> PArray (PArray (Int,a))
{-# INLINE indexedPA_l #-}
indexedPA_l (PArray n# xss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    PNested segd (P_2 (toScalarPData $ U.indices_s segd) xs) }

indexedPA :: PA a => PArray a :-> PArray (Int,a)
{-# INLINE indexedPA #-}
indexedPA = closure1 indexedPA_v indexedPA_l


-- slice ----------------------------------------------------------------------
slicePA_v :: PA a => Int -> Int -> PArray a -> PArray a
{-# INLINE slicePA_v #-}
slicePA_v (I# from) (I# to) xs 
  = extractPA# xs from (to -# from) 

-- TODO: Can we define this in terms of extractPA?
slicePA_l :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
{-# INLINE slicePA_l #-}
slicePA_l (PArray n# is) (PArray _ lens) (PArray _ xss)
  = PArray n#
  $ case xss of { PNested segd xs ->
    PNested segd'
  $ bpermutePD xs (elementsSegd# segd')
                  (U.zipWith (+) (U.indices_s segd')
                                 (U.replicate_s segd'
                                    (U.zipWith (+) (fromScalarPData is)
                                                   (U.indicesSegd segd)))) }
  where
    segd' = U.lengthsToSegd (fromScalarPData lens)

slicePA :: PA a => Int :-> Int :-> PArray a :-> PArray a
{-# INLINE slicePA #-}
slicePA = closure3 slicePA_v slicePA_l


-- update ---------------------------------------------------------------------
updatePA_v :: PA a => PArray a -> PArray (Int,a) -> PArray a
{-# INLINE_PA updatePA_v #-}
updatePA_v xs (PArray n# (P_2 is ys))
  = updatePA# xs (fromScalarPData is) (PArray n# ys)

updatePA_l
  :: PA a => PArray (PArray a) -> PArray (PArray (Int,a)) -> PArray (PArray a)
{-# INLINE_PA updatePA_l #-}
updatePA_l (PArray m# xss) (PArray n# pss)
  = PArray m#
  $ case xss of { PNested segd  xs ->
    case pss of { PNested segd' (P_2 is ys) ->
    PNested segd
  $ updatePD xs (U.zipWith (+) (fromScalarPData is)
                                (U.replicate_s segd' (U.indicesSegd segd)))
                ys }}

updatePA :: PA a => PArray a :-> PArray (Int,a) :-> PArray a
{-# INLINE updatePA #-}
updatePA = closure2 updatePA_v updatePA_l


-- bpermute -------------------------------------------------------------------
bpermutePA_v :: PA a => PArray a -> PArray Int -> PArray a
{-# INLINE_PA bpermutePA_v #-}
bpermutePA_v xs (PArray n# is) = bpermutePA# xs n# (fromScalarPData is)

bpermutePA_l :: PA a => PArray (PArray a) -> PArray (PArray Int) -> PArray (PArray a)
{-# INLINE_PA bpermutePA_l #-}
bpermutePA_l (PArray _ xss) (PArray n# iss)
  = PArray n#
  $ case xss of { PNested segd  xs ->
    case iss of { PNested isegd is ->
    PNested isegd
  $ bpermutePD xs (elementsSegd# isegd)
                  (U.zipWith (+) (fromScalarPData is)
                                 (U.replicate_s isegd (U.indicesSegd segd))) }}

bpermutePA :: PA a => PArray a :-> PArray Int :-> PArray a
{-# INLINE bpermutePA #-}
bpermutePA = closure2 bpermutePA_v bpermutePA_l

