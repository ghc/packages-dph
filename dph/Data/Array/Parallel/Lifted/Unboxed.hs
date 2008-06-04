{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Unboxed (
  Segd, toSegd,

  PArray_Int#,
  lengthPA_Int#, emptyPA_Int#,
  replicatePA_Int#, replicatelPA_Int#, repeatPA_Int#,
  indexPA_Int#, bpermutePA_Int#, appPA_Int#, applPA_Int#,
  packPA_Int#, pack'PA_Int#, combine2PA_Int#, combine2'PA_Int#,
  upToPA_Int#, enumFromToPA_Int#, enumFromToEachPA_Int#,
  selectPA_Int#, selectorToIndices2PA#,
  sumPA_Int#, sumPAs_Int#,
  unsafe_mapPA_Int#, unsafe_zipWithPA_Int#, unsafe_foldPA_Int#,
  unsafe_scanPA_Int#,

  PArray_Double#,
  lengthPA_Double#, emptyPA_Double#,
  replicatePA_Double#, replicatelPA_Double#, repeatPA_Double#,
  indexPA_Double#, bpermutePA_Double#, appPA_Double#, applPA_Double#,
  packPA_Double#, pack'PA_Double#, combine2PA_Double#, combine2'PA_Double#,
  unsafe_zipWithPA_Double#, unsafe_foldPA_Double#, unsafe_fold1PA_Double#,
  unsafe_foldPAs_Double#,

  PArray_Bool#,
  lengthPA_Bool#, replicatelPA_Bool#,
  packPA_Bool#, truesPA_Bool#, truesPAs_Bool#,

  fromBoolPA#, toBoolPA#
) where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Base ((:*:)(..), fromBool, toBool)

import GHC.Exts ( Int#, Int(..),
                  Double#, Double(..) )


import Debug.Trace

type Segd = USegd

toSegd :: PArray_Int# -> PArray_Int# -> Segd
toSegd is js = toUSegd (zipU is js)

type PArray_Int# = UArr Int

lengthPA_Int# :: PArray_Int# -> Int#
lengthPA_Int# arr = case lengthU arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Int# #-}

emptyPA_Int# :: PArray_Int#
emptyPA_Int# = emptyU
{-# INLINE_PA emptyPA_Int# #-}

replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = replicateU (I# n#) (I# i#)
{-# INLINE_PA replicatePA_Int# #-}

replicatelPA_Int# :: Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
replicatelPA_Int# n# ns is = replicateEachU (I# n#) ns is
{-# INLINE_PA replicatelPA_Int# #-}

repeatPA_Int# :: Int# -> PArray_Int# -> PArray_Int#
repeatPA_Int# n# is = repeatU (I# n#) is
{-# INLINE_PA repeatPA_Int# #-}

indexPA_Int# :: PArray_Int# -> Int# -> Int#
indexPA_Int# ns i# = case ns !: I# i# of { I# n# -> n# }
{-# INLINE_PA indexPA_Int# #-}

bpermutePA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
bpermutePA_Int# ns is = bpermuteU ns is
{-# INLINE_PA bpermutePA_Int# #-}

appPA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
appPA_Int# ms ns = ms +:+ ns
{-# INLINE_PA appPA_Int# #-}

applPA_Int# :: Segd -> PArray_Int# -> Segd -> PArray_Int# -> PArray_Int#
applPA_Int# is xs js ys
  = concatSU $ (is >: xs) ^+:+^ (js >: ys)
{-# INLINE_PA applPA_Int# #-}

pack'PA_Int# :: PArray_Int# -> PArray_Bool# -> PArray_Int#
pack'PA_Int# ns bs = packU ns bs
{-# INLINE_PA pack'PA_Int# #-}

packPA_Int# :: PArray_Int# -> Int# -> PArray_Bool# -> PArray_Int#
packPA_Int# ns _ bs = pack'PA_Int# ns bs
{-# INLINE_PA packPA_Int# #-}

combine2'PA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2'PA_Int# sel xs ys = combineU (mapU (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Int# #-}

combine2PA_Int# :: Int# -> PArray_Int# -> PArray_Int#
                -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2PA_Int# _ sel _ xs ys = combine2'PA_Int# sel xs ys
{-# INLINE_PA combine2PA_Int# #-}

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = enumFromToU 0 (I# n# - 1)
{-# INLINE_PA upToPA_Int# #-}

enumFromToPA_Int# :: Int# -> Int# -> PArray_Int#
enumFromToPA_Int# m# n# = enumFromToU (I# m#) (I# n#)
{-# INLINE_PA enumFromToPA_Int# #-}

enumFromToEachPA_Int# :: Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
enumFromToEachPA_Int# n# is js = enumFromToEachU (I# n#) (zipU is js)
{-# INLINE_PA enumFromToEachPA_Int# #-}

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# ns i# = mapU (\n -> n == I# i#) ns
{-# INLINE_PA selectPA_Int# #-}

selectorToIndices2PA# :: PArray_Int# -> PArray_Int#
selectorToIndices2PA# sel
  = zipWithU pick sel
  . scanU index (0 :*: 0)
  $ mapU init sel
  where
    init 0 = 1 :*: 0
    init _ = 0 :*: 1

    index (i1 :*: j1) (i2 :*: j2) = (i1+i2 :*: j1+j2)

    pick 0 (i :*: j) = i
    pick _ (i :*: j) = j
{-# INLINE_PA selectorToIndices2PA# #-}

sumPA_Int# :: PArray_Int# -> Int#
sumPA_Int# ns = case sumU ns of I# n# -> n#
{-# INLINE_PA sumPA_Int# #-}

sumPAs_Int# :: Segd -> PArray_Int# -> PArray_Int#
sumPAs_Int# segd ds
  = sumSU (segd >: ds)
{-# INLINE_PA sumPAs_Int# #-}

unsafe_mapPA_Int# :: (Int -> Int) -> PArray_Int# -> PArray_Int#
unsafe_mapPA_Int# f ns = mapU f ns
{-# INLINE_PA unsafe_mapPA_Int# #-}

unsafe_zipWithPA_Int# :: (Int -> Int -> Int)
                      -> PArray_Int# -> PArray_Int# -> PArray_Int#
unsafe_zipWithPA_Int# f ms ns = zipWithU f ms ns
{-# INLINE_PA unsafe_zipWithPA_Int# #-}

unsafe_foldPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> Int
unsafe_foldPA_Int# f z ns = foldU f z ns
{-# INLINE_PA unsafe_foldPA_Int# #-}

unsafe_scanPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> PArray_Int#
unsafe_scanPA_Int# f z ns = scanU f z ns
{-# INLINE_PA unsafe_scanPA_Int# #-}

type PArray_Double# = UArr Double

lengthPA_Double# :: PArray_Double# -> Int#
lengthPA_Double# arr = case lengthU arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Double# #-}

emptyPA_Double# :: PArray_Double#
emptyPA_Double# = emptyU
{-# INLINE_PA emptyPA_Double# #-}

replicatePA_Double# :: Int# -> Double# -> PArray_Double#
replicatePA_Double# n# d# = replicateU (I# n#) (D# d#)
{-# INLINE_PA replicatePA_Double# #-}

replicatelPA_Double# :: Int# -> PArray_Int# -> PArray_Double# -> PArray_Double#
replicatelPA_Double# n# ns ds = replicateEachU (I# n#) ns ds
{-# INLINE_PA replicatelPA_Double# #-}

repeatPA_Double# :: Int# -> PArray_Double# -> PArray_Double#
repeatPA_Double# n# ds = repeatU (I# n#) ds
{-# INLINE_PA repeatPA_Double# #-}

indexPA_Double# :: PArray_Double# -> Int# -> Double#
indexPA_Double# ds i# = case ds !: I# i# of { D# d# -> d# }
{-# INLINE_PA indexPA_Double# #-}

bpermutePA_Double# :: PArray_Double# -> PArray_Int# -> PArray_Double#
bpermutePA_Double# ds is = bpermuteU ds is
{-# INLINE_PA bpermutePA_Double# #-}

appPA_Double# :: PArray_Double# -> PArray_Double# -> PArray_Double#
appPA_Double# ms ns = ms +:+ ns
{-# INLINE_PA appPA_Double# #-}

applPA_Double# :: Segd -> PArray_Double# -> Segd -> PArray_Double#
               -> PArray_Double#
applPA_Double# is xs js ys = concatSU $ (is >: xs) ^+:+^ (js >: ys)
{-# INLINE_PA applPA_Double# #-}

pack'PA_Double# :: PArray_Double# -> PArray_Bool# -> PArray_Double#
pack'PA_Double# ns bs = packU ns bs
{-# INLINE_PA pack'PA_Double# #-}

packPA_Double# :: PArray_Double# -> Int# -> PArray_Bool# -> PArray_Double#
packPA_Double# ns _ bs = pack'PA_Double# ns bs
{-# INLINE_PA packPA_Double# #-}

combine2'PA_Double# :: PArray_Int#
                    -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2'PA_Double# sel xs ys = combineU (mapU (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Double# #-}

combine2PA_Double# :: Int# -> PArray_Int# -> PArray_Int#
                   -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2PA_Double# _ sel _ xs ys = combine2'PA_Double# sel xs ys
{-# INLINE_PA combine2PA_Double# #-}

unsafe_zipWithPA_Double# :: (Double -> Double -> Double)
                         -> PArray_Double# -> PArray_Double# -> PArray_Double#
unsafe_zipWithPA_Double# f ms ns = zipWithU f ms ns
{-# INLINE_PA unsafe_zipWithPA_Double# #-}

unsafe_foldPA_Double# :: (Double -> Double -> Double)
                    -> Double -> PArray_Double# -> Double
unsafe_foldPA_Double# f z ns = foldU f z ns
{-# INLINE_PA unsafe_foldPA_Double# #-}

unsafe_fold1PA_Double#
  :: (Double -> Double -> Double) -> PArray_Double# -> Double
unsafe_fold1PA_Double# f ns = fold1U f ns
{-# INLINE_PA unsafe_fold1PA_Double# #-}

unsafe_foldPAs_Double# :: (Double -> Double -> Double) -> Double
                       -> Segd -> PArray_Double# -> PArray_Double#
unsafe_foldPAs_Double# f z segd ds = foldSU f z (segd >: ds)
{-# INLINE_PA unsafe_foldPAs_Double# #-}
               
type PArray_Bool# = UArr Bool

lengthPA_Bool# :: PArray_Bool# -> Int#
lengthPA_Bool# arr = case lengthU arr of { I# n# -> n# }
{-# INLINE_PA lengthPA_Bool# #-}

replicatelPA_Bool# :: Int# -> PArray_Int# -> PArray_Bool# -> PArray_Bool#
replicatelPA_Bool# n# ns ds = replicateEachU (I# n#) ns ds
{-# INLINE_PA replicatelPA_Bool# #-}

packPA_Bool# :: PArray_Bool# -> Int# -> PArray_Bool# -> PArray_Bool#
packPA_Bool# ns _ bs = packU ns bs
{-# INLINE_PA packPA_Bool# #-}

truesPA_Bool# :: PArray_Bool# -> Int#
truesPA_Bool# bs = sumPA_Int# (fromBoolPA# bs)
{-# INLINE_PA truesPA_Bool# #-}

truesPAs_Bool# :: Segd -> PArray_Bool# -> PArray_Int#
truesPAs_Bool# segd = sumPAs_Int# segd . fromBoolPA#
{-# INLINE truesPAs_Bool# #-}

fromBoolPA# :: PArray_Bool# -> PArray_Int#
fromBoolPA# = mapU fromBool
{-# INLINE_PA fromBoolPA# #-}

toBoolPA# :: PArray_Int# -> PArray_Bool#
toBoolPA# = mapU toBool
{-# INLINE_PA toBoolPA# #-}

