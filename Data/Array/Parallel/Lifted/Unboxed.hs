module Data.Array.Parallel.Lifted.Unboxed (
  PArray_Int#,
  lengthPA_Int#, emptyPA_Int#,
  replicatePA_Int#, replicatelPA_Int#, repeatPA_Int#,
  indexPA_Int#, bpermutePA_Int#, appPA_Int#, applPA_Int#,
  packPA_Int#, pack'PA_Int#,
  upToPA_Int#, selectPA_Int#, sumPA_Int#,
  unsafe_zipWithPA_Int#, unsafe_foldPA_Int#, unsafe_scanPA_Int#,
  sumPAs_Int#, 

  PArray_Double#,
  lengthPA_Double#, emptyPA_Double#,
  replicatePA_Double#, replicatelPA_Double#, repeatPA_Double#,
  indexPA_Double#, bpermutePA_Double#, appPA_Double#, applPA_Double#,
  packPA_Double#, pack'PA_Double#,
  unsafe_zipWithPA_Double#, unsafe_foldPA_Double#, unsafe_fold1PA_Double#,
  unsafe_foldPAs_Double#,

  PArray_Bool#(..),
  lengthPA_Bool#, replicatelPA_Bool#,
  truesPA_Bool#
) where

import Data.Array.Parallel.Unlifted

import GHC.Exts ( Int#, Int(..),
                  Double#, Double(..) )

type PArray_Int# = UArr Int

lengthPA_Int# :: PArray_Int# -> Int#
lengthPA_Int# arr = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Int# #-}

emptyPA_Int# :: PArray_Int#
emptyPA_Int# = emptyU
{-# INLINE emptyPA_Int# #-}

replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = replicateU (I# n#) (I# i#)
{-# INLINE replicatePA_Int# #-}

replicatelPA_Int# :: Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
replicatelPA_Int# n# ns is = concatSU (replicateSU ns is)
{-# INLINE replicatelPA_Int# #-}

repeatPA_Int# :: Int# -> PArray_Int# -> PArray_Int#
repeatPA_Int# n# is = repeatU (I# n#) is
{-# INLINE repeatPA_Int# #-}

indexPA_Int# :: PArray_Int# -> Int# -> Int#
indexPA_Int# ns i# = case ns !: I# i# of { I# n# -> n# }
{-# INLINE indexPA_Int# #-}

bpermutePA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
bpermutePA_Int# ns is = bpermuteU ns is
{-# INLINE bpermutePA_Int# #-}

appPA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
appPA_Int# ms ns = ms +:+ ns
{-# INLINE appPA_Int# #-}

applPA_Int# :: USegd -> PArray_Int# -> USegd -> PArray_Int# -> PArray_Int#
applPA_Int# is xs js ys
  = concatSU $ (is >: xs) ^+:+^ (js >: ys)
{-# INLINE applPA_Int# #-}

pack'PA_Int# :: PArray_Int# -> PArray_Bool# -> PArray_Int#
pack'PA_Int# ns bs = packU ns bs
{-# INLINE pack'PA_Int# #-}

packPA_Int# :: PArray_Int# -> Int# -> PArray_Bool# -> PArray_Int#
packPA_Int# ns _ bs = pack'PA_Int# ns bs
{-# INLINE packPA_Int# #-}

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = enumFromToU 0 (I# n# - 1)
{-# INLINE upToPA_Int# #-}

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# ns i# = mapU (\n -> n == I# i#) ns
{-# INLINE selectPA_Int# #-}

sumPA_Int# :: PArray_Int# -> Int#
sumPA_Int# ns = case sumU ns of I# n# -> n#
{-# INLINE sumPA_Int# #-}

sumPAs_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
sumPAs_Int# lens idxs ds
  = sumSU (toUSegd (zipU lens idxs) >: ds)
{-# INLINE sumPAs_Int# #-}

unsafe_zipWithPA_Int# :: (Int -> Int -> Int)
                      -> PArray_Int# -> PArray_Int# -> PArray_Int#
unsafe_zipWithPA_Int# f ms ns = zipWithU f ms ns
{-# INLINE unsafe_zipWithPA_Int# #-}

unsafe_foldPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> Int
unsafe_foldPA_Int# f z ns = foldU f z ns
{-# INLINE unsafe_foldPA_Int# #-}

unsafe_scanPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> PArray_Int#
unsafe_scanPA_Int# f z ns = scanU f z ns
{-# INLINE unsafe_scanPA_Int# #-}

type PArray_Double# = UArr Double

lengthPA_Double# :: PArray_Double# -> Int#
lengthPA_Double# arr = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Double# #-}

emptyPA_Double# :: PArray_Double#
emptyPA_Double# = emptyU
{-# INLINE emptyPA_Double# #-}

replicatePA_Double# :: Int# -> Double# -> PArray_Double#
replicatePA_Double# n# d# = replicateU (I# n#) (D# d#)
{-# INLINE replicatePA_Double# #-}

replicatelPA_Double# :: Int# -> PArray_Int# -> PArray_Double# -> PArray_Double#
replicatelPA_Double# n# ns ds = concatSU (replicateSU ns ds)
{-# INLINE replicatelPA_Double# #-}

repeatPA_Double# :: Int# -> PArray_Double# -> PArray_Double#
repeatPA_Double# n# ds = repeatU (I# n#) ds
{-# INLINE repeatPA_Double# #-}

indexPA_Double# :: PArray_Double# -> Int# -> Double#
indexPA_Double# ds i# = case ds !: I# i# of { D# d# -> d# }
{-# INLINE indexPA_Double# #-}

bpermutePA_Double# :: PArray_Double# -> PArray_Int# -> PArray_Double#
bpermutePA_Double# ds is = bpermuteU ds is
{-# INLINE bpermutePA_Double# #-}

appPA_Double# :: PArray_Double# -> PArray_Double# -> PArray_Double#
appPA_Double# ms ns = ms +:+ ns
{-# INLINE appPA_Double# #-}

applPA_Double# :: USegd -> PArray_Double# -> USegd -> PArray_Double#
               -> PArray_Double#
applPA_Double# is xs js ys = concatSU $ (is >: xs) ^+:+^ (js >: ys)
{-# INLINE applPA_Double# #-}

pack'PA_Double# :: PArray_Double# -> PArray_Bool# -> PArray_Double#
pack'PA_Double# ns bs = packU ns bs
{-# INLINE pack'PA_Double# #-}

packPA_Double# :: PArray_Double# -> Int# -> PArray_Bool# -> PArray_Double#
packPA_Double# ns _ bs = pack'PA_Double# ns bs
{-# INLINE packPA_Double# #-}

unsafe_zipWithPA_Double# :: (Double -> Double -> Double)
                         -> PArray_Double# -> PArray_Double# -> PArray_Double#
unsafe_zipWithPA_Double# f ms ns = zipWithU f ms ns
{-# INLINE unsafe_zipWithPA_Double# #-}

unsafe_foldPA_Double# :: (Double -> Double -> Double)
                    -> Double -> PArray_Double# -> Double
unsafe_foldPA_Double# f z ns = foldU f z ns
{-# INLINE unsafe_foldPA_Double# #-}

unsafe_fold1PA_Double#
  :: (Double -> Double -> Double) -> PArray_Double# -> Double
unsafe_fold1PA_Double# f ns = fold1U f ns
{-# INLINE unsafe_fold1PA_Double# #-}

unsafe_foldPAs_Double# :: (Double -> Double -> Double) -> Double
                       -> PArray_Int# -> PArray_Int# -> PArray_Double#
                       -> PArray_Double#
unsafe_foldPAs_Double# f z lens idxs ds
  = foldSU f z (toUSegd (zipU lens idxs) >: ds)
{-# INLINE unsafe_foldPAs_Double# #-}
               
type PArray_Bool# = UArr Bool

lengthPA_Bool# :: PArray_Bool# -> Int#
lengthPA_Bool# arr = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Bool# #-}

replicatelPA_Bool# :: Int# -> PArray_Int# -> PArray_Bool# -> PArray_Bool#
replicatelPA_Bool# n# ns ds = concatSU (replicateSU ns ds)
{-# INLINE replicatelPA_Bool# #-}

truesPA_Bool# :: PArray_Bool# -> Int#
truesPA_Bool# arr
  = case sumU (mapU (\b -> if b then 1 else 0) arr) of I# n# -> n#
{-# INLINE truesPA_Bool# #-}

