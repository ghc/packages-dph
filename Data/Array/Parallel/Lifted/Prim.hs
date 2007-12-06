module Data.Array.Parallel.Lifted.Prim (
  PArray_Int#(..),
  lengthPA_Int#, emptyPA_Int#,
  replicatePA_Int#, replicatelPA_Int#, repeatPA_Int#,
  indexPA_Int#, bpermutePA_Int#, upToPA_Int#, selectPA_Int#, sumPA_Int#,
  unsafe_zipWithPA_Int#, unsafe_foldPA_Int#, unsafe_scanPA_Int#,
  sumPAs_Double#,

  PArray_Double#(..),
  lengthPA_Double#, emptyPA_Double#,
  replicatePA_Double#, replicatelPA_Double#, repeatPA_Double#,
  indexPA_Double#, bpermutePA_Double#,
  unsafe_zipWithPA_Double#, unsafe_foldPA_Double#,

  PArray_Bool#(..),
  lengthPA_Bool#,
  truesPA_Bool#
) where

import Data.Array.Parallel.Unlifted

import GHC.Exts ( Int#, Int(..),
                  Double#, Double(..) )

newtype PArray_Int# = PInt# (UArr Int)

lengthPA_Int# :: PArray_Int# -> Int#
lengthPA_Int# (PInt# arr) = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Int# #-}

emptyPA_Int# :: PArray_Int#
emptyPA_Int# = PInt# emptyU
{-# INLINE emptyPA_Int# #-}

replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = PInt# (replicateU (I# n#) (I# i#))
{-# INLINE replicatePA_Int# #-}

replicatelPA_Int# :: Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
replicatelPA_Int# n# (PInt# ns) (PInt# is)
  = PInt# (concatSU (replicateSU ns is))
{-# INLINE replicatelPA_Int# #-}

repeatPA_Int# :: Int# -> PArray_Int# -> PArray_Int#
repeatPA_Int# n# (PInt# is) = PInt# (repeatU (I# n#) is)
{-# INLINE repeatPA_Int# #-}

indexPA_Int# :: PArray_Int# -> Int# -> Int#
indexPA_Int# (PInt# ns) i# = case ns !: I# i# of { I# n# -> n# }
{-# INLINE indexPA_Int# #-}

bpermutePA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int#
bpermutePA_Int# (PInt# ns) (PInt# is) = PInt# (bpermuteU ns is)
{-# INLINE bpermutePA_Int# #-}

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = PInt# (enumFromToU 0 ((I# n#) -1))
{-# INLINE upToPA_Int# #-}

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# (PInt# ns) i# = PBool# (mapU (\n -> n == I# i#) ns)
{-# INLINE selectPA_Int# #-}

sumPA_Int# :: PArray_Int# -> Int#
sumPA_Int# (PInt# ns) = case sumU ns of I# n# -> n#
{-# INLINE sumPA_Int# #-}

unsafe_zipWithPA_Int# :: (Int -> Int -> Int)
                      -> PArray_Int# -> PArray_Int# -> PArray_Int#
unsafe_zipWithPA_Int# f (PInt# ms) (PInt# ns) = PInt# (zipWithU f ms ns)
{-# INLINE unsafe_zipWithPA_Int# #-}

unsafe_foldPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> Int
unsafe_foldPA_Int# f z (PInt# ns) = foldU f z ns
{-# INLINE unsafe_foldPA_Int# #-}

unsafe_scanPA_Int# :: (Int -> Int -> Int) -> Int -> PArray_Int# -> PArray_Int#
unsafe_scanPA_Int# f z (PInt# ns) = PInt# (scanU f z ns)
{-# INLINE unsafe_scanPA_Int# #-}

newtype PArray_Double# = PDouble# (UArr Double)

lengthPA_Double# :: PArray_Double# -> Int#
lengthPA_Double# (PDouble# arr) = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Double# #-}

emptyPA_Double# :: PArray_Double#
emptyPA_Double# = PDouble# emptyU
{-# INLINE emptyPA_Double# #-}

replicatePA_Double# :: Int# -> Double# -> PArray_Double#
replicatePA_Double# n# d# = PDouble# (replicateU (I# n#) (D# d#))
{-# INLINE replicatePA_Double# #-}

replicatelPA_Double# :: Int# -> PArray_Int# -> PArray_Double# -> PArray_Double#
replicatelPA_Double# n# (PInt# ns) (PDouble# ds)
  = PDouble# (concatSU (replicateSU ns ds))
{-# INLINE replicatelPA_Double# #-}

repeatPA_Double# :: Int# -> PArray_Double# -> PArray_Double#
repeatPA_Double# n# (PDouble# ds) = PDouble# (repeatU (I# n#) ds)
{-# INLINE repeatPA_Double# #-}

indexPA_Double# :: PArray_Double# -> Int# -> Double#
indexPA_Double# (PDouble# ds) i# = case ds !: I# i# of { D# d# -> d# }
{-# INLINE indexPA_Double# #-}

bpermutePA_Double# :: PArray_Double# -> PArray_Int# -> PArray_Double#
bpermutePA_Double# (PDouble# ds) (PInt# is) = PDouble# (bpermuteU ds is)
{-# INLINE bpermutePA_Double# #-}

unsafe_zipWithPA_Double# :: (Double -> Double -> Double)
                         -> PArray_Double# -> PArray_Double# -> PArray_Double#
unsafe_zipWithPA_Double# f (PDouble# ms) (PDouble# ns)
  = PDouble# (zipWithU f ms ns)
{-# INLINE unsafe_zipWithPA_Double# #-}

unsafe_foldPA_Double# :: (Double -> Double -> Double)
                    -> Double -> PArray_Double# -> Double
unsafe_foldPA_Double# f z (PDouble# ns) = foldU f z ns
{-# INLINE unsafe_foldPA_Double# #-}

sumPAs_Double# :: PArray_Int# -> PArray_Int# -> PArray_Double#
               -> PArray_Double#
sumPAs_Double# (PInt# lens) (PInt# idxs) (PDouble# ds)
  = PDouble# (sumSU (toUSegd (zipU lens idxs) >: ds))

newtype PArray_Bool# = PBool# (UArr Bool)

lengthPA_Bool# :: PArray_Bool# -> Int#
lengthPA_Bool# (PBool# arr) = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Bool# #-}

truesPA_Bool# :: PArray_Bool# -> Int#
truesPA_Bool# (PBool# arr)
  = case sumU (mapU (\b -> if b then 1 else 0) arr) of
      I# n# -> n#
{-# INLINE truesPA_Bool# #-}

