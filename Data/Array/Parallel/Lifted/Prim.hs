module Data.Array.Parallel.Lifted.Prim (
  PArray_Int#(..),
  lengthPA_Int#, emptyPA_Int#, replicatePA_Int#, replicatelPA_Int#,
  indexPA_Int#, upToPA_Int#, selectPA_Int#,

  PArray_Double#(..),
  lengthPA_Double#, emptyPA_Double#, replicatePA_Double#, replicatelPA_Double#,
  indexPA_Double#,

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

indexPA_Int# :: PArray_Int# -> Int# -> Int#
indexPA_Int# (PInt# ns) i# = case ns !: I# i# of { I# n# -> n# }
{-# INLINE indexPA_Int# #-}

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = PInt# (enumFromToU 0 ((I# n#) -1))
{-# INLINE upToPA_Int# #-}

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# (PInt# ns) i# = PBool# (mapU (\n -> n == I# i#) ns)
{-# INLINE selectPA_Int# #-}

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

indexPA_Double# :: PArray_Double# -> Int# -> Double#
indexPA_Double# (PDouble# ds) i# = case ds !: I# i# of { D# d# -> d# }
{-# INLINE indexPA_Double# #-}

newtype PArray_Bool# = PBool# (UArr Bool)

lengthPA_Bool# :: PArray_Bool# -> Int#
lengthPA_Bool# (PBool# arr) = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Bool# #-}

truesPA_Bool# :: PArray_Bool# -> Int#
truesPA_Bool# (PBool# arr)
  = case sumU (mapU (\b -> if b then 1 else 0) arr) of
      I# n# -> n#
{-# INLINE truesPA_Bool# #-}

