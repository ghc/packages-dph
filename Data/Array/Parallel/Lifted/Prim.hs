module Data.Array.Parallel.Lifted.Prim (
  PArray_Int#(..),
  lengthPA_Int#, emptyPA_Int#, replicatePA_Int#, indexPA_Int#,
  upToPA_Int#, selectPA_Int#,

  PArray_Bool#,
  lengthPA_Bool#,
  truesPA_Bool#
) where

import Data.Array.Parallel.Unlifted

import GHC.Exts (Int#, Int(..))

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

indexPA_Int# :: PArray_Int# -> Int# -> Int#
indexPA_Int# (PInt# ns) i# = case ns !: I# i# of { I# n# -> n# }

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = PInt# (enumFromToU 0 ((I# n#) -1))
{-# INLINE upToPA_Int# #-}

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# (PInt# ns) i# = PBool# (mapU (\n -> n == I# i#) ns)
{-# INLINE selectPA_Int# #-}

newtype PArray_Bool# = PBool# (UArr Bool)

lengthPA_Bool# :: PArray_Bool# -> Int#
lengthPA_Bool# (PBool# arr) = case lengthU arr of { I# n# -> n# }
{-# INLINE lengthPA_Bool# #-}

truesPA_Bool# :: PArray_Bool# -> Int#
truesPA_Bool# (PBool# arr)
  = case sumU (mapU (\b -> if b then 1 else 0) arr) of
      I# n# -> n#
{-# INLINE truesPA_Bool# #-}

