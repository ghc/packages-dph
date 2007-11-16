module Data.Array.Parallel.Lifted.Prim (
  PArray_Int#,
  lengthPA_Int#, emptyPA_Int#, replicatePA_Int#,
  upToPA_Int#, selectPA_Int#,

  PArray_Bool#
) where

import Data.Array.Parallel.Unlifted

import GHC.Exts (Int#, Int(..))

newtype PArray_Int# = PInt# (UArr Int)

lengthPA_Int# :: PArray_Int# -> Int#
lengthPA_Int# (PInt# arr) = case lengthU arr of { I# n# -> n# }

emptyPA_Int# :: PArray_Int#
emptyPA_Int# = PInt# emptyU

replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = PInt# (replicateU (I# n#) (I# i#))

upToPA_Int# :: Int# -> PArray_Int#
upToPA_Int# n# = PInt# (enumFromToU 0 ((I# n#) -1))

selectPA_Int# :: PArray_Int# -> Int# -> PArray_Bool#
selectPA_Int# (PInt# ns) i# = PBool# (mapU (\n -> n == I# i#) ns)

newtype PArray_Bool# = PBool# (UArr Bool)

