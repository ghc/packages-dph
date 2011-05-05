{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

-- |Underlying data types and type classes for parallel arrays.
--
--	For motivational material see:
--	   "An Approach to Fast Arrays in Haskell", Chakravarty and Keller, 2003
--
--	For discussion of how the mapping to generic types works see:
--         "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
--
module Data.Array.Parallel.Lifted.PArray (
  PArray(..), PData,

  PA(..),
  lengthPA#, dataPA#, replicatePA#, replicatelPA#, repeatPA#,
  emptyPA, indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packByTagPA#, combine2PA#, updatePA#, fromListPA#, fromListPA, nfPA,

  replicatePD, replicatelPD, repeatPD, emptyPD,
  indexPD, extractPD, bpermutePD, appPD, applPD,
  packByTagPD, combine2PD, updatePD, fromListPD, nfPD,

  PRepr, PR(..),

  Scalar(..),
  replicatePRScalar, replicatelPRScalar, repeatPRScalar, emptyPRScalar,
  indexPRScalar, extractPRScalar, bpermutePRScalar, appPRScalar, applPRScalar,
  packByTagPRScalar, combine2PRScalar, updatePRScalar, fromListPRScalar,
  nfPRScalar,
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )
import Data.Array.Parallel.Base           ( Tag, intToTag, traceF )
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import Data.Array.Parallel.PArray.PData
import GHC.Exts (Int#, Int(..), (+#), (*#))
import SpecConstr


-- | Lifted\/bulk parallel arrays
--   This contains the array length, along with the element data.
--
{-# ANN type PArray NoSpecConstr #-}
data PArray a = PArray Int# (PData a)




-- Dictionaries -----------------------------------------------------------------------------------

-- PA Wrappers ------------------------------------------------------------------------------------
--
-- These functions operate on whole PArrays.
--   For most of these we can just take the parallel array data (PData) from the 
--   array structure then apply the corresponding PD wrapper. Depending on
--   the function we may also need to calculuate the length of the resulting array.
--

-- Simple projections
-- | Take the length field of a PArray.
lengthPA# :: PArray a -> Int#
{-# INLINE_PA lengthPA# #-}
lengthPA# (PArray n# _) = n#

-- | Take the data field of a PArray.
dataPA# :: PArray a -> PData a
{-# INLINE_PA dataPA# #-}
dataPA# (PArray _ d) = d

-- | An array with no elements.
emptyPA :: PA a => PArray a
{-# INLINE_PA emptyPA #-}
emptyPA
  = PArray 0# emptyPD

replicatePA# :: PA a => Int# -> a -> PArray a
{-# INLINE_PA replicatePA# #-}
replicatePA# n# x
  = PArray n# (replicatePD n# x)

replicatelPA# :: PA a => U.Segd -> PArray a -> PArray a
{-# INLINE_PA replicatelPA# #-}
replicatelPA# segd (PArray n# xs)
  = PArray (elementsSegd# segd) (replicatelPD segd xs)

repeatPA# :: PA a => Int# -> PArray a -> PArray a
{-# INLINE_PA repeatPA# #-}
repeatPA# m# (PArray n# xs) 
  = PArray (m# *# n#) (repeatPD m# n# xs)

indexPA# :: PA a => PArray a -> Int# -> a
{-# INLINE_PA indexPA# #-}
indexPA# (PArray _ xs) i# 
  = indexPD xs i#

extractPA# :: PA a => PArray a -> Int# -> Int# -> PArray a
{-# INLINE_PA extractPA# #-}
extractPA# (PArray _ xs) i# n#
  = PArray n# (extractPD xs i# n#)

bpermutePA# :: PA a => PArray a -> Int# -> U.Array Int -> PArray a
{-# INLINE bpermutePA# #-}
bpermutePA# (PArray _ xs) n# is
  = PArray n# (bpermutePD xs n# is)

appPA# :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA# #-}
appPA# (PArray m# xs) (PArray n# ys)
  = PArray (m# +# n#) (appPD xs ys)

applPA# :: PA a => U.Segd -> U.Segd -> PArray a -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA applPA# #-}
applPA# segd is (PArray m# xs) js (PArray n# ys)
  = PArray (m# +# n#) (applPD segd is xs js ys)

packByTagPA# :: PA a => PArray a -> Int# -> U.Array Tag -> Int# -> PArray a
{-# INLINE_PA packByTagPA# #-}
packByTagPA# (PArray _ xs) n# tags t# 
  = PArray n# (packByTagPD xs n# tags t#)

combine2PA# :: PA a => Int# -> U.Sel2 -> PArray a -> PArray a -> PArray a
{-# INLINE_PA combine2PA# #-}
combine2PA# n# sel (PArray _ as) (PArray _ bs)
  = PArray n# (combine2PD n# sel as bs)

updatePA# :: PA a => PArray a -> U.Array Int -> PArray a -> PArray a
{-# INLINE_PA updatePA# #-}
updatePA# (PArray n# xs) is (PArray _ ys)
  = PArray n# (updatePD xs is ys)

fromListPA# :: PA a => Int# -> [a] -> PArray a
{-# INLINE_PA fromListPA# #-}
fromListPA# n# xs 
  = PArray n# (fromListPD n# xs)

fromListPA :: PA a => [a] -> PArray a
{-# INLINE fromListPA #-}
fromListPA xs
  = case length xs of
     I# n# -> fromListPA# n# xs

nfPA :: PA a => PArray a -> ()
{-# INLINE nfPA #-}
nfPA (PArray _ xs) 
  = nfPD xs



