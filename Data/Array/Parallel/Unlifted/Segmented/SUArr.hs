-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.SUArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- This module defines segmented arrays in terms of unsegmented ones.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.SUArr (

  -- * Array types and classes containing the admissble elements types
  USegd, MUSegd, SUArr, MSUArr,

  -- * Basic operations on segment descriptors
  lengthUSegd, lengthsUSegd, indicesUSegd, toUSegd,
  sliceUSegd, extractUSegd,
  newMUSegd, unsafeFreezeMUSegd,

  -- * Basic operations on segmented parallel arrays
  lengthSU, lengthsSU, indicesSU, segdSU,
  flattenSU, (>:),
  newMSU, unsafeFreezeMSU,
) where

-- friends
import Data.Array.Parallel.Base (
  (:*:)(..), ST)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr, MUArr,
  lengthU, (!:), scanlU, fstU, sndU, zipU, streamU, unstreamU,
  sliceU, extractU, mapU,
  newMU, unsafeFreezeMU)

import Monad (
  liftM2)

infixr 9 >:


-- |Segmented arrays
-- -----------------

-- |Segment descriptors are used to represent the structure of nested arrays.
--
type USegd  = UArr  (Int :*: Int)
type MUSegd = MUArr (Int :*: Int)

-- |Segmented arrays (only one level of segmentation)
-- 

-- NOTE: We do *not* make this strict in the arrays. This allows GHC to
--       eliminate construction/deconstruction of segmented arrays.

-- TODO: Is this ok?
data SUArr  e   = SUArr  USegd      (UArr  e)
data MSUArr e s = MSUArr (MUSegd s) (MUArr e s)

-- |Operations on segment descriptors
-- ----------------------------------

-- |Allocate a mutable segment descriptor for the given number of segments
--
newMUSegd :: Int -> ST s (MUSegd s)
{-# INLINE newMUSegd #-}
newMUSegd = newMU

-- |Convert a mutable segment descriptor to an immutable one
--
unsafeFreezeMUSegd :: MUSegd s -> Int -> ST s USegd
{-# INLINE unsafeFreezeMUSegd #-}
unsafeFreezeMUSegd = unsafeFreezeMU

-- |Yield the overall number of segments
--
lengthUSegd :: USegd -> Int
{-# INLINE lengthUSegd #-}
lengthUSegd = lengthU

-- |Yield the segment lengths of a segment descriptor
--
lengthsUSegd :: USegd -> UArr Int
{-# INLINE lengthsUSegd #-}
lengthsUSegd = fstU

-- |Yield the segment indices of a segment descriptor
--
indicesUSegd :: USegd -> UArr Int
{-# INLINE indicesUSegd #-}
indicesUSegd = sndU

-- |Convert a length array into a segment descriptor.
--
toUSegd :: UArr Int -> USegd
{-# INLINE [1] toUSegd #-}
toUSegd lens = zipU lens (scanlU (+) 0 lens)

-- |Convert a length array to a segment descriptor - fusible version.
--
toUSegd' :: UArr Int -> USegd
{-# INLINE toUSegd' #-}
toUSegd' = scanlU (\(_ :*: i) n -> n :*: (i+n)) (0 :*: 0)

{-# RULES

"toUSegd/unstreamU" forall s.
  toUSegd (unstreamU s) = toUSegd' (unstreamU s)
"streamU/toUSegd" forall a.
  streamU (toUSegd a) = streamU (toUSegd' a)
 #-}

sliceUSegd :: USegd -> Int -> Int -> USegd
sliceUSegd segd i n = zipU lens idxs'
  where
    lens  = sliceU (lengthsUSegd segd) i n
    idxs  = sliceU (indicesUSegd segd) i n
    idxs' = mapU (subtract k) idxs
    k     = idxs !: 0

extractUSegd :: USegd -> Int -> Int -> USegd
extractUSegd segd i n = zipU lens idxs'
  where
    lens  = extractU (lengthsUSegd segd) i n
    idxs  = sliceU   (indicesUSegd segd) i n
    idxs' = mapU (subtract k) idxs
    k     = idxs !: 0

-- |Operations on segmented arrays
-- -------------------------------

-- |Yield the segment descriptor
--
segdSU :: UA e => SUArr e -> USegd
{-# INLINE segdSU #-}
segdSU (SUArr segd _) = segd

-- |Yield the flat data array
--
flattenSU :: UA e => SUArr e -> UArr e
{-# INLINE flattenSU #-}
flattenSU (SUArr _ a) = a

-- |Yield the number of segments.
-- 
lengthSU :: UA e => SUArr e -> Int
{-# INLINE lengthSU #-}
lengthSU = lengthU . segdSU

-- |Yield the lengths of the segments.
--
lengthsSU :: UA e => SUArr e -> UArr Int
{-# INLINE lengthsSU #-}
lengthsSU = lengthsUSegd . segdSU

-- |Yield the starting indices of the segments.
--
indicesSU :: UA e => SUArr e -> UArr Int
{-# INLINE indicesSU #-}
indicesSU = indicesUSegd . segdSU

-- |Compose a nested array.
--
(>:) :: UA a => USegd -> UArr a -> SUArr a
{-# INLINE (>:) #-}
(>:) = SUArr

-- |Operations on mutable segmented arrays
-- ---------------------------------------

-- |Allocate a segmented parallel array (providing the number of segments and
-- number of base elements).
--
newMSU :: UA e => Int -> Int -> ST s (MSUArr e s)
{-# INLINE newMSU #-}
newMSU nsegd n = liftM2 MSUArr (newMUSegd nsegd) (newMU n)

-- |Convert a mutable segmented array into an immutable one.
--
unsafeFreezeMSU :: UA e => MSUArr e s -> Int -> ST s (SUArr e)
{-# INLINE unsafeFreezeMSU #-}
unsafeFreezeMSU (MSUArr msegd ma) n = 
  do
    segd <- unsafeFreezeMUSegd msegd n
    let n' = if n == 0 then 0 else indicesUSegd segd !: (n - 1) + 
				   lengthsUSegd segd !: (n - 1)
    a <- unsafeFreezeMU ma n'
    return $ SUArr segd a


