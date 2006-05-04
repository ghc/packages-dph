-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Monadic.SUArr
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

module Data.Array.Parallel.Monadic.SUArr (

  -- * Array types and classes containing the admissble elements types
  USegd(..), MUSegd(..), SUArr(..), MSUArr(..),

  -- * Basic operations on segmented parallel arrays
  lengthSU, indexSU, sliceIndexSU, extractIndexSU, sliceSU, extractSU,
  flattenSU, (>:), newMSU, nextMSU, unsafeFreezeMSU, toUSegd, fromUSegd

) where

-- standard libraries
import Monad (liftM)

-- friends
import Data.Array.Parallel.Base ((:*:)(..), ST)
import Data.Array.Parallel.Arr.BUArr (
  BUArr, MBUArr, lengthBU, newMBU, indexBU, sliceBU, readMBU, writeMBU,
  unsafeFreezeMBU, extractBU, mapBU, scanBU)
import Data.Array.Parallel.Monadic.UArr (
  UA(..), UArr(..), MUArr, extractU)
import Data.Array.Parallel.Arr.Prim (
  Prim(..))


infixl 9 `indexSU`
infixr 9 >:


-- |Segmented arrays
-- -----------------

-- |Segment descriptors are used to represent the structure of nested arrays.
--
data USegd = USegd {
	       segdUS :: !(BUArr Int),  -- segment lengths
	       psumUS :: !(BUArr Int)   -- prefix sum of former
	     }

-- |Mutable segment descriptor
--
data MUSegd s = MUSegd {
	          segdMUS :: !(MBUArr s Int),  -- segment lengths
	          psumMUS :: !(MBUArr s Int)   -- prefix sum of former
	        }

-- |Segmented arrays (only one level of segmentation)
-- 
data SUArr e = SUArr {
                 segdSU :: !USegd,             -- segment descriptor
                 dataSU :: !(UArr e)           -- flat data array
               }

-- |Mutable segmented arrays (only one level of segmentation)
--
data MSUArr e s = MSUArr {
                    segdMSU :: !(MUSegd s),    -- segment descriptor
                    dataMSU :: !(MUArr e s)    -- flat data array
                  }

-- |Operations on segmented arrays
-- -------------------------------

-- |Yield the number of segments.
-- 
lengthSU :: UA e => SUArr e -> Int
lengthSU (SUArr segd _) = lengthBU (segdUS segd)

-- |Extract the segment at the given index using the given extraction function
-- (either 'sliceU' or 'extractU').
-- 
indexSU :: UA e => (UArr e -> Int -> Int -> UArr e) -> SUArr e -> Int -> UArr e
indexSU copy (SUArr segd a) i = copy a (psumUS segd `indexBU` i)
                                       (segdUS segd `indexBU` i)

-- |Extract the segment at the given index without copying the elements.
--
sliceIndexSU :: UA e => SUArr e -> Int -> UArr e
sliceIndexSU = indexSU sliceU

-- |Extract the segment at the given index (elements are copied).
-- 
extractIndexSU :: UA e => SUArr e -> Int -> UArr e
extractIndexSU = indexSU extractU

-- |Extract a subrange of the segmented array without copying the elements.
--
sliceSU :: UA e => SUArr e -> Int -> Int -> SUArr e
sliceSU (SUArr segd a) i n =
  let
    segd1 = segdUS segd
    psum  = psumUS segd
    m     = if i == 0 then 0 else psum `indexBU` (i - 1)
    psum' = mapBU (subtract m) (sliceBU psum i n)
    segd' = USegd (sliceBU segd1 i n) psum'
    i'    = psum `indexBU` i
  in
  SUArr segd' (sliceU a i' (psum `indexBU` (i + n - 1) - i' + 1))

-- |Extract a subrange of the segmented array (elements are copied).
--
extractSU :: UA e => SUArr e -> Int -> Int -> SUArr e
extractSU (SUArr segd a) i n =
  let
    segd1 = segdUS segd
    psum  = psumUS segd
    m     = if i == 0 then 0 else psum `indexBU` (i - 1)
    psum' = mapBU (subtract m) (extractBU psum i n)
    segd' = USegd (extractBU segd1 i n) psum'
    i'    = psum `indexBU` i
  in
  SUArr segd' (extractU a i' (psum `indexBU` (i + n - 1) - i' + 1))

-- |The functions `newMSU', `nextMSU', and `unsafeFreezeMSU' are to 
-- iteratively define a segmented mutable array; i.e., arrays of type `MSUArr
-- s e`.

-- |Allocate a segmented parallel array (providing the number of segments and
-- number of base elements).
--
newMSU :: UA e => Int -> Int -> ST s (MSUArr e s)
{-# INLINE newMSU #-}
newMSU nsegd n = do
		   segd <- newMBU nsegd
		   psum <- newMBU nsegd
		   a    <- newMU n
		   return $ MSUArr (MUSegd segd psum) a

-- |Iterator support for filling a segmented mutable array left-to-right.
--
-- * If no element is given (ie, third argument is `Nothing'), a segment is
--   initialised.  Segment initialisation relies on previous segments already
--   being completed.
--
-- * Every segment must be initialised before it is filled left-to-right
--
nextMSU :: UA e => MSUArr e s -> Int -> Maybe e -> ST s ()
{-# INLINE nextMSU #-}
nextMSU (MSUArr (MUSegd segd psum) a) i Nothing =
  do                                                -- segment initialisation
    i' <- if i == 0 then return 0 
		    else do
		      off <- psum `readMBU` (i - 1)
		      n   <- segd `readMBU` (i - 1)
		      return $ off + n
    writeMBU psum i i'
    writeMBU segd i 0
nextMSU (MSUArr (MUSegd segd psum) a) i (Just e) = 
  do
    i' <- psum `readMBU` i
    n' <- segd `readMBU` i
    writeMU a (i' + n') e
    writeMBU segd i (n' + 1)

-- |Convert a mutable segmented array into an immutable one.
--
unsafeFreezeMSU :: UA e => MSUArr e s -> Int -> ST s (SUArr e)
{-# INLINE unsafeFreezeMSU #-}
unsafeFreezeMSU (MSUArr segd a) n = 
  do
    segd' <- unsafeFreezeMBU (segdMUS segd) n
    psum' <- unsafeFreezeMBU (psumMUS segd) n
    let n' = if n == 0 then 0 else psum' `indexBU` (n - 1) + 
				   segd' `indexBU` (n - 1)
    a' <- unsafeFreezeMU a n'
    return $ SUArr (USegd segd' psum') a'


-- |Basic operations on segmented arrays
-- -------------------------------------

-- |Compose a nested array.
--
(>:) :: UA a => USegd -> UArr a -> SUArr a
{-# INLINE [1] (>:) #-}  -- see `UAFusion'
(>:) = SUArr

-- |Decompose a nested array.
--
flattenSU :: UA a => SUArr a -> (USegd :*: UArr a)
flattenSU (SUArr segd a) = (segd :*: a)

-- |Convert a length array into a segment descriptor.
--
toUSegd :: UArr Int -> USegd 
toUSegd (UAPrim (PrimInt lens)) = USegd lens (scanBU (+) 0 lens)

-- |Extract the length array from a segment descriptor.
--
fromUSegd :: USegd -> UArr Int
fromUSegd (USegd lens _) = UAPrim (PrimInt lens)
