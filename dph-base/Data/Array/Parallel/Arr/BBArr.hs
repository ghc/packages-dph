-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.BBArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--               (c) [2006..2007] Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable
--
--
-- This module defines two-phase boxed arrays which are strict in all
-- elements which have been initialised. It is ok not to initialise some
-- elements as long as we don't access them. Initialising an element with
-- bottom diverges.
--
-- This means that in
--
-- > let arr = runST (newMBB n >>= unsafeFreezeMBB)
--
-- @arr@ itself is defined but @arr `indexBB` i@ diverges for all @i@. In
--
-- > let brr = runST (do mb <- newMBB n
-- >                     <initialise all elements except at index 2>
-- >                     unsafeFreezeMBB mb)
--
-- @brr `indexBB` i@ diverges for @i=2@ and converges otherwise.
--
-- Thus, our arrays effectively model strict collections of (index,value)
-- pairs but do not require the indices to be contiguous.
--
-- Internally, we do use bottoms for uninitialised elements in the underlying
-- GHC array; however, @writeMBB@ is strict in the value being written.
-- Indexing into the array can never yield a thunk if the elements are
-- hyperstrict but can diverge (which is ok). Hence, we can say that the
-- arrays are hyperstrict for hyperstrict elements even though the underlying
-- representation is not.
--
-- The reason for this slightly peculiar (but sound, in my opinion) model are
-- distributed 'MaybeS's.
--

module Data.Array.Parallel.Arr.BBArr (
  -- * Types
  BBArr, MBBArr,

  -- * Operations on immutable arrays
  lengthBB, indexBB, extractBB,

  -- * Operations on mutable arrays
  newMBB, lengthMBB, readMBB, writeMBB,
  unsafeFreezeMBB, unsafeFreezeAllMBB,
  extractMBB, copyMBB
) where

-- standard library
import Control.Monad (liftM)

-- GHC-internal definitions
import GHC.Prim           (unsafeFreezeArray#)
import GHC.Arr		  (Array(..), STArray(..), bounds, boundsSTArray,
			   (!), newSTArray, readSTArray, writeSTArray)

-- NDP library
import Data.Array.Parallel.Base (
  HS, ST(..), runST, checkLen)

infixl 9 `indexBB`, `readMBB`


-- Boxed arrays
-- ------------

-- | Immutable boxed arrays
--
newtype BBArr    e = BBArr  (Array     Int e)

-- |Mutable boxed arrays
--
newtype MBBArr s e = MBBArr (STArray s Int e)

instance HS e => HS (BBArr e)

-- |Length of an immutable boxed array
--
lengthBB :: BBArr e -> Int
lengthBB (BBArr arr) = (+ 1) . snd . bounds $ arr

-- |Length of a mutable boxed array
--
lengthMBB :: MBBArr s e -> Int
lengthMBB (MBBArr marr) = (+ 1) . snd . boundsSTArray $ marr

-- |Allocate a boxed array
--
newMBB :: Int -> ST s (MBBArr s e)
newMBB n = liftM MBBArr (newSTArray (0, n - 1) bottomBBArrElem)

-- |Access an element in an immutable, boxed array
--
indexBB :: BBArr e -> Int -> e
indexBB (BBArr arr) = (arr!)

-- |Access an element in an mutable, boxed array
--
readMBB :: MBBArr s e -> Int -> ST s e
readMBB (MBBArr marr) = readSTArray marr

-- |Update an element in an mutable, boxed array
--
writeMBB :: MBBArr s e -> Int -> e -> ST s ()
writeMBB (MBBArr marr) e = e `seq` writeSTArray marr e

-- |Turn a mutable into an immutable array WITHOUT copying its contents, which
-- implies that the mutable array must not be mutated anymore after this
-- operation has been executed.
--
-- * The explicit size parameter supports partially filled arrays (and must be
--   less than or equal the size used when allocating the mutable array)
--
unsafeFreezeMBB :: MBBArr s e -> Int -> ST s (BBArr e)
{-# INLINE unsafeFreezeMBB #-}
unsafeFreezeMBB (MBBArr (STArray _ m1 _ marr#)) n = 
  checkLen "PAPrim.unsafeFreezeMB: " (m1 + 1) n $ ST $ \s1# ->
  case unsafeFreezeArray# marr# s1# of {(# s2#, arr# #) ->
  (# s2#, BBArr (Array 0 (n - 1) n arr#) #)}

-- |Turn a mutable into an immutable array WITHOUT copying its contents, which
-- implies that the mutable array must not be mutated anymore after this
-- operation has been executed.
--
-- * In contrast to 'unsafeFreezeMBB', this operation always freezes the
-- entire array.
--
unsafeFreezeAllMBB :: MBBArr s e -> ST s (BBArr e)
unsafeFreezeAllMBB marr = unsafeFreezeMBB marr (lengthMBB marr)

-- |Extract a slice from an array (given by its start index and length)
--
extractBB :: BBArr e -> Int -> Int -> BBArr e
{-# INLINE extractBB #-}
extractBB arr i n = 
  runST (do
    ma@(MBBArr sta) <- newMBB n
    copy0 sta
    unsafeFreezeMBB ma n
  )
  where
    fence = n `min` (lengthBB arr - i)
    copy0 sta = copy 0
      where
        copy off | off == fence = return ()
		 | otherwise	= do
                                    -- Do not use writeMBB here because it is
                                    -- strict but we want to be able to copy
                                    -- arrays with uninitialised elements.
                                    -- This is ok because we know that
                                    -- initialised elements in the original
                                    -- array have already been evaluated by a
                                    -- writeMBB.
                                    writeSTArray sta off (arr `indexBB` (i + off))
				    copy (off + 1)

bottomBBArrElem = 
  error "PAPrim: Touched an uninitialised element in a `BBArr'"

-- |Extract a slice from a mutable array (the slice is immutable)
--
extractMBB :: MBBArr s e -> Int -> Int -> ST s (BBArr e)
extractMBB arr i n = 
  do
    arr' <- unsafeFreezeMBB arr (i + n)
    return $ extractBB arr' i n

-- |Copy a the contents of an immutable array into a mutable array from the
-- specified position on
--
copyMBB :: MBBArr s e -> Int -> BBArr e -> ST s ()
copyMBB marr i arr = ins i 0
  where
    n = lengthBB arr
    --
    ins i j | j == n    = return ()
	    | otherwise = do
			    writeMBB marr i (arr `indexBB` j)
			    ins (i + 1) (j + 1)

-- Show instance
--
instance Show e => Show (BBArr e) where
  showsPrec _ a =   showString "toBB " 
		  . showList [a `indexBB` i | i <- [0..lengthBB a - 1]]


