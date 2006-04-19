-- |GHC-specific low-level support for boxed arrays
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--  Copyright (c) 2006	       Manuel M T Chakravarty
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 + GHC-internal libraries
--
--- Todo ----------------------------------------------------------------------
--
--  * See some of the comments in UArr.hs

module Data.Array.Parallel.Base.BBArr (
  -- * Boxed primitive arrays (both immutable and mutable)
  BBArr, MBBArr,

  -- * Operations on primitive boxed arrays
  lengthBB, lengthMBB, newMBB, indexBB, readMBB, writeMBB,
  unsafeFreezeMBB, unsafeFreezeAllMBB,
  sliceBB, sliceMBB, insertMBB,

  -- * Re-exporting some of GHC's internals that higher-level modules need
  ST, runST
) where

-- standard library
import Monad (liftM)

-- GHC-internal definitions
import GHC.Prim           (unsafeFreezeArray#)
import GHC.ST		  (ST(..), runST)
import GHC.Arr		  (Array(..), STArray(..), bounds, boundsSTArray,
			   (!), newSTArray, readSTArray, writeSTArray)

-- config
import Data.Array.Parallel.Base.Debug (checkLen)

infixl 9 `indexBB`, `readMBB`


-- |Boxed arrays
-- -------------

-- |Boxed arrays in both an immutable and a mutable variant
--
-- NB: We use a newtype instead of a type, as we need to be able to partially
--     apply the new type constructors (which type synonyms only support in a
--     restricted way).
--
newtype BBArr    e = BBArr  (Array     Int e)
newtype MBBArr s e = MBBArr (STArray s Int e)

-- |Length of an immutable boxed array
--
lengthBB :: BBArr e -> Int
lengthBB (BBArr arr) = (+ 1) . snd . bounds $ arr

-- |Length of an immutable boxed array
--
lengthMBB :: MBBArr s e -> Int
lengthMBB (MBBArr marr) = (+ 1) . snd . boundsSTArray $ marr

-- |Allocate a boxed array initialised with the given value
--
newMBB :: Int -> e -> ST s (MBBArr s e)
newMBB n = liftM MBBArr . newSTArray (0, n - 1)

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
writeMBB (MBBArr marr) = writeSTArray marr

-- |Turn a mutable into an immutable array WITHOUT copying its contents, which
-- implies that the mutable array must not be mutated anymore after this
-- operation has been executed.
--
-- * The explicit size parameter supports partially filled arrays (and must be
--   less than or equal the size used when allocating the mutable array)
--
unsafeFreezeMBB :: MBBArr s e -> Int -> ST s (BBArr e)
{-# INLINE unsafeFreezeMBB #-}
unsafeFreezeMBB (MBBArr (STArray _ m1 marr#)) n = 
  checkLen "PAPrim.unsafeFreezeMB: " (m1 + 1) n $ ST $ \s1# ->
  case unsafeFreezeArray# marr# s1# of {(# s2#, arr# #) ->
  (# s2#, BBArr (Array 0 (n - 1) arr#) #)}

--- |Turn a mutable into an immutable array WITHOUT copying its contents, which
-- implies that the mutable array must not be mutated anymore after this
-- operation has been executed.
--
-- * In contrast to 'unsafeFreezeMBB', this operation always freezes the
-- entire array.
--
unsafeFreezeAllMBB :: MBBArr s e -> ST s (BBArr e)
unsafeFreezeAllMBB marr = unsafeFreezeMBB marr (lengthMBB marr)


-- |Loop-based combinators on boxed arrays
-- -

-- |Extract a slice from an array (given by its start index and length)
--
sliceBB :: BBArr e -> Int -> Int -> BBArr e
{-# INLINE sliceBB #-}
sliceBB arr i n = 
  runST (do
    ma <- newMBB n bottomBBArrElem
    copy0 ma
    unsafeFreezeMBB ma n
  )
  where
    fence = n `min` (lengthBB arr - i)
    copy0 ma = copy 0
      where
        copy off | off == fence = return ()
		 | otherwise	= do
				    writeMBB ma off (arr `indexBB` (i + off))
				    copy (off + 1)

bottomBBArrElem = 
  error "PAPrim: Touched an uninitialised element in a `BBArr'"

-- |Extract a slice from a mutable array (the slice is immutable)
--
sliceMBB :: MBBArr s e -> Int -> Int -> ST s (BBArr e)
sliceMBB arr i n = 
  do
    arr' <- unsafeFreezeMBB arr (i + n)
    return $ sliceBB arr' i n

-- |Insert a the contents of an immutable array into a mutable array from the
-- specified position on
--
insertMBB :: MBBArr s e -> Int -> BBArr e -> ST s ()
insertMBB marr i arr = ins i 0
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


