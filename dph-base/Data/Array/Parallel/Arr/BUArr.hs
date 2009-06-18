{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Arr.BUArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--               (c) [2006..2007] Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (unboxed values and GHC libraries)
--
--
-- This module define our own infrastructure for unboxed arrays, but recycle
-- some of the existing abstractions for boxed arrays.  It's more important to
-- have precise control over the implementation of unboxed arrays, because
-- they are more performance critical.  All arrays defined here are
-- `Int'-indexed without H98 `Ix' support.
--
-- So far, we only support Char, Int, Float, and Double in unboxed arrays
-- (adding more is merely a matter of tedious typing).
--
--
-- TODO
--
-- * For some not understood reason, `checkCritical' prevents the write
--   operations to be inlined.  Instead, a specialised version of them is
--   called.  Interestingly, this doesn't seem to affect runtime negatively
--   (as opposed to still checking, but inlining everything).  Nevertheless,
--   bounds checks cost performance.  (Checking only the writes in SMVM costs
--   about a factor of two for the fully fused version and about 50% for the
--   partially fused version.)
--
--   We could check only check some of the writes (eg, in permutations) as we
--   know for others that they can never be out of bounds (provided this
--   library is correct).
--
-- * There is no proper block copy support yet.  It would be helpful for
--   extracting and copying.  But do we need extracting if we have slicing?
--   (Slicing instead of extracting may introduce space leaks..)
--
-- * If during freezing it becomes clear that the array is much smaller than
--   originally allocated, it might be worthwhile to copy the data into a new,
--   smaller array.

module Data.Array.Parallel.Arr.BUArr (
  -- * Types
  BUArr, MBUArr,

  -- * Elements of unboxed arrays
  UAE,

  -- * Operations on mutable arrays
  lengthMBU, newMBU, readMBU, writeMBU, extractMBU, copyMBU,
  unsafeFreezeMBU, unsafeFreezeAllMBU,

  -- * Operations on immutable arrays
  -- ** Basic operations
  lengthBU, emptyBU, replicateBU, indexBU, sliceBU, extractBU,

  -- ** Streaming
  streamBU, unstreamBU,

  -- ** Higher-order and arithmetic operations
  mapBU, foldlBU, foldBU, scanlBU, scanBU, sumBU,

  -- * Conversions to\/from lists
  toBU, fromBU,

  -- * I\/O
  hPutBU, hGetBU
) where

-- GHC-internal definitions
import GHC.Prim (
  Char#, Int#, Float#, Double#,
  ByteArray#, MutableByteArray#, RealWorld,
  (*#), newByteArray#, unsafeFreezeArray#, unsafeThawArray#, unsafeCoerce#,
  indexWideCharArray#, readWideCharArray#, writeWideCharArray#,
  indexIntArray#, readIntArray#, writeIntArray#,
  indexWord8Array#, readWord8Array#, writeWord8Array#,
  indexFloatArray#, readFloatArray#, writeFloatArray#,
  indexDoubleArray#, readDoubleArray#, writeDoubleArray#) 
import GHC.Base	(
  Char(..), Int(..), (+#), and#, or#, neWord#, int2Word#)
import GHC.Float (
  Float(..), Double(..))
import GHC.Word ( Word8(..) )
import Data.Array.Base (
  wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE)

import System.IO
import Foreign
import Foreign.C   (CSize,CInt)

import GHC.Handle
import GHC.IOBase

-- NDP library
import Data.Array.Parallel.Base
import Data.Array.Parallel.Stream

infixl 9 `indexBU`, `readMBU`

here s = "Arr.BUArr." ++ s

-- | Immutable unboxed arrays
--
data BUArr    e = BUArr  !Int !Int ByteArray#

-- | Mutable unboxed arrays
--
data MBUArr s e = MBUArr !Int      (MutableByteArray# s)

instance HS e => HS (BUArr e)
instance HS e => HS (MBUArr s e)

-- |Number of elements of an immutable unboxed array
--
lengthBU :: BUArr e -> Int
lengthBU (BUArr _ n _) = n

-- |Number of elements of a mutable unboxed array
--
lengthMBU :: MBUArr s e -> Int
lengthMBU (MBUArr n _) = n


-- | Class of elements that can be stored in unboxed arrays
class HS e => UAE e where
  -- | Compute the size of an unboxed array with @n@ elements. The second
  -- argument is just for type inference and will not be inspected.
  --
  sizeBU   :: Int -> e -> Int

  -- | Yield the element at the given position of an immutable array.
  --
  indexBU  :: BUArr e    -> Int      -> e

  -- | Read the element at the given position of a mutable array.
  --
  readMBU  :: MBUArr s e -> Int      -> ST s e

  -- | Write the element at the given position of a mutable array.
  --
  writeMBU :: MBUArr s e -> Int -> e -> ST s ()

-- |Empty array
--
emptyBU :: UAE e => BUArr e
emptyBU = runST (do
            a <- newMBU 0
            unsafeFreezeMBU a 0
          )

-- |Produces an array that consists of a subrange of the original one without
-- copying any elements.
--
sliceBU :: BUArr e -> Int -> Int -> BUArr e
sliceBU (BUArr start len arr) newStart newLen =
  let start' = start + newStart
  in
  BUArr start' ((len - newStart) `min` newLen) arr

-- |Allocate an uninitialised unboxed array
--
newMBU :: forall s e. UAE e => Int -> ST s (MBUArr s e)
{-# INLINE newMBU #-}
newMBU n = ST $ \s1# ->
  case sizeBU n (undefined::e) of {I# len#          ->
  case newByteArray# len# s1#   of {(# s2#, marr# #) ->
  (# s2#, MBUArr n marr# #) }}

-- |Turn a mutable into an immutable array WITHOUT copying its contents, which
-- implies that the mutable array must not be mutated anymore after this
-- operation has been executed.
--
-- * The explicit size parameter supports partially filled arrays (and must be
--   less than or equal the size used when allocating the mutable array)
--
unsafeFreezeMBU :: MBUArr s e -> Int -> ST s (BUArr e)
{-# INLINE unsafeFreezeMBU #-}
unsafeFreezeMBU (MBUArr m mba#) n = 
  checkLen (here "unsafeFreezeMBU") m n $ ST $ \s# ->
  (# s#, BUArr 0 n (unsafeCoerce# mba#) #)

-- |Turn a mutable into an immutable array WITHOUT copying its contents, which
-- implies that the mutable array must not be mutated anymore after this
-- operation has been executed.
--
-- * In contrast to 'unsafeFreezeMBU', this operation always freezes the
-- entire array.
-- 
unsafeFreezeAllMBU :: MBUArr s e -> ST s (BUArr e)
{-# INLINE unsafeFreezeAllMBU #-}
unsafeFreezeAllMBU (MBUArr m mba#) = 
  ST $ \s# -> (# s#, BUArr 0 m (unsafeCoerce# mba#) #)


-- |Instances of unboxed arrays
-- -

-- This is useful to define loops that act as generators cheaply (see the
-- ``Functional Array Fusion'' paper)
--
instance UAE () where
  sizeBU _ _ = 0

  {-# INLINE indexBU #-}
  indexBU (BUArr _ _ _) (I# _) = ()

  {-# INLINE readMBU #-}
  readMBU (MBUArr _ _) (I# _) =
    ST $ \s# -> 
    (# s#, () #)

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr _ _) (I# _) () = 
    ST $ \s# -> 
    (# s#, () #)

instance UAE Bool where
  sizeBU (I# n#) _ = I# n#

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Bool]") n i $
      (indexWord8Array# ba# (s# +# i#) `neWord#` int2Word# 0#)

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Bool]") n i $
    ST $ \s# ->
    case readWord8Array# mba# i# s#   of {(# s2#, r# #) ->
    (# s2#, r# `neWord#` int2Word# 0# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) e# = 
    checkCritical (here "writeMBU[Bool]") n i $
    ST $ \s# ->
    case writeWord8Array# mba# i# b# s# of {s2# ->
    (# s2#, () #)}
    where
      !b# = int2Word# (if e# then 1# else 0#)

{-
instance UAE Bool where
  sizeBU (I# n#) _ = I# (bOOL_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Bool]") n i $
      (indexWordArray# ba# (bOOL_INDEX (s# +# i#)) `and#` bOOL_BIT (s# +# i#))
      `neWord#` int2Word# 0#

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Bool]") n i $
    ST $ \s# ->
    case readWordArray# mba# (bOOL_INDEX i#) s#   of {(# s2#, r# #) ->
    (# s2#, (r# `and#` bOOL_BIT i#) `neWord#` int2Word# 0# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) e# = 
    checkCritical (here "writeMBU[Bool]") n i $
    ST $ \s# ->
    case bOOL_INDEX i#                            of {j#            ->
    case readWordArray# mba# j# s#                of {(# s2#, v# #) ->
    case if e# then v# `or#`  bOOL_BIT     i#
               else v# `and#` bOOL_NOT_BIT i#     of {v'#           ->
    case writeWordArray# mba# j# v'# s2#          of {s3#           ->
    (# s3#, () #)}}}}
-}

instance UAE Char where
  sizeBU (I# n#) _ = I# (cHAR_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Char]") n i $
    case indexWideCharArray# ba# (s# +# i#)	    of {r# ->
    (C# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Char]") n i $
    ST $ \s# ->
    case readWideCharArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, C# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (C# e#) = 
    checkCritical (here "writeMBU[Char]") n i $
    ST $ \s# ->
    case writeWideCharArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Int where
  sizeBU (I# n#) _ = I# (wORD_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Int]") n i $
    case indexIntArray# ba# (s# +# i#) 	       of {r# ->
    (I# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Int]") n i $
    ST $ \s# ->
    case readIntArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, I# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (I# e#) = 
    checkCritical (here "writeMBU[Int]") n i $
    ST $ \s# ->
    case writeIntArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Word8 where
  sizeBU (I# n#) _ = I# n#

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Word8]") n i $
    case indexWord8Array# ba# (s# +# i#) of {r# ->
    (W8# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Word8]") n i $
    ST $ \s# ->
    case readWord8Array# mba# i# s# of {(# s2#, r# #) ->
    (# s2#, W8# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (W8# e#) = 
    checkCritical (here "writeMBU[Word8]") n i $
    ST $ \s# ->
    case writeWord8Array# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Float where
  sizeBU (I# n#) _ = I# (fLOAT_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Float]") n i $
    case indexFloatArray# ba# (s# +# i#)         of {r# ->
    (F# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Float]") n i $
    ST $ \s# ->
    case readFloatArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, F# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (F# e#) = 
    checkCritical (here "writeMBU[Float]") n i $
    ST $ \s# ->
    case writeFloatArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Double where
  sizeBU (I# n#) _ = I# (dOUBLE_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check (here "indexBU[Double]") n i $
    case indexDoubleArray# ba# (s# +# i#)         of {r# ->
    (D# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check (here "readMBU[Double]") n i $
    ST $ \s# ->
    case readDoubleArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, D# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (D# e#) = 
    checkCritical (here "writeMBU[Double]") n i $
    ST $ \s# ->
    case writeDoubleArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

-- |Stream of unboxed arrays
-- -------------------------

-- | Generate a stream from an array, from left to right
--
streamBU :: UAE e => BUArr e -> Stream e
{-# INLINE [1] streamBU #-}
streamBU arr = Stream next 0 (lengthBU arr)
  where
    n = lengthBU arr
    --
    next i | i == n    = Done
           | otherwise = Yield (arr `indexBU` i) (i+1)

-- | Construct an array from a stream, filling it from left to right
--
unstreamBU :: UAE e => Stream e -> BUArr e
{-# INLINE [1] unstreamBU #-}
unstreamBU (Stream next s n) =
  runST (do
    marr <- newMBU n
    n'   <- fill0 marr
    unsafeFreezeMBU marr n'
  )
  where
    fill0 marr = fill s 0
      where
        fill s i = i `seq`
                   case next s of
                     Done       -> return i
                     Skip s'    -> fill s' i
                     Yield x s' -> do
                                     writeMBU marr i x
                                     fill s' (i+1)

-- Fusion rules for unboxed arrays

{-# RULES  -- -} (for font-locking)

"streamBU/unstreamBU" forall s.
  streamBU (unstreamBU s) = s

 #-}


-- |Combinators for unboxed arrays
-- -

-- |Replicate combinator for unboxed arrays
--
replicateBU :: UAE e => Int -> e -> BUArr e
{-# INLINE replicateBU #-}
replicateBU n = unstreamBU . replicateS n


-- |Extract a slice from an array (given by its start index and length)
--
extractBU :: UAE e => BUArr e -> Int -> Int -> BUArr e
{-# INLINE extractBU #-}
extractBU arr i n = 
  runST (do
    ma <- newMBU n
    copy0 ma
    unsafeFreezeMBU ma n
  )
  where
    fence = n `min` (lengthBU arr - i)
    copy0 ma = copy 0
      where
        copy off | off == fence = return ()
		 | otherwise	= do
				    writeMBU ma off (arr `indexBU` (i + off))
				    copy (off + 1)
-- NB: If we had a bounded version of loopBU, we could express extractBU in
--     terms of that loop combinator.  The problem is that this makes fusion
--     more awkward; in particular, when the second loopBU in a
--     "loopBU/loopBU" situation has restricted bounds.  On the other hand
--     sometimes fusing the extraction of a slice with the following
--     computation on that slice is very useful.
-- FIXME: If we leave it as it, we should at least use a block copy operation.
--	  (What we really want is to represent extractBU as a loop when we can
--	  fuse it with a following loop on the computed slice and, otherwise,
--	  when there is no opportunity for fusion, we want to use a block copy
--	  routine.)
-- FIXME: The above comments no longer apply as we've switched to stream-based
--        fusion. Moreover, slicing gives us bounded iteration for free.

-- |Map a function over an unboxed array
--
mapBU :: (UAE a, UAE b) => (a -> b) -> BUArr a -> BUArr b
{-# INLINE mapBU #-}
mapBU f = unstreamBU . mapS f . streamBU

-- |Reduce an unboxed array
--
foldlBU :: UAE b => (a -> b -> a) -> a -> BUArr b -> a
{-# INLINE foldlBU #-}
foldlBU f z = foldS f z . streamBU

-- |Reduce an unboxed array using an *associative* combining operator
--
foldBU :: UAE a => (a -> a -> a) -> a -> BUArr a -> a
{-# INLINE foldBU #-}
foldBU = foldlBU

-- |Summation of an unboxed array
--
sumBU :: (UAE a, Num a) => BUArr a -> a
{-# INLINE sumBU #-}
sumBU = foldBU (+) 0

-- |Prefix reduction of an unboxed array
--
scanlBU :: (UAE a, UAE b) => (a -> b -> a) -> a -> BUArr b -> BUArr a
{-# INLINE scanBU #-}
scanlBU f z = unstreamBU . scanS f z . streamBU

-- |Prefix reduction of an unboxed array using an *associative* combining
-- operator
--
scanBU :: UAE a => (a -> a -> a) -> a -> BUArr a -> BUArr a
scanBU = scanlBU

-- |Extract a slice from a mutable array (the slice is immutable)
--
extractMBU :: UAE e => MBUArr s e -> Int -> Int -> ST s (BUArr e)
{-# INLINE extractMBU #-}
extractMBU arr i n = do
		       arr' <- unsafeFreezeMBU arr (i + n)
		       return $ extractBU arr' i n

-- |Copy a the contents of an immutable array into a mutable array from the
-- specified position on
--
copyMBU :: UAE e => MBUArr s e -> Int -> BUArr e -> ST s ()
{-# INLINE copyMBU #-}
copyMBU marr i arr = ins i 0
  where
    n = lengthBU arr
    --
    ins i j | j == n    = return ()
	    | otherwise = do
			    writeMBU marr i (arr `indexBU` j)
			    ins (i + 1) (j + 1)

-- Eq instance
--
instance (Eq e, UAE e) => Eq (BUArr e) where
  arr == brr = n == lengthBU brr && eq 0
    where
      n = lengthBU arr
      eq i | i == n    = True
           | otherwise = (arr `indexBU` i) == (brr `indexBU` i)
                         && eq (i+1)

-- Show instance
--
instance (Show e, UAE e) => Show (BUArr e) where
  showsPrec _ a =   showString "toBU " 
		  . showList [a `indexBU` i | i <- [0..lengthBU a - 1]]

-- Auxilliary functions
-- --------------------

-- |Convert a list to an array
--
toBU :: UAE e => [e] -> BUArr e
toBU = unstreamBU . toStream

-- |Convert an array to a list
--
fromBU :: UAE e => BUArr e -> [e]
fromBU a = map (a `indexBU`) [0 .. lengthBU a - 1]

-- That's missing from Data.Array.Base
--
cHAR_SCALE :: Int# -> Int#
cHAR_SCALE n# = 4# *# n#


-- IO
-- --

hGetBU :: forall e. UAE e => Handle -> IO (BUArr e)
hGetBU h =
  alloca $ \iptr ->
  do
    hGetBuf h iptr (sizeOf (undefined :: Int))
    n <- peek iptr
    let bytes = sizeBU n (undefined :: e)
    -- ToDo: we're doing an extra copy here.  If we allocated the array
    -- pinned, then we could read directly into the array rather than
    -- copying it.
    allocaBytes bytes $ \ptr -> do
      r <- hGetBuf h ptr bytes
      marr@(MBUArr _ marr#) <- stToIO (newMBU n)
      memcpy_ba marr# ptr (fromIntegral r)
      stToIO (unsafeFreezeAllMBU marr)

hPutBU :: forall e. UAE e => Handle -> BUArr e -> IO ()
hPutBU h arr@(BUArr i n arr#) =
  alloca $ \iptr ->
  do
    poke iptr n
    hPutBuf h iptr (sizeOf n)
    -- ToDo: we're doing an extra copy here.  If we allocated the array
    -- pinned, then we could read directly into the array rather than
    -- copying it.
    allocaBytes size $ \ptr -> do
      memcpy_src_off ptr arr# (fromIntegral off) (fromIntegral size)
      hPutBuf h ptr size
      return ()
  where
    off  = sizeBU i (undefined :: e)
    size = sizeBU n (undefined :: e)

-- ToDo: dodgy dependency on base-package C wrapper.  This should be moved
-- into the dph package.
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_src_off :: Ptr a -> ByteArray# -> CInt -> CSize -> IO (Ptr ())

foreign import ccall unsafe "memcpy"
   memcpy_ba :: MutableByteArray# RealWorld -> Ptr a -> CSize -> IO (Ptr ())

