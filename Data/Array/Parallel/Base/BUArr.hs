-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Base.BUArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (unboxed values and GHC libraries)
--
-- Description ---------------------------------------------------------------
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
-- Todo ----------------------------------------------------------------------
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
--   sliceing and inserting.  But do we need slicing if we have clipping?
--   (Clipping instead of slicing may introduce space leaks..)
--
-- * If during freezing it becomes clear that the array is much smaller than
--   originally allocated, it might be worthwhile to copy the data into a new,
--   smaller array.


module Data.Array.Parallel.Base.BUArr (
  -- * Unboxed primitive arrays (both immutable and mutable)
  BUArr, MBUArr,

  -- * Class with operations on primitive unboxed arrays
  UAE, lengthBU, lengthMBU, newMBU, indexBU, clipBU, readMBU, writeMBU,
  unsafeFreezeMBU, unsafeFreezeAllMBU,
  replicateBU, loopBU, loopArr, loopAcc, loopSndAcc, sliceBU,
  mapBU, foldlBU, foldBU, sumBU, scanlBU, scanBU, sliceMBU, insertMBU,

  -- * Re-exporting some of GHC's internals that higher-level modules need
  Char#, Int#, Float#, Double#, Char(..), Int(..), Float(..), Double(..), ST,
  runST
) where

-- standard library
import Monad (liftM)

-- GHC-internal definitions
import GHC.Prim        (Char#, Int#, Float#, Double#, ByteArray#,
		        MutableByteArray#, (*#), newByteArray#,
		        unsafeFreezeArray#, unsafeCoerce#,
		        indexWideCharArray#, readWideCharArray#,
		        writeWideCharArray#, indexIntArray#, readIntArray#, 
		        writeIntArray#, indexWordArray#, readWordArray#, 
		        writeWordArray#, indexFloatArray#, readFloatArray#, 
		        writeFloatArray#, indexDoubleArray#,
		        readDoubleArray#, writeDoubleArray#) 
import GHC.Base	       (Char(..), Int(..), (+#), and#, or#, neWord#, int2Word#)
import GHC.Float       (Float(..), Double(..))
import GHC.ST	       (ST(..), runST)
import Data.Array.Base (bOOL_SCALE, wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE,
			bOOL_INDEX, bOOL_BIT, bOOL_NOT_BIT)

-- NDP library
import Data.Array.Parallel.Base.Hyperstrict

-- config
import Data.Array.Parallel.Base.Debug (check, checkLen, checkCritical)


infixl 9 `indexBU`, `readMBU`

-- |Unboxed arrays
-- ---------------

-- Unboxed arrays of primitive element types arrays constructed from an
-- explicit length and a byte array in both an immutable and a mutable variant
--
data BUArr    e = BUArr  !Int !Int ByteArray#
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

-- |The basic operations on unboxed arrays are overloaded
--
class HS e => UAE e where
  sizeBU   :: Int -> e -> Int		-- size of an array with n elements
  indexBU  :: BUArr e    -> Int      -> e
  readMBU  :: MBUArr s e -> Int      -> ST s e
  writeMBU :: MBUArr s e -> Int -> e -> ST s ()

-- |Produces an array that consists of a subrange of the original one without
-- copying any elements.
--
clipBU :: BUArr e -> Int -> Int -> BUArr e
clipBU (BUArr start len arr) newStart newLen = 
  let start' = start + newStart
  in
  BUArr start' (len - newStart `min` newLen) arr

-- |Allocate an uninitialised unboxed array
--
newMBU :: UAE e => Int -> ST s (MBUArr s e)
{-# INLINE newMBU #-}
newMBU n :: ST s (MBUArr s e) = ST $ \s1# ->
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
  checkLen "PAPrim.unsafeFreezeMU: " m n $ ST $ \s# ->
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
  sizeBU (I# n#) _ = I# (bOOL_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check "PAPrim.indexBU[Bool]" n i $
      (indexWordArray# ba# (bOOL_INDEX (s# +# i#)) `and#` bOOL_BIT i#) 
      `neWord#` int2Word# 0#

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check "PAPrim.readMBU[Bool]" n i $
    ST $ \s# ->
    case readWordArray# mba# (bOOL_INDEX i#) s#   of {(# s2#, r# #) ->
    (# s2#, (r# `and#` bOOL_BIT i#) `neWord#` int2Word# 0# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) e# = 
    checkCritical "PAPrim.writeMBU[Bool]" n i $
    ST $ \s# ->
    case bOOL_INDEX i#                            of {j#            ->
    case readWordArray# mba# j# s#                of {(# s2#, v# #) ->
    case if e# then v# `or#`  bOOL_BIT     i#
               else v# `and#` bOOL_NOT_BIT i#     of {v'#           ->
    case writeWordArray# mba# j# v'# s2#          of {s3#           ->
    (# s3#, () #)}}}}

instance UAE Char where
  sizeBU (I# n#) _ = I# (cHAR_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check "PAPrim.indexBU[Char]" n i $
    case indexWideCharArray# ba# (s# +# i#)	    of {r# ->
    (C# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check "PAPrim.readMBU[Char]" n i $
    ST $ \s# ->
    case readWideCharArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, C# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (C# e#) = 
    checkCritical "PAPrim.writeMBU[Char]" n i $
    ST $ \s# ->
    case writeWideCharArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Int where
  sizeBU (I# n#) _ = I# (wORD_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check "PAPrim.indexBU[Int]" n i $
    case indexIntArray# ba# (s# +# i#) 	       of {r# ->
    (I# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check "PAPrim.readMBU[Int]" n i $
    ST $ \s# ->
    case readIntArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, I# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (I# e#) = 
    checkCritical "PAPrim.writeMBU[Int]" n i $
    ST $ \s# ->
    case writeIntArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Float where
  sizeBU (I# n#) _ = I# (fLOAT_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check "PAPrim.indexBU[Float]" n i $
    case indexFloatArray# ba# (s# +# i#)         of {r# ->
    (F# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check "PAPrim.readMBU[Float]" n i $
    ST $ \s# ->
    case readFloatArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, F# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (F# e#) = 
    checkCritical "PAPrim.writeMBU[Float]" n i $
    ST $ \s# ->
    case writeFloatArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}

instance UAE Double where
  sizeBU (I# n#) _ = I# (dOUBLE_SCALE n#)

  {-# INLINE indexBU #-}
  indexBU (BUArr (I# s#) n ba#) i@(I# i#) =
    check "PAPrim.indexBU[Double]" n i $
    case indexDoubleArray# ba# (s# +# i#)         of {r# ->
    (D# r#)}

  {-# INLINE readMBU #-}
  readMBU (MBUArr n mba#) i@(I# i#) =
    check "PAPrim.readMBU[Double]" n i $
    ST $ \s# ->
    case readDoubleArray# mba# i# s#      of {(# s2#, r# #) ->
    (# s2#, D# r# #)}

  {-# INLINE writeMBU #-}
  writeMBU (MBUArr n mba#) i@(I# i#) (D# e#) = 
    checkCritical "PAPrim.writeMBU[Double]" n i $
    ST $ \s# ->
    case writeDoubleArray# mba# i# e# s#  of {s2#   ->
    (# s2#, () #)}


-- |Loop combinators for unboxed arrays
-- -

-- |Replicate combinator for unboxed arrays
--
replicateBU :: UAE e => Int -> e -> BUArr e
{-# INLINE replicateBU #-}
replicateBU n e = 
  runST (do
    ma <- newMBU n
    fill0 ma
    unsafeFreezeMBU ma n
  )
  where
   fill0 ma = fill 0
     where
      fill off | off == n  = return ()
	       | otherwise = do
			       writeMBU ma off e
			       fill (off + 1)

-- |Loop combinator over unboxed arrays
--
loopBU :: (UAE e, UAE e')
       => (acc -> e -> (acc, Maybe e'))  -- mapping & folding, once per element
       -> acc				 -- initial acc value
       -> BUArr e			 -- input array
       -> (BUArr e', acc)
{-# INLINE loopBU #-}
loopBU mf start a = 
  runST (do
    ma          <- newMBU len
    (acc, len') <- trans0 ma start
    a'          <- unsafeFreezeMBU ma len'
    return (a', acc)
  )
  where
    len = lengthBU a
    --
    trans0 ma start = trans 0 0 start
      where
        trans a_off ma_off acc 
	  | a_off == len = ma_off `seq`	       -- needed for these arguments...
			   acc    `seq`	       -- ...getting unboxed
			   return (acc, ma_off)
	  | otherwise    =
	    do
	      let (acc', oe) = mf acc (a `indexBU` a_off)
	      ma_off' <- case oe of
			   Nothing -> return ma_off
			   Just e  -> do
				        writeMBU ma ma_off e
					return $ ma_off + 1
	      trans (a_off + 1) ma_off' acc'

-- |Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
--
loopArr :: (arr, acc) -> arr
{-# INLINE [1] loopArr #-}
loopArr (arr, _) = arr
loopAcc :: (arr, acc) -> acc
{-# INLINE [1] loopAcc #-}
loopAcc (_, acc) = acc
loopSndAcc :: (arr, (acc1, acc2)) -> (arr, acc2)
{-# INLINE [1] loopSndAcc #-}
loopSndAcc (arr, (_, acc)) = (arr, acc)

-- Loop fusion for unboxed arrays
--

{-# RULES  -- -} (for font-locking)

"loopBU/replicateBU" forall mf start n v.
  loopBU mf start (replicateBU n v) = 
    loopBU (\a _ -> mf v a) start (replicateBU n ())

"loopBU/loopBU" forall mf1 mf2 start1 start2 arr.
  loopBU mf2 start2 (loopArr (loopBU mf1 start1 arr)) =
    let
      mf (acc1, acc2) e = case mf1 e acc1 of
			    (acc1', Nothing) -> ((acc1', acc2), Nothing)
			    (acc1', Just e') ->
			      case mf1 e' acc2 of
			        (acc2', res) -> ((acc1', acc2'), res)
    in
    loopSndAcc (loopBU mf (start1, start2) arr)

"loopArr/loopSndAcc" forall x.
  loopArr (loopSndAcc x) = loopArr x

 #-}

-- |Loop-based combinators for unboxed arrays
-- -

-- |Extract a slice from an array (given by its start index and length)
--
sliceBU :: UAE e => BUArr e -> Int -> Int -> BUArr e
{-# INLINE sliceBU #-}
sliceBU arr i n = 
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
-- NB: If we had a bounded version of loopBU, we could express sliceBU in terms
--     of that loop combinator.  The problem is that this makes fusion more
--     awkward; in particular, when the second loopBU in a "loopBU/loopBU"
--     situation has restricted bounds.  On the other hand sometimes fusing
--     the extraction of a slice with the following computation on that slice
--     is very useful.
-- FIXME: If we leave it as it, we should at least use a block copy operation.
--	  (What we really want is to represent sliceBU as a loop when we can
--	  fuse it with a following loop on the computed slice and, otherwise,
--	  when there is no opportunity for fusion, we want to use a block copy
--	  routine.)

-- |Map a function over an unboxed array
--
mapBU :: (UAE a, UAE b) => (a -> b) -> BUArr a -> BUArr b
mapBU f = loopArr . loopBU (\_ e -> ((), Just $ f e)) () 

-- |Reduce an unboxed array
--
foldlBU :: UAE b => (a -> b -> a) -> a -> BUArr b -> a
{-# INLINE foldlBU #-}
foldlBU f z = loopAcc . loopBU (\a e -> (f a e, Nothing::Maybe ())) z

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
scanlBU f z = loopArr . loopBU (\a e -> (f a e, Just a)) z

-- |Prefix reduction of an unboxed array using an *associative* combining
-- operator
--
scanBU :: UAE a => (a -> a -> a) -> a -> BUArr a -> BUArr a
scanBU = scanlBU

-- |Extract a slice from a mutable array (the slice is immutable)
--
sliceMBU :: UAE e => MBUArr s e -> Int -> Int -> ST s (BUArr e)
{-# INLINE sliceMBU #-}
sliceMBU arr i n = do
		     arr' <- unsafeFreezeMBU arr (i + n)
		     return $ sliceBU arr' i n

-- |Insert a the contents of an immutable array into a mutable array from the
-- specified position on
--
insertMBU :: UAE e => MBUArr s e -> Int -> BUArr e -> ST s ()
{-# SPECIALIZE 
      insertMBU :: MBUArr s Int -> Int -> BUArr Int -> ST s () #-}
insertMBU marr i arr = ins i 0
  where
    n = lengthBU arr
    --
    ins i j | j == n    = return ()
	    | otherwise = do
			    writeMBU marr i (arr `indexBU` j)
			    ins (i + 1) (j + 1)

-- Show instance
--
instance (Show e, UAE e) => Show (BUArr e) where
  showsPrec _ a =   showString "toBU " 
		  . showList [a `indexBU` i | i <- [0..lengthBU a - 1]]

-- Auxilliary functions
-- --------------------

-- That's missing from Data.Array.Base
--
cHAR_SCALE :: Int# -> Int#
cHAR_SCALE n# = 4# *# n#

