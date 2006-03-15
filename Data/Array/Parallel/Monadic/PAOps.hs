-- |Loop-based combinators
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  $Id: PAOps.hs,v 1.14 2002/12/02 07:42:40 chak Exp $
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 + multi-parameter classes
--
--  Parallel array versions of the combinators that are either commonly used
--  with lists or in Nesl-style languages.
--
--- Todo ----------------------------------------------------------------------
--

module PAOps (
  -- * List-like combinators
  mapP,	(+:+), filterP, concatP, {-concatMapP,-} nullP, (!:), foldlP, foldlSP,
  {-foldl1P,-} scanlP, {-scanl1P, foldrP, foldr1P, scanrP, scanr1P,-}
  foldP, foldSP, {-fold1P, fold1SP,-} scanP, {-scanSP, scan1P, scan1SP,-}
  takeP, dropP,	splitAtP, {-takeWhileP, dropWhileP, spanP, breakP,-}
--  lines, words, unlines, unwords,  -- is string processing really needed
  reverseP, andP, andSP, orP, orSP, anyP, allP, elemP, notElemP, {-lookupP,-}
  sumP, sumSP, productP, productSP, maximumP, maximumSP, minimumP, minimumSP, 
  zipP, zip3P, zipWithP, zipWith3P, unzipP, unzip3P, enumFromToP,
  enumFromToSP, enumFromThenToP, enumFromThenToSP, 

  -- * Array-oriented combinators
  --
  flattenP, (>:), segmentP, toP, toSP, fromP, emptyP, 
  permuteP, bpermuteP, bpermuteSP, bpermuteDftP, {-crossP, indexOfP -}
) where

-- GHC-specific libraries
import Data.Generics

-- friends
import Data.Array.Parallel.Base.UArr  (indexU, runST)
import PABase    (PArray, FArray, PArr, SPArr, PAProd, PArrBool, PArrInt,
		  SPArrInt, SPArrBool, UInt,
		  lengthP, toSegd, sliceP, newMP, newMSP, writeMP, nextMSP,
		  unsafeFreezeMP, psumS) 
import qualified
       PABase    (zipP, unzipP)
import PALoop    (loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc)
import PAEP      (EP(..), indexP, (>:), flattenP, replicateP, loopP,
		  replicateSP, loopSP)
import PAFusion  (noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL,
		  transSFL, keepSFL)


infixl 9 !:
infixr 5 +:+
infix  4 `elemP`, `notElemP`


-- |List-like combinators
-- ----------------------

-- |Map a function over an array
--
mapP :: (EP e r, PArray r arr, EP e' r', FArray r' arr')
     => (e -> e') -> PArr arr e -> PArr arr' e'
{-# INLINE mapP #-}
mapP f = loopArr . loopP (mapEFL f) noAL

-- |Concatenate two arrays
--
(+:+) :: (EP e r, FArray r arr)
      => PArr arr e -> PArr arr e -> PArr arr e
{-# INLINE (+:+) #-}
a1 +:+ a2 = loopArr $ loopP extract 0 (replicateP len noAL)
  where
    len1 = lengthP a1
    len  = len1 + lengthP a2
    --
    extract i _ = (i + 1, Just $ if i < len1 then a1!:i else a2!:(i - len1))

-- |Extract all elements from an array that meet the given predicate
--
filterP :: (EP e r, FArray r arr) 
	=> (e -> Bool) -> PArr arr e -> PArr arr e 
{-# INLINE filterP #-}
filterP p  = loopArr . loopP (filterEFL p) noAL

-- |Concatenate the subarrays of an array of arrays
--
concatP :: PArray r arr => SPArr arr e -> PArr arr e
concatP = snd . flattenP

-- |Test whether the given array is empty
--
nullP :: PArray r arr => PArr arr e -> Bool
nullP  = (== 0) . lengthP

-- |Yield an empty array
--
emptyP :: FArray r arr => PArr arr e
emptyP = runST (do
	   mpa <- newMP 0
	   unsafeFreezeMP mpa 0
         )

-- |Array indexing
--
(!:) :: (EP e r, PArray r arr) 
     => PArr arr e -> Int -> e
(!:) = indexP

-- |Array reduction proceeding from the left
--
foldlP :: (EP a r, PArray r arr) => (b -> a -> b) -> b -> PArr arr a -> b
{-# INLINE foldlP #-}
foldlP f z = loopAcc . loopP (foldEFL f) z

-- |Array reduction that requires an associative combination function with its
-- unit
--
foldP :: (EP a r, PArray r arr) => (a -> a -> a) -> a -> PArr arr a -> a
foldP = foldlP

-- |Segmented array reduction proceeding from the left
--
foldlSP :: (EP a ra, PArray ra arra, EP b rb, FArray rb arrb)
	=> (b -> a -> b) -> b -> SPArr arra a -> PArr arrb b
{-# INLINE foldlSP #-}
foldlSP f z = loopAccS . loopSP (foldEFL f) (keepSFL (const z)) z

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSP :: (EP a r, FArray r arr) 
       => (a -> a -> a) -> a -> SPArr arr a -> PArr arr a
foldSP = foldlSP

-- |Prefix scan proceedings from left to right
--
scanlP :: (EP a ar, PArray ar aarr, EP b br, FArray br barr) 
       => (b -> a -> b) -> b -> PArr aarr a -> PArr barr b
{-# INLINE scanlP #-}
scanlP f z = loopArr . loopP (scanEFL f) z

-- |Prefix scan proceedings from left to right that needs an associative
-- combination function with its unit
--
scanP :: (EP a r, FArray r arr) 
      => (a -> a -> a) -> a -> PArr arr a -> PArr arr a
scanP = scanlP

-- |Extract a prefix of an array
--
takeP :: PArray r arr
      => Int -> PArr arr e -> PArr arr e
{-# INLINE takeP #-}
takeP n a = sliceP a 0 n

-- |Extract a suffix of an array
--
dropP :: PArray r arr
      => Int -> PArr arr e -> PArr arr e
{-# INLINE dropP #-}
dropP n a = let len = lengthP a 
	    in
	    sliceP a n (len - n)

-- |Split an array into two halves at the given index
--
splitAtP :: PArray r arr
         => Int -> PArr arr e -> (PArr arr e, PArr arr e)
{-# INLINE splitAtP #-}
splitAtP n a = (takeP n a, dropP n a)

-- |Reverse the order of elements in an array
--
reverseP :: (EP e r, FArray r arr)
	 => PArr arr e -> PArr arr e
reverseP a = loopArr $ loopP extract (len - 1) (replicateP len noAL)
	     where
	       len = lengthP a
	       --
	       extract i _ = (i - 1, Just $ a!:i)

-- |
andP :: PArrBool -> Bool
andP = foldP (&&) True

-- |
andSP :: SPArrBool -> PArrBool
andSP = foldSP (&&) True

-- |
orP :: PArrBool -> Bool
orP = foldP (||) False

-- |
orSP :: SPArrBool -> PArrBool
orSP = foldSP (||) False

-- |
allP :: (EP e r, PArray r arr) 
     => (e -> Bool) -> PArr arr e -> Bool
{-# INLINE allP #-}
allP p = andP . mapP p

-- |
anyP :: (EP e r, PArray r arr) 
     => (e -> Bool) -> PArr arr e -> Bool
{-# INLINE anyP #-}
anyP p =  orP . mapP p

-- |Determine whether the given element is in an array
--
elemP :: (Eq e, EP e r, PArray r arr) 
      => e -> PArr arr e -> Bool
elemP e = anyP (== e)

-- |Negation of `elemP'
--
notElemP :: (Eq e, EP e r, PArray r arr) 
	 => e -> PArr arr e -> Bool
notElemP e = allP (/= e)

-- |Compute the sum of an array of numerals
--
sumP :: (Num e, EP e r, PArray r arr) => PArr arr e -> e
{-# INLINE sumP #-}
sumP = foldP (+) 0

-- |Compute the segmented sum of an array of numerals
--
sumSP :: (Num e, EP e r, FArray r arr) => SPArr arr e -> PArr arr e
{-# INLINE sumSP #-}
sumSP = foldSP (+) 0

-- |Compute the product of an array of numerals
--
productP :: (Num e, EP e r, PArray r arr) => PArr arr e -> e
{-# INLINE productP #-}
productP = foldP (*) 0

-- |Compute the segmented product of an array of numerals
--
productSP :: (Num e, EP e r, FArray r arr) => SPArr arr e -> PArr arr e
{-# INLINE productSP #-}
--productSP  = foldSP (*) 0
productSP  = undefined
--FIXME

-- |Determine the maximum element in an array
--
maximumP :: (Bounded e, Ord e, EP e r, PArray r arr) 
         => PArr arr e -> e
--FIXME: provisional
--maximumP :: (Ord e, EP e r, PArray r arr) => PArr arr -> e
{-# INLINE maximumP #-}
--maximumP = fold1P max
maximumP = foldP max (minBound)

-- |Determine the maximum element in each subarray
--
maximumSP :: (Bounded e, Ord e, EP e r, FArray r arr) 
          => SPArr arr e -> PArr arr e
--FIXME: provisional
--maximumSP :: (Ord e, EP e r, PArray r arr) => SPArr arr -> PArr arr
{-# INLINE maximumSP #-}
--maximumSP = fold1SP max
maximumSP = foldSP max minBound

-- |Determine the minimum element in an array
--
minimumP :: (Bounded e, Ord e, EP e r, PArray r arr) 
	 => PArr arr e -> e
--FIXME: provisional
--minimumP :: (Ord e, EP e r, PArray r arr) => PArr arr -> e
{-# INLINE minimumP #-}
--minimumP = fold1P min
minimumP = foldP min maxBound

-- |Determine the minimum element in each subarray
--
minimumSP :: (Bounded e, Ord e, EP e r, FArray r arr) 
	  => SPArr arr e -> PArr arr e
--FIXME: provisional
--minimumSP :: (Ord e, EP e r, PArray r arr) => SPArr arr -> PArr arr
{-# INLINE minimumSP #-}
--minimumSP = fold1SP min
minimumSP = foldSP min maxBound

-- |
zipP :: (PArray r1 arr1, PArray r2 arr2) 
     => PArr arr1 e1 -> PArr arr2 e2 -> PArr (PAProd arr1 arr2) (e1, e2)
zipP = PABase.zipP

-- |
zip3P :: (PArray r1 arr1, PArray r2 arr2, PArray r3 arr3) 
      => PArr arr1 e1 -> PArr arr2 e2 -> PArr arr3 e3
      -> PArr (PAProd (PAProd arr1 arr2) arr3) (e1, e2, e3)
{-# INLINE zip3P #-}
zip3P a1 a2 a3 = (a1 `PABase.zipP` a2) `PABase.zipP` a3

-- |
zipWithP :: (EP a ra, PArray ra arra, 
	     EP b rb, PArray rb arrb, 
	     EP c rc, FArray rc arrc)
         => (a -> b -> c) -> PArr arra a -> PArr arrb b -> PArr arrc c
{-# INLINE zipWithP #-}
zipWithP f a1 a2 = loopArr $ loopP (mapEFL (uncurry f)) noAL (zipP a1 a2)

-- |
zipWith3P :: (EP a ra, PArray ra arra, 
	      EP b rb, PArray rb arrb, 
	      EP c rc, PArray rc arrc,
	      EP d rd, FArray rd arrd)
          => (a -> b -> c -> d) 
	  -> PArr arra a -> PArr arrb b -> PArr arrc c -> PArr arrd d
{-# INLINE zipWith3P #-}
zipWith3P f a1 a2 a3 = 
  loopArr $ loopP (mapEFL (\(x, y, z) -> f x y z)) noAL (zip3P a1 a2 a3)

-- |
unzipP :: (PArray r1 arr1, PArray r2 arr2) 
       => PArr (PAProd arr1 arr2) (e1, e2) -> (PArr arr1 e1, PArr arr2 e2)
unzipP = PABase.unzipP

-- |
unzip3P :: (PArray r1 arr1, PArray r2 arr2, PArray r3 arr3) 
        => PArr (PAProd (PAProd arr1 arr2) arr3) (e1, e2, e3)
        -> (PArr arr1 e1, PArr arr2 e2, PArr arr3 e3)
{-# INLINE unzip3P #-}
unzip3P a = let (a12, a3) = PABase.unzipP a
		(a1 , a2) = PABase.unzipP a12
	    in
	    (a1, a2, a3)


-- |Enumeration functions
-- ----------------------

-- |Yield an enumerated array
--
enumFromToP :: (Enum e, EP e r, FArray r arr)
	    => e -> e -> PArr arr e
{-# INLINE enumFromToP #-}
enumFromToP start = enumFromThenToP start (succ start)

-- |Yield a segmented enumerated array
--
enumFromToSP :: (Enum e, EP e r, FArray r arr)
	     => PArr arr e -> PArr arr e -> SPArr arr e
{-# INLINE enumFromToSP #-}
enumFromToSP starts = enumFromThenToSP starts (mapP succ starts)

-- |Yield an enumerated array using a specific step
--
enumFromThenToP :: (Enum e, EP e r, FArray r arr)
		=> e -> e -> e -> PArr arr e
{-# INLINE enumFromThenToP #-}
enumFromThenToP start next end = 
  loopArr $ loopP step start' (replicateP len noAL)
  where
    start' = fromEnum start
    next'  = fromEnum next
    end'   = fromEnum end
    delta  = next' - start'
    len    = abs (end' - start' + delta) `div` (abs delta)
    --
    step x _ = (x + delta, Just $ toEnum x)

-- |Yield a segmented enumerated array using a specific step
--
enumFromThenToSP :: (Enum e, EP e r, FArray r arr)
		 => PArr arr e -> PArr arr e -> PArr arr e -> SPArr arr e
{-# INLINE enumFromThenToSP #-}
enumFromThenToSP starts nexts ends = 
  loopArrS $ loopSP step seg init (segd >: replicateP len Unit)
  where
    lens    = zipWith3P calcLen starts nexts ends
	      where
		calcLen start next end = 
		  abs (end' - start' + delta) `div` (abs delta)
		  where
		    start' = fromEnum start
		    next'  = fromEnum next
		    end'   = fromEnum end
		    delta  = next' - start'
    len     = sumP    lens
    segd    = toSegd  lens
    segdlen = lengthP lens
    --
    step (x, delta) _ = ((x + delta, delta), Just $ toEnum x)
    seg  _          i = ((start, delta), Nothing::Maybe Unit)
			where
			  start = fromEnum (starts!:(i + 1))
			  next  = fromEnum (nexts !:(i + 1))
			  delta = if (i + 1) == segdlen 
				  then 0 
				  else next - start
    --
    init = fst $ seg undefined (-1)


-- |Segmentation
-- -------------

-- |Segment an array according to the given vector of segment lengths
--
-- FIXME: Change this to the type used in the lecture notes?  To pass around a
--   segd without any array structure attached, we could always pass a
--   properly segmented unit array.
segmentP :: PArray r arr => PArrInt -> PArr arr e -> SPArr arr e
{-# INLINE segmentP #-}
segmentP lens arr = toSegd lens >: arr


-- |Conversion
-- -----------

-- |Turn a list into a parallel array
--
toP :: (EP e r, FArray r arr) => [e] -> PArr arr e
{-# INLINE toP #-}
toP l = 
  loopArr $ 
    loopP (\(x:xs) (_::Unit) -> (xs, Just $ x)) l (replicateP (length l) noAL)

-- |Turn a nested list into a segmented parallel array
--
toSP :: (EP e r, FArray r arr) => [[e]] -> SPArr arr e
{-# INLINE toSP #-}
toSP ls = let lens = toP $ map length ls
	      a    = toP $ concat ls
          in
	  lens `segmentP` a

-- |Collect the elements of a parallel array in a list
--
fromP :: (EP e r, PArray r arr) => PArr arr e -> [e]
{-# INLINE fromP #-}
fromP a = [a!:i | i <- [0..lengthP a - 1]]


-- |Permutations
-- -------------

-- |Standard permutation
--
permuteP :: (EP e r, FArray r arr) => PArr arr e -> PArrInt -> PArr arr e
{-# INLINE permuteP #-}
permuteP arr is = 
  runST (do
    mpa <- newMP n
    permute0 mpa
    unsafeFreezeMP mpa n
  )
  where
    n		 = lengthP arr
    permute0 mpa = permute 0
      where
        permute i 
	  | i == n    = return ()
	  | otherwise = writeMP mpa (is!:i) (from (arr!:i)) >> permute (i + 1)

-- |Back permutation operation (ie, the permutation vector determines for each
-- position in the result array its origin in the input array)
--
bpermuteP :: (EP e r, FArray r arr) => PArr arr e -> PArrInt -> PArr arr e
{-# INLINE bpermuteP #-}
bpermuteP a = loopArr . loopP (mapEFL (a !:)) noAL

-- |Segmented back permute
--
bpermuteSP :: (EP e r, FArray r arr) => SPArr arr e -> SPArrInt -> SPArr arr e
{-# INLINE bpermuteSP #-}
bpermuteSP as = loopArrS . loopSP extract nextOff 0
	        where
		  (segd, a) = flattenP as
		  psum	    = psumS segd
		  --
	          extract off i = (off, Just $ a!:(off + i))
		  --
		  nextOff _ segi = (psum `indexU` (segi + 1), 
				    Nothing::Maybe Unit)

-- |Default back permute
--
-- * The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
-- * All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
--
bpermuteDftP :: (EP e r, FArray r arr)
	     => Int			        -- length of result array
	     -> (Int -> e)		        -- initialiser function
	     -> PArr (PAProd UInt arr) (Int, e) -- index-value pairs
	     -> PArr arr e
{-# INLINE bpermuteDftP #-}
bpermuteDftP n init arr =
  runST (do
    mpa <- newMP n
    doInit0  mpa
    permute0 mpa
    unsafeFreezeMP mpa n
  )
  where
    doInit0 mpa = doInit 0
      where
        doInit i | i == n    = return ()
		 | otherwise = writeMP mpa i (from $ init i) >> doInit (i + 1)
    --
    m		 = lengthP arr
    permute0 mpa = permute 0
      where
        permute i 
	  | i == m    = return ()
	  | otherwise = do
			  let (j, e) = arr!:i
			  writeMP mpa j (from e)
			  permute (i + 1)
