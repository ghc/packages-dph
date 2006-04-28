-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.ListLike
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Unlifted array versions of list-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.ListLike (
  -- * List-like combinators
  mapU,	(+:+), filterU, concatSU, {-concatMapU,-} nullU, lengthU, (!:), foldlU,
  foldlSU, {-foldl1U,-} scanlU, {-scanl1U, foldrU, foldr1U, scanrU, scanr1U,-}
  foldU, foldSU, {-fold1U, fold1SU,-} scanU, {-scanSU, scan1U, scan1SU,-}
  replicateU,
  takeU, dropU,	splitAtU, {-takeWhileU, dropWhileU, spanU, breakU,-}
--  lines, words, unlines, unwords,  -- is string processing really needed
  reverseU, andU, andSU, orU, orSU, anyU, allU, elemU, notElemU, {-lookupU,-}
  sumU, sumSU, productU, productSU, maximumU, maximumSU, minimumU, minimumSU, 
  zipU, zip3U, zipWithU, zipWith3U, unzipU, unzip3U, enumFromToU,
  enumFromToSU, enumFromThenToU, enumFromThenToSU, 
) where

-- friends
import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Base.BUArr (
  indexBU, runST)
import Data.Array.Parallel.Monadic.UArr (
  UA, UArr, lengthU, indexU, extractU, newMU, writeMU, unsafeFreezeMU, zipU,
  unzipU) 
import Data.Array.Parallel.Monadic.SUArr (
  SUArr, toUSegd, (>:), flattenSU)
import Data.Array.Parallel.Declarative.Loop (
  replicateU, loopU, replicateSU, loopSU,
  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc)
import Data.Array.Parallel.Declarative.Fusion (
  noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL)


infixl 9 !:
infixr 5 +:+
infix  4 `elemU`, `notElemU`


-- |List-like combinators
-- ----------------------

-- |Map a function over an array
--
mapU :: (UA e, UA e') => (e -> e') -> UArr e -> UArr e'
{-# INLINE mapU #-}
mapU f = loopArr . loopU (mapEFL f) noAL

-- |Concatenate two arrays
--
(+:+) :: UA e => UArr e -> UArr e -> UArr e
{-# INLINE (+:+) #-}
a1 +:+ a2 = loopArr $ loopU extract 0 (replicateU len noAL)
  where
    len1 = lengthU a1
    len  = len1 + lengthU a2
    --
    extract i _ = (i + 1, Just $ if i < len1 then a1!:i else a2!:(i - len1))

-- |Extract all elements from an array that meet the given predicate
--
filterU :: UA e => (e -> Bool) -> UArr e -> UArr e 
{-# INLINE filterU #-}
filterU p  = loopArr . loopU (filterEFL p) noAL

-- |Concatenate the subarrays of an array of arrays
--
concatSU :: UA e => SUArr e -> UArr e
concatSU = snd . flattenSU

-- |Test whether the given array is empty
--
nullU :: UA e => UArr e -> Bool
nullU  = (== 0) . lengthU

-- lengthU is re-exported from UArr

-- |Array indexing
--
(!:) :: UA e => UArr e -> Int -> e
(!:) = indexU

-- |Array reduction proceeding from the left
--
foldlU :: UA a => (b -> a -> b) -> b -> UArr a -> b
{-# INLINE foldlU #-}
foldlU f z = loopAcc . loopU (foldEFL f) z

-- |Array reduction that requires an associative combination function with its
-- unit
--
foldU :: UA a => (a -> a -> a) -> a -> UArr a -> a
foldU = foldlU

-- |Segmented array reduction proceeding from the left
--
foldlSU :: (UA a, UA b) => (b -> a -> b) -> b -> SUArr a -> UArr b
{-# INLINE foldlSU #-}
foldlSU f z = loopAccS . loopSU (foldEFL f) (keepSFL (const z)) z

-- |Segmented array reduction that requires an associative combination
-- function with its unit
--
foldSU :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
foldSU = foldlSU

-- |Prefix scan proceedings from left to right
--
scanlU :: (UA a, UA b) => (b -> a -> b) -> b -> UArr a -> UArr b
{-# INLINE scanlU #-}
scanlU f z = loopArr . loopU (scanEFL f) z

-- |Prefix scan proceedings from left to right that needs an associative
-- combination function with its unit
--
scanU :: UA a => (a -> a -> a) -> a -> UArr a -> UArr a
scanU = scanlU

-- |Extract a prefix of an array
--
takeU :: UA e=> Int -> UArr e -> UArr e
{-# INLINE takeU #-}
takeU n a = extractU a 0 n

-- |Extract a suffix of an array
--
dropU :: UA e => Int -> UArr e -> UArr e
{-# INLINE dropU #-}
dropU n a = let len = lengthU a 
	    in
	    extractU a n (len - n)

-- |Split an array into two halves at the given index
--
splitAtU :: UA e => Int -> UArr e -> (UArr e, UArr e)
{-# INLINE splitAtU #-}
splitAtU n a = (takeU n a, dropU n a)

-- |Reverse the order of elements in an array
--
reverseU :: UA e => UArr e -> UArr e
reverseU a = loopArr $ loopU extract (len - 1) (replicateU len noAL)
	     where
	       len = lengthU a
	       --
	       extract i _ = (i - 1, Just $ a!:i)

-- |
andU :: UArr Bool -> Bool
andU = foldU (&&) True

-- |
andSU :: SUArr Bool -> UArr Bool
andSU = foldSU (&&) True

-- |
orU :: UArr Bool -> Bool
orU = foldU (||) False

-- |
orSU :: SUArr Bool -> UArr Bool
orSU = foldSU (||) False

-- |
allU :: UA e => (e -> Bool) -> UArr e -> Bool
{-# INLINE allU #-}
allU p = andU . mapU p

-- |
anyU :: UA e => (e -> Bool) -> UArr e -> Bool
{-# INLINE anyU #-}
anyU p =  orU . mapU p

-- |Determine whether the given element is in an array
--
elemU :: (Eq e, UA e) => e -> UArr e -> Bool
elemU e = anyU (== e)

-- |Negation of `elemU'
--
notElemU :: (Eq e, UA e) => e -> UArr e -> Bool
notElemU e = allU (/= e)

-- |Compute the sum of an array of numerals
--
sumU :: (Num e, UA e) => UArr e -> e
{-# INLINE sumU #-}
sumU = foldU (+) 0

-- |Compute the segmented sum of an array of numerals
--
sumSU :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE sumSU #-}
sumSU = foldSU (+) 0

-- |Compute the product of an array of numerals
--
productU :: (Num e, UA e) => UArr e -> e
{-# INLINE productU #-}
productU = foldU (*) 0

-- |Compute the segmented product of an array of numerals
--
productSU :: (Num e, UA e) => SUArr e -> UArr e
{-# INLINE productSU #-}
productSU = foldSU (*) 1

-- |Determine the maximum element in an array
--
maximumU :: (Bounded e, Ord e, UA e) => UArr e -> e
--FIXME: provisional until fold1U implemented
--maximumU :: (Ord e, UA e) => UArr e -> e
{-# INLINE maximumU #-}
--maximumU = fold1U max
maximumU = foldU max (minBound)

-- |Determine the maximum element in each subarray
--
maximumSU :: (Bounded e, Ord e, UA e) => SUArr e -> UArr e
--FIXME: provisional until fold1SU implemented
--maximumSU :: (Ord e, MUA e) => UArr (UArr e) -> UArr e
{-# INLINE maximumSU #-}
--maximumSU = fold1SU max
maximumSU = foldSU max minBound

-- |Determine the minimum element in an array
--
minimumU :: (Bounded e, Ord e, UA e) => UArr e -> e
--FIXME: provisional until fold1U implemented
--minimumU :: (Ord e, UA e) => UArr e -> e
{-# INLINE minimumU #-}
--minimumU = fold1U min
minimumU = foldU min maxBound

-- |Determine the minimum element in each subarray
--
minimumSU :: (Bounded e, Ord e, UA e) => SUArr e -> UArr e
--FIXME: provisional until fold1SU implemented
--minimumSU :: (Ord e, MUA e) => UArr (UArr e) -> UArr e
{-# INLINE minimumSU #-}
--minimumSU = fold1SU min
minimumSU = foldSU min maxBound

-- zipU is re-exported from UArr

-- |
zip3U :: (UA e1, UA e2, UA e3) 
      => UArr e1 -> UArr e2 -> UArr e3 -> UArr (e1 :*: e2 :*: e3)
{-# INLINE zip3U #-}
zip3U a1 a2 a3 = (a1 `zipU` a2) `zipU` a3

-- |
zipWithU :: (UA a, UA b, UA c) 
	 => (a -> b -> c) -> UArr a -> UArr b -> UArr c
{-# INLINE zipWithU #-}
zipWithU f a1 a2 = 
  loopArr $ loopU (mapEFL (\(x:*:y) -> f x y)) noAL (zipU a1 a2)

-- |
zipWith3U :: (UA a, UA b, UA c, UA d) 
          => (a -> b -> c -> d) -> UArr a -> UArr b -> UArr c -> UArr d
{-# INLINE zipWith3U #-}
zipWith3U f a1 a2 a3 = 
  loopArr $ loopU (mapEFL (\(x:*:y:*:z) -> f x y z)) noAL (zip3U a1 a2 a3)

-- unzipP is re-exported from UArr

-- |
unzip3U :: (UA e1, UA e2, UA e3) 
	=> UArr (e1 :*: e2 :*: e3) -> (UArr e1, UArr e2, UArr e3)
{-# INLINE unzip3U #-}
unzip3U a = let (a12, a3) = unzipU a
		(a1 , a2) = unzipU a12
	    in
	    (a1, a2, a3)


-- |Enumeration functions
-- ----------------------

-- |Yield an enumerated array
--
enumFromToU :: (Enum e, UA e) => e -> e -> UArr e
{-# INLINE enumFromToU #-}
enumFromToU start = enumFromThenToU start (succ start)

-- |Yield a segmented enumerated array
--
enumFromToSU :: (Enum e, UA e) => UArr e -> UArr e -> SUArr e
{-# INLINE enumFromToSU #-}
enumFromToSU starts = enumFromThenToSU starts (mapU succ starts)

-- |Yield an enumerated array using a specific step
--
enumFromThenToU :: (Enum e, UA e) => e -> e -> e -> UArr e
{-# INLINE enumFromThenToU #-}
enumFromThenToU start next end = 
  loopArr $ loopU step start' (replicateU len noAL)
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
enumFromThenToSU :: (Enum e, UA e) 
		 => UArr e -> UArr e -> UArr e -> SUArr e
{-# INLINE enumFromThenToSU #-}
enumFromThenToSU starts nexts ends = 
  loopArrS $ loopSU step seg init (segd >: replicateU len ())
  where
    lens    = zipWith3U calcLen starts nexts ends
	      where
		calcLen start next end = 
		  abs (end' - start' + delta) `div` (abs delta)
		  where
		    start' = fromEnum start
		    next'  = fromEnum next
		    end'   = fromEnum end
		    delta  = next' - start'
    len     = sumU    lens
    segd    = toUSegd lens
    segdlen = lengthU lens
    --
    step (x, delta) _ = ((x + delta, delta), Just $ toEnum x)
    seg  _          i = ((start, delta), Nothing::Maybe ())
			where
			  start = fromEnum (starts!:(i + 1))
			  next  = fromEnum (nexts !:(i + 1))
			  delta = if (i + 1) == segdlen 
				  then 0 
				  else next - start
    --
    init = fst $ seg undefined (-1)
