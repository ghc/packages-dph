-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Listlike
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
--  Unlifted array versions of Nesl-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.NeslLike (
  -- * Nesl-like combinators
  flattenSU, (>:), segmentU, toU, toSU, fromU, emptyU, extractU, sliceU,
  permuteU, permuteMU, bpermuteU, bpermuteSU, bpermuteDftU, {-crossU, indexOfU -}
) where

-- friends
import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Base.BUArr (
  indexBU, ST, runST)
import Data.Array.Parallel.Monadic.UArr (
  UA, UArr, MUArr, lengthU, indexU, extractU, sliceU,
  newMU, writeMU, unsafeFreezeMU) 
import Data.Array.Parallel.Monadic.SUArr (
  SUArr, toUSegd, (>:), flattenSU, psumUS) 
import Data.Array.Parallel.Declarative.Loop (
  replicateU, loopU, replicateSU, loopSU)
import Data.Array.Parallel.Declarative.Fusion (
  noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL,
  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc)
import Data.Array.Parallel.Unlifted.ListLike
  ((!:))


-- |Segmentation
-- -------------

-- |Segment an array according to the segmentation of the first argument
--
segmentU :: (UA e', UA e) => SUArr e' -> UArr e -> SUArr e
{-# INLINE segmentU #-}
segmentU template arr = fstS (flattenSU template) >: arr


-- |Conversion
-- -----------

-- |Turn a list into a parallel array
--
toU :: UA e => [e] -> UArr e
{-# INLINE toU #-}
toU l = 
  loopArr $ 
    loopU (\(x:xs) (_::()) -> (xs :*: Just x)) l (replicateU (length l) ())

-- |Turn a nested list into a segmented parallel array
--
toSU :: UA e => [[e]] -> SUArr e
{-# INLINE toSU #-}
toSU ls = let lens = toU $ map length ls
	      a    = toU $ concat ls
          in
	  toUSegd lens >: a

-- |Collect the elements of a parallel array in a list
--
fromU :: UA e => UArr e -> [e]
{-# INLINE fromU #-}
fromU a = [a!:i | i <- [0..lengthU a - 1]]

-- |Yield an empty array
--
emptyU :: UA e => UArr e
emptyU = runST (do
	   mpa <- newMU 0
	   unsafeFreezeMU mpa 0
         )

-- extractU is re-exported from UArr


-- |Permutations
-- -------------

permuteMU :: UA e => MUArr e s -> UArr e -> UArr Int -> ST s ()
permuteMU mpa arr is = permute 0
  where
    n = lengthU arr
    permute i
      | i == n    = return ()
      | otherwise = writeMU mpa (is!:i) (arr!:i) >> permute (i + 1)
    

-- |Standard permutation
--
permuteU :: UA e => UArr e -> UArr Int -> UArr e
{-# INLINE permuteU #-}
permuteU arr is =
  runST (do
    mpa <- newMU n
    permuteMU mpa arr is
    unsafeFreezeMU mpa n
  )
  where
    n = lengthU arr
{-  runST (do
    mpa <- newMU n
    permute0 mpa
    unsafeFreezeMU mpa n
  )
  where
    n		 = lengthU arr
    permute0 mpa = permute 0
      where
        permute i 
	  | i == n    = return ()
	  | otherwise = writeMU mpa (is!:i) (arr!:i) >> permute (i + 1)
-}

-- |Back permutation operation (ie, the permutation vector determines for each
-- position in the result array its origin in the input array)
--
bpermuteU :: UA e => UArr e -> UArr Int -> UArr e
{-# INLINE bpermuteU #-}
bpermuteU a = loopArr . loopU (mapEFL (a !:)) noAL

-- |Segmented back permute
--
bpermuteSU :: UA e => SUArr e -> SUArr Int -> SUArr e
{-# INLINE bpermuteSU #-}
bpermuteSU as = loopArrS . loopSU extract nextOff 0
	        where
		  (segd :*: a) = flattenSU as
		  psum	       = psumUS segd
		  --
	          extract off i = (off :*: (Just $ a!:(off + i)))
		  --
		  nextOff _ segi = (psum `indexBU` (segi + 1) :*: 
				    (Nothing::Maybe ()))

-- |Default back permute
--
-- * The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
-- * All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
--
bpermuteDftU :: UA e
	     => Int			        -- |length of result array
	     -> (Int -> e)		        -- |initialiser function
	     -> UArr (Int :*: e)		-- |index-value pairs
	     -> UArr e
{-# INLINE bpermuteDftU #-}
bpermuteDftU n init arr =
  runST (do
    mpa <- newMU n
    doInit0  mpa
    permute0 mpa
    unsafeFreezeMU mpa n
  )
  where
    doInit0 mpa = doInit 0
      where
        doInit i | i == n    = return ()
		 | otherwise = writeMU mpa i (init i) >> doInit (i + 1)
    --
    m		 = lengthU arr
    permute0 mpa = permute 0
      where
        permute i 
	  | i == m    = return ()
	  | otherwise = do
			  let (j :*: e) = arr!:i
			  writeMU mpa j e
			  permute (i + 1)
