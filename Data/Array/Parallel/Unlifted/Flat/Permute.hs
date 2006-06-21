-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Permute
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Permutations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Permute (
  permuteU, permuteMU, bpermuteU, bpermuteDftU, reverseU
) where

import Data.Array.Parallel.Base (
  ST, (:*:)(..))
import Data.Array.Parallel.Stream (
  mapS, enumFromToS)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr, MUArr,
  lengthU, newU, writeMU)
import Data.Array.Parallel.Unlifted.Flat.Stream (
  streamU, unstreamU)
import Data.Array.Parallel.Unlifted.Flat.Basics (
  (!:))

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
permuteU arr is = newU (lengthU arr) $ \mpa -> permuteMU mpa arr is

-- |Back permutation operation (ie, the permutation vector determines for each
-- position in the result array its origin in the input array)
--
bpermuteU :: UA e => UArr e -> UArr Int -> UArr e
{-# INLINE bpermuteU #-}
bpermuteU a = unstreamU . mapS (a !:) . streamU

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
bpermuteDftU n init arr = newU n $ \mpa -> doInit0 mpa >> permute0 mpa
{-  runST (do
    mpa <- newMU n
    doInit0  mpa
    permute0 mpa
    unsafeFreezeMU mpa n
  ) -}
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

-- |Reverse the order of elements in an array
--
reverseU :: UA e => UArr e -> UArr e
reverseU a = unstreamU . mapS (a!:) . enumFromToS 0 $ lengthU a - 1

