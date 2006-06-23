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
  permuteU, permuteMU, bpermuteU, bpermuteDftU, reverseU, updateU
) where

import Data.Array.Parallel.Base (
  ST, runST, (:*:)(..))
import Data.Array.Parallel.Stream (
  Step(..), Stream(..))
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr, MUArr,
  lengthU, newU, newMU, unsafeFreezeMU, writeMU)
import Data.Array.Parallel.Unlifted.Flat.Stream (
  streamU)
import Data.Array.Parallel.Unlifted.Flat.Basics (
  (!:), enumFromToU)
import Data.Array.Parallel.Unlifted.Flat.Combinators (
  mapU)

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
bpermuteU a = mapU (a!:)

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
bpermuteDftU n init = updateU (mapU init . enumFromToU 0 $ n-1)

-- | Yield an array constructed by updating the first array by the
-- associations from the second array (which contains index/value pairs).
--
updateU :: UA e => UArr e -> UArr (Int :*: e) -> UArr e
{-# INLINE updateU #-}
updateU arr upd = update (streamU arr) (streamU upd)

update :: UA e => Stream e -> Stream (Int :*: e) -> UArr e
{-# INLINE [1] update #-}
update (Stream next1 s1 n) (Stream next2 s2 _) =
      runST (do
        marr <- newMU n
        n'   <- fill0 marr
        unsafeFreezeMU marr n'
      )
  where
    fill0 marr = do
                   n' <- fill s1 0
                   upd s2
                   return n'
      where
        fill s1 i = i `seq`
                    case next1 s1 of
                      Done        -> return i
                      Skip s1'    -> fill s1' i
                      Yield x s1' -> do
                                       writeMU marr i x
                                       fill s1' (i+1)
        upd s2 = case next2 s2 of
                   Done                -> return ()
                   Skip s2'            -> upd s2'
                   Yield (i :*: x) s2' -> do
                                            writeMU marr i x
                                            upd s2'
                                         

-- |Reverse the order of elements in an array
--
reverseU :: UA e => UArr e -> UArr e
reverseU a = mapU (a!:) . enumFromToU 0 $ lengthU a - 1

