----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Permute
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Permutations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Permute (
  permuteU, permuteMU, mbpermuteU, bpermuteU, bpermuteDftU, reverseU, updateU,
  atomicUpdateMU
) where

import Data.Array.Parallel.Base (
  ST, runST, (:*:)(..), Rebox(..))
import Data.Array.Parallel.Stream (
  Step(..), Stream(..), mapS)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr, MUArr,
  lengthU, newU, newDynU, newMU, unsafeFreezeAllMU, writeMU,
  sliceU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  unstreamU, streamU, unstreamMU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Basics (
  (!:))
import Data.Array.Parallel.Unlifted.Sequential.Flat.Enum (
  enumFromToU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators (
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
{-# INLINE_U permuteU #-}
permuteU arr is = newU (lengthU arr) $ \mpa -> permuteMU mpa arr is

-- |Back permutation operation (ie, the permutation vector determines for each
-- position in the result array its origin in the input array)
--
-- WARNING: DO NOT rewrite this as unstreamU . bpermuteUS es . streamU
-- because GHC won't be able to figure out its strictness.
--
bpermuteU :: UA e => UArr e -> UArr Int -> UArr e
{-# INLINE_U bpermuteU #-}
bpermuteU es is = unstreamU (bpermuteUS es (streamU is))

mbpermuteU:: (UA e, UA d) => (e -> d) -> UArr e -> UArr Int -> UArr d
{-# INLINE_STREAM mbpermuteU #-}
mbpermuteU f es is  = unstreamU (mbpermuteUS f es (streamU is))



bpermuteUS :: UA e => UArr e -> Stream Int -> Stream e
{-# INLINE_STREAM bpermuteUS #-}
bpermuteUS !a s = mapS (a!:) s

mbpermuteUS:: (UA e, UA d) => (e -> d) -> UArr e -> Stream Int -> Stream d
{-# INLINE_STREAM mbpermuteUS #-}
mbpermuteUS f !a = mapS (f . (a!:))

-- |Default back permute
--
-- * The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
-- * All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
--
bpermuteDftU :: UA e
	     => Int			        -- ^ length of result array
	     -> (Int -> e)		        -- ^ initialiser function
	     -> UArr (Int :*: e)		-- ^ index-value pairs
	     -> UArr e
{-# INLINE_U bpermuteDftU #-}
bpermuteDftU n init = updateU (mapU init . enumFromToU 0 $ n-1)

atomicUpdateMU :: UA e => MUArr e s -> UArr (Int :*: e) -> ST s ()
{-# INLINE_U atomicUpdateMU #-}
atomicUpdateMU marr upd = updateM writeMU marr (streamU upd)

updateM :: UA e => (MUArr e s -> Int -> e -> ST s ())
                -> MUArr e s -> Stream (Int :*: e) -> ST s ()
{-# INLINE_STREAM updateM #-}
updateM write marr (Stream next s _) = upd s
  where
    upd s = case next s of
              Done               -> return ()
              Skip s'            -> upd s'
              Yield (i :*: x) s' -> do
                                      write marr i x
                                      upd s' 

-- | Yield an array constructed by updating the first array by the
-- associations from the second array (which contains index\/value pairs).
--
updateU :: UA e => UArr e -> UArr (Int :*: e) -> UArr e
{-# INLINE_U updateU #-}
updateU arr upd = update (streamU arr) (streamU upd)

update :: UA e => Stream e -> Stream (Int :*: e) -> UArr e
{-# INLINE_STREAM update #-}
update s1@(Stream _ _ n) !s2 = newDynU n (\marr ->
  do
    i <- unstreamMU marr s1
    updateM writeMU marr s2
    return i
  )

-- |Reverse the order of elements in an array
--
reverseU :: UA e => UArr e -> UArr e
{-# INLINE_U reverseU #-}
--reverseU a = mapU (a!:) . enumFromToU 0 $ lengthU a - 1
reverseU = rev . streamU

rev :: UA e => Stream e -> UArr e
{-# INLINE_STREAM rev #-}
rev (Stream next s n) =
  runST (do
    marr <- newMU n
    i <- fill marr
    a <- unsafeFreezeAllMU marr
    return $ sliceU a i (n-i)
  )
  where
    fill marr = fill0 s n
      where
        fill0 s !i = case next s of
                       Done       -> return i
                       Skip    s' -> s' `dseq` fill0 s' i
                       Yield x s' -> s' `dseq`
                                     let i' = i-1
                                     in
                                     do
                                       writeMU marr i' x
                                       fill0 s' i'

