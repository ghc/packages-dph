-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Standard combinators for flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators (
  mapU, 
  filterU, 
  packU,
  foldlU, foldl1U, foldl1MaybeU, {-foldrU, foldr1U,-}
  foldU,  fold1U,  fold1MaybeU,
  scanlU, scanl1U, {-scanrU, scanr1U,-} scanU, scan1U,
  scanResU,
  mapAccumLU,
  zipU, zip3U, unzipU, unzip3U, fstU, sndU,
  zipWithU, zipWith3U, 
  combineU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), checkNotEmpty, checkEq, sndS, Rebox(..), ST, runST)
import Data.Array.Parallel.Stream (
  Step(..), Stream(..),
  mapS, filterS, foldS, fold1MaybeS, scan1S, scanS, mapAccumS,
  zipWithS, zipWith3S, combineS)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr, MUArr,
  writeMU, newDynResU,
  zipU, unzipU, fstU, sndU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  streamU, unstreamU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Basics (
  lengthU, (!:))
import Data.Array.Parallel.Unlifted.Sequential.Flat.Subarrays (
  sliceU)

import Debug.Trace

here s = "Data.Array.Parallel.Unlifted.Sequential.Flat.Combinators." ++ s

-- |Map a function over an array
--
mapU :: (UA e, UA e') => (e -> e') -> UArr e -> UArr e'
{-# INLINE_U mapU #-}
mapU f = unstreamU . mapS f . streamU

-- |Extract all elements from an array that meet the given predicate
--
filterU :: UA e => (e -> Bool) -> UArr e -> UArr e 
{-# INLINE_U filterU #-}
filterU p = unstreamU . filterS p . streamU

-- |Extract all elements from an array according to a given flag array
-- 
packU:: UA e => UArr e -> UArr Bool -> UArr e
{-# INLINE_U packU #-}
packU xs = fstU . filterU sndS . zipU xs



-- |Array reduction proceeding from the left
--
foldlU :: UA a => (b -> a -> b) -> b -> UArr a -> b
{-# INLINE_U foldlU #-}
foldlU f z xs = foldS f z (streamU xs)

-- |Array reduction proceeding from the left for non-empty arrays
--
-- FIXME: Rewrite for 'Stream's.
--
foldl1U :: UA a => (a -> a -> a) -> UArr a -> a
{-# INLINE_U foldl1U #-}
foldl1U f arr = checkNotEmpty (here "foldl1U") (lengthU arr) $
                foldlU f (arr !: 0) (sliceU arr 1 (lengthU arr - 1))

foldl1MaybeU :: UA a => (a -> a -> a) -> UArr a -> MaybeS a
{-# INLINE_U foldl1MaybeU #-}
foldl1MaybeU f = fold1MaybeS f . streamU

-- |Array reduction that requires an associative combination function with its
-- unit
--
foldU :: UA a => (a -> a -> a) -> a -> UArr a -> a
{-# INLINE_U foldU #-}
foldU = foldlU

fold1MaybeU :: UA a => (a -> a -> a) -> UArr a -> MaybeS a
{-# INLINE_U fold1MaybeU #-}
fold1MaybeU = foldl1MaybeU

-- |Reduction of a non-empty array which requires an associative combination
-- function
--
fold1U :: UA a => (a -> a -> a) -> UArr a -> a
{-# INLINE_U fold1U #-}
fold1U = foldl1U

-- |Prefix scan proceedings from left to right
--
scanlU :: (UA a, UA b) => (b -> a -> b) -> b -> UArr a -> UArr b
{-# INLINE_U scanlU #-}
scanlU f z = unstreamU . scanS f z . streamU

-- |Prefix scan of a non-empty array proceeding from left to right
--
scanl1U :: UA a => (a -> a -> a) -> UArr a -> UArr a
{-# INLINE_U scanl1U #-}
scanl1U f arr = checkNotEmpty (here "scanl1U") (lengthU arr) $
                unstreamU (scan1S f (streamU arr))

-- |Prefix scan proceeding from left to right that needs an associative
-- combination function with its unit
--
scanU :: UA a => (a -> a -> a) -> a -> UArr a -> UArr a
{-# INLINE_U scanU #-}
scanU = scanlU

-- |Prefix scan of a non-empty array proceeding from left to right that needs
-- an associative combination function
--
scan1U :: UA a => (a -> a -> a) -> UArr a -> UArr a
{-# INLINE_U scan1U #-}
scan1U = scanl1U

scanResU :: UA a => (a -> a -> a) -> a -> UArr a -> UArr a :*: a
{-# INLINE_U scanResU #-}
scanResU f z = unstreamScan f z . streamU

unstreamScan :: UA a => (a -> a -> a) -> a -> Stream a -> UArr a :*: a
{-# INLINE_STREAM unstreamScan #-}
unstreamScan f z st@(Stream _ _ n)
  = newDynResU n (\marr -> unstreamScanM marr f z st)

unstreamScanM :: UA a => MUArr a s -> (a -> a -> a) -> a -> Stream a
                      -> ST s (Int :*: a)
{-# INLINE_U unstreamScanM #-}
unstreamScanM marr f z (Stream next s n) = fill s z 0
  where
    fill s !z !i = case next s of
                     Done       -> return (i :*: z)
                     Skip    s' -> s' `dseq` fill s' z i
                     Yield x s' -> s' `dseq`
                                   do
                                     writeMU marr i z
                                     fill s' (f z x) (i+1)

-- |Accumulating map from left to right. Does not return the accumulator.
--
-- FIXME: Naming inconsistent with lists.
--
mapAccumLU :: (UA a, UA b) => (c -> a -> c :*: b) -> c -> UArr a -> UArr b
{-# INLINE_U mapAccumLU #-}
mapAccumLU f z = unstreamU . mapAccumS f z . streamU

-- zipU is re-exported from UArr

-- |
--
zip3U :: (UA e1, UA e2, UA e3) 
      => UArr e1 -> UArr e2 -> UArr e3 -> UArr (e1 :*: e2 :*: e3)
{-# INLINE_U zip3U #-}
zip3U a1 a2 a3 = (a1 `zipU` a2) `zipU` a3

-- |
zipWithU :: (UA a, UA b, UA c) 
	 => (a -> b -> c) -> UArr a -> UArr b -> UArr c
{-# INLINE_U zipWithU #-}
zipWithU f a1 a2 = unstreamU (zipWithS f (streamU a1) (streamU a2))

-- |
zipWith3U :: (UA a, UA b, UA c, UA d) 
          => (a -> b -> c -> d) -> UArr a -> UArr b -> UArr c -> UArr d
{-# INLINE_U zipWith3U #-}
zipWith3U f a1 a2 a3 = unstreamU (zipWith3S f (streamU a1)
                                              (streamU a2)
                                              (streamU a3))

-- unzipU is re-exported from UArr

-- |
unzip3U :: (UA e1, UA e2, UA e3) 
	=> UArr (e1 :*: e2 :*: e3) -> (UArr e1 :*: UArr e2 :*: UArr e3)
{-# INLINE_U unzip3U #-}
unzip3U a = let (a12 :*: a3) = unzipU a
		(a1  :*: a2) = unzipU a12
	    in
	    (a1 :*: a2 :*: a3)

-- fstU and sndU reexported from UArr
-- |
combineU :: UA a
	 => UArr Bool -> UArr a -> UArr a -> UArr a
{-# INLINE_U combineU #-}
combineU f a1 a2 = checkEq (here "combineU") 
     ("flag length not equal to sum of arg length")
     (lengthU f) (lengthU a1 + lengthU a2) $ 
--  trace ("combineU:\n\t"  ++ show (lengthU f)  ++ "\n\t" ++ show (lengthU a1) ++ "\n\t" ++ show (lengthU a2) ++ "\n")
  unstreamU (combineS (streamU f) (streamU a1) (streamU a2))

