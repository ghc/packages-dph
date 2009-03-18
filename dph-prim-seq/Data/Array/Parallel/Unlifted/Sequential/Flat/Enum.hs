-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Enum
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
--  Enum-related operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Enum (
  enumFromToU, enumFromThenToU, enumFromStepLenU, enumFromToEachU, enumFromStepLenEachU
) where

import Data.Array.Parallel.Base (
  (:*:))
import Data.Array.Parallel.Stream (
  enumFromToS, enumFromThenToS, enumFromStepLenS, enumFromToEachS, enumFromStepLenEachS)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  unstreamU, streamU)
import Data.Array.Parallel.Unlifted.Sequential.Flat.Sums (
  sumU)

-- |Yield an enumerated array
--
-- FIXME: See comments about enumFromThenToS
enumFromToU :: Int -> Int -> UArr Int
{-# INLINE_U enumFromToU #-}
enumFromToU start end = unstreamU (enumFromToS start end)

-- |Yield an enumerated array using a specific step
--
-- FIXME: See comments about enumFromThenToS
enumFromThenToU :: Int -> Int -> Int -> UArr Int
{-# INLINE_U enumFromThenToU #-}
enumFromThenToU start next end = unstreamU (enumFromThenToS start next end)

enumFromStepLenU :: Int -> Int -> Int -> UArr Int
{-# INLINE_U enumFromStepLenU #-}
enumFromStepLenU s d n = unstreamU (enumFromStepLenS s d n)

enumFromToEachU :: Int -> UArr (Int :*: Int) -> UArr Int
{-# INLINE_U enumFromToEachU #-}
enumFromToEachU n = unstreamU . enumFromToEachS n . streamU

enumFromStepLenEachU :: Int -> UArr (Int :*: Int :*: Int) -> UArr Int
{-# INLINE_U enumFromStepLenEachU #-}
enumFromStepLenEachU len = unstreamU . enumFromStepLenEachS len . streamU 




