-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Enum
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Enum-related operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Enum (
  enumFromToU, enumFromThenToU
) where

import Data.Array.Parallel.Stream (
  enumFromToS, enumFromThenToS)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Flat.Stream (
  unstreamU)

-- |Yield an enumerated array
--
-- FIXME: See comments about enumFromThenToS
enumFromToU :: (Enum e, UA e) => e -> e -> UArr e
{-# INLINE enumFromToU #-}
enumFromToU start end = unstreamU (enumFromToS start end)

-- |Yield an enumerated array using a specific step
--
-- FIXME: See comments about enumFromThenToS
enumFromThenToU :: (Enum e, UA e) => e -> e -> e -> UArr e
{-# INLINE enumFromThenToU #-}
enumFromThenToU start next end = unstreamU (enumFromThenToS start next end)

