-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Stream
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Stream combinators and fusion rules for segmented unboxed arrays.
--

module Data.Array.Parallel.Unlifted.Segmented.Stream (
  streamSU, unstreamSU
) where

import Data.Array.Parallel.Stream (
  Stream, SStream(..), segmentS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, streamU, unstreamU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, USegd,
  lengthsUSegd, toUSegd,
  segdSU, flattenSU, (>:))

streamSegd :: USegd -> Stream Int
{-# INLINE streamSegd #-}
streamSegd = streamU . lengthsUSegd

unstreamSegd :: Stream Int -> USegd
{-# INLINE unstreamSegd #-}
unstreamSegd = toUSegd . unstreamU

streamSU :: UA a => SUArr a -> SStream a
{-# INLINE streamSU #-}
streamSU sa = segmentS (streamSegd (segdSU sa))
                       (streamU (flattenSU sa))

unstreamSU :: UA a => SStream a -> SUArr a
{-# INLINE unstreamSU #-}
unstreamSU (SStream segs vals) = unstreamSegd segs >: unstreamU vals

