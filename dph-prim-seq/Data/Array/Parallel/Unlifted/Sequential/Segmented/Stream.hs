-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.Stream
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Stream combinators and fusion rules for segmented unboxed arrays.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Segmented.Stream (
  streamSU, unstreamSU
) where

import Data.Array.Parallel.Stream (
  Stream, SStream(..), segmentS)
import Data.Array.Parallel.Unlifted.Sequential.Flat (
  UA, streamU, unstreamU)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.SUArr (
  SUArr, USegd,
  lengthsUSegd, lengthsToUSegd,
  segdSU, flattenSU, (>:))

streamSegd :: USegd -> Stream Int
{-# INLINE_U streamSegd #-}
streamSegd = streamU . lengthsUSegd

unstreamSegd :: Stream Int -> USegd
{-# INLINE_U unstreamSegd #-}
unstreamSegd = lengthsToUSegd . unstreamU

streamSU :: UA a => SUArr a -> SStream a
{-# INLINE_U streamSU #-}
streamSU !sa = segmentS (streamSegd (segdSU sa))
                       (streamU (flattenSU sa))

unstreamSU :: UA a => SStream a -> SUArr a
{-# INLINE_U unstreamSU #-}
unstreamSU (SStream segs vals) = unstreamSegd segs >: unstreamU vals

