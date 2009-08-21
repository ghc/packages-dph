-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics
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
--  Basic segmented operations on unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics (
  replicateSU, replicateRSU, appendSU
) where

import Data.Array.Parallel.Stream
import Data.Array.Parallel.Unlifted.Sequential.Flat
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd

replicateSU :: UA a => USegd -> UArr a -> UArr a
{-# INLINE_U replicateSU #-}
replicateSU segd xs = unstreamU
                     (replicateEachS (elementsUSegd segd)
                     (zipS (streamU (lengthsUSegd segd)) (streamU xs)))

replicateRSU :: UA a => Int -> UArr a -> UArr a
{-# INLINE_U replicateRSU #-}
replicateRSU n xs = unstreamU
                  . replicateEachRS n
                  $ streamU xs
                  

appendSU :: UA a => USegd -> UArr a -> USegd -> UArr a -> UArr a
{-# INLINE_U appendSU #-}
appendSU xd xs yd ys = unstreamU
                     $ appendSS (streamU (lengthsUSegd xd))
                                (streamU xs)
                                (streamU (lengthsUSegd yd))
                                (streamU ys)

