-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Permute
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
--  Permutations of segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Segmented.Permute (
  bpermuteSU, bpermuteSU'
) where

import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr, bpermuteU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr, segdSU, (>:))
import Data.Array.Parallel.Unlifted.Segmented.Basics (
  concatSU)

bpermuteSU' :: UA e => UArr e -> SUArr Int -> SUArr e
{-# INLINE_U bpermuteSU' #-}
bpermuteSU' es is = segdSU is >: bpermuteU es (concatSU is)

-- |Segmented back permute
--
bpermuteSU :: UA e => SUArr e -> SUArr Int -> SUArr e
{-# INLINE_U bpermuteSU #-}
bpermuteSU as = error "Not implemented: bpermuteSU"
{-
bpermuteSU as = loopArrS . loopSU extract nextOff 0
	        where
		  (segd :*: a) = flattenSU as
		  psum	       = psumUS segd
		  --
	          extract off i = (off :*: (JustS $ a!:(off + i)))
		  --
		  nextOff _ segi = (psum !: (segi + 1) :*: 
				    (NothingS::MaybeS ()))
-}

