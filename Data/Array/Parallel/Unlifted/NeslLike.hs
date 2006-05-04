-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Listlike
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Unlifted array versions of Nesl-like combinators.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.NeslLike (
  -- * Nesl-like combinators
  flattenSU, (>:), segmentU, toU, toSU, fromU, emptyU, extractU, sliceU,
  permuteU, permuteMU, bpermuteU, bpermuteSU, bpermuteDftU, {-crossU, indexOfU -}
) where

import Data.Array.Parallel.Unlifted.Flat.NeslLike
import Data.Array.Parallel.Unlifted.Segmented.NeslLike

