{-# OPTIONS_GHC -fparr #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel
-- Copyright   :  (c) 2006 Manuel M T Chakravarty
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- User level interface of parallel arrays.
--
-- Description ---------------------------------------------------------------
--
-- Language: Haskell 98 plus parallel arrays
--
-- The semantic difference between standard Haskell arrays (aka "lazy
-- arrays") and parallel arrays (aka "strict arrays") is that the evaluation
-- of two different elements of a lazy array is independent, whereas in a
-- strict array either non or all elements are evaluated.  In other words,
-- when a parallel array is evaluated to WHNF, all its elements will be
-- evaluated to WHNF.  The name parallel array indicates that all array
-- elements may, in general, be evaluated to WHNF in parallel without any
-- need to resort to speculative evaluation.  This parallel evaluation
-- semantics is also beneficial in the sequential case, as it facilitates
-- loop-based array processing as known from classic array-based languages,
-- such as Fortran.
--
-- The interface of this module is essentially a variant of the list
-- component of the Prelude, but also includes some functions (such as
-- permutations) that are not provided for lists.  The following list
-- operations are not supported on parallel arrays, as they would require the
-- availability of infinite parallel arrays: `iterate', `repeat', and `cycle'.
--

-- *** !!!FIXME: THIS IS CURRENTLY ONLY A STUB FILE **

module Data.Array.Parallel (
  -- [::],		-- Built-in syntax
) where
