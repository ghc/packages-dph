{-# LANGUAGE PArr #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel
-- Copyright   :  (c) [2006..2007] Manuel M T Chakravarty & Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
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

--  ++++ !!!FIXME: THIS IS CURRENTLY ONLY A STUB FILE ++++

{- 		Comments about library structure


Data.Array.Parallel.Lifted
	... the final user types ...

Data.Array.Parallel.Unlifted
  Re-exports Data.Array.Parallel.Unlifted.Parallel
	     Data.Array.Parallel.Unlifted.Distributed
	     Data.Array.Parallel.Unlifted.Segmented
	     Data.Array.Parallel.Unlifted.Flat

Data.Array.Parallel.Unlifted.Parallel
  Parallel operations over UArrs.  No new data types!
  These operations each
	- convert the incoming (UArr t) to a Dist (UArr t)
	- run the operation in parallel using a gang
	- convert the result back to a (UArr t')
  Plus fusion rules, of course!

Data.Array.Parallel.Unlifted.Distributed
  Logically: type Dist a = Array GangMember a
	That is, one 'a' per gang member
  Mutable version: MDist

  Element types: unboxed, and products, (), 
		 *and* UArr, SUArr, Segd.
 
Data.Array.Parallel.Unlifted.Segmented
  Provides SUArr, which are segmented UArrs with exactly one nesting level
  Logically: type SUArr a = UArr (UArr a)
  Element types: unboxed, and products and ().

  Again, a mutable version MSUArr is defined internally.

Data.Array.Parallel.Unlifted.Flat
  Provides immutable (UArr) arrays of unboxed
  values, and strict products thereof.

  Simply lifts BUArr to work over strict products (incl unit).
  Internally to Flat, we also define mutable (MUArr) arrays, 
  but they aren't exported.

	.UArr: representation
	.Loop: main loop combinator over UArr, loopU
	.Fusion: fusion rules for loopU
	.Combinators: instantiate loopU (to get fold, scan, map etc)
  
  The exported maps and folds over these arrays are 
  purely sequential

Data.Array.Parallel.Arr.BUArr
  Arrays of *unboxed* values, immutable (BUArr t) and mutable (MBUArr
  t), indexed by Int.  Supports slicing (= sub-segment) operations.

  ToDo: combine with UArray and STUArray? (But they are indexed by Ix.)

  ToDo: a common pattern (e.g. when filtering) is that we allocate a
  big mutable array, *partially* fill it, and then want to shrink to
  fixed size when freezing it. (This is needed in Don's ByteString
  library too.)


Data.Array.Parallel.Base.Fusion
  Specialised combining functions that specialise loopU to be map, fold, etc
  They are also useful for loopBU; hence not in Unlifted.Flat
  
Data.Array.Parallel.Arr.BBArr
  Similar to BUArr, but for strict, boxed values.  
  Representation: Array, STArray.
  Main application: array of UArrs in Distibuted.Types

Data.Array.Parallel.Base.Hyperstrict
  A type is "hyperstrict" (in class HS) if evaluating it to WHNF
  produces a result that has no thunks.  It has no methods.  It stops
  you calling a function that would run slowly because of shared 
  thunks in a parallel setting.
-}

module Data.Array.Parallel (
  -- [::],		-- Built-in syntax

  emptyP, singletonP, replicateP, lengthP,
  (+:+), concatP,
  mapP, filterP, combineP, zipP, unzipP,
) where

import GHC.PArr
import Data.Array.Parallel.Prelude.Base.PArr

