{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- | User level interface of parallel arrays.
--
--  The semantic difference between standard Haskell arrays (aka "lazy
--  arrays") and parallel arrays (aka "strict arrays") is that the evaluation
--  of two different elements of a lazy array is independent, whereas in a
--  strict array either non or all elements are evaluated.
-- 
--  In other words, when a parallel array is evaluated to WHNF, all its elements
--  will be evaluated to WHNF. The name parallel array indicates that all array
--  elements may, in general, be evaluated to WHNF in parallel without any
--  need to resort to speculative evaluation.  This parallel evaluation
--  semantics is also beneficial in the sequential case, as it facilitates
--  loop-based array processing as known from classic array-based languages,
--  such as Fortran.
--
--  The interface of this module is essentially a variant of the list
--  component of the Prelude, but also includes some functions (such as
--  permutations) that are not provided for lists.  The following list of
--  operations are not supported on parallel arrays, as they would require the
--  infinite parallel arrays: `iterate', `repeat', and `cycle'.
-- 
--  /WARNING:/ In the current implementation, the functionality provided in
--  this module is tied to the vectoriser pass of GHC invoked by passing the
--  `-fvectorise` option.  Without vectorisation these functions will not work
--  at all!
--
module Data.Array.Parallel (
        -- [::]         -- Built-in syntax
        
        -- * Constructors
        emptyP,
        singletonP,
        replicateP,
        
        -- * Projections
        (!:)
        
        -- * Traversals
)
where
import Data.Array.Parallel.VectDepend
-- IMPORTANT: see Note [Vectoriser dependencies] in the same module

import Data.Array.Parallel.Lifted
import Data.Array.Parallel.Prelude.Int  (Int)
import qualified Prelude        as P

nope    = P.error "Data.Array.Parallel: can't use unvectorised definition"
{-# NOVECTORISE nope #-}

-- Constructors ---------------------------------------------------------------
-- | O(1). Construct an empty array, with no elements.
emptyP :: [:a:]
emptyP          = nope
{-# NOINLINE  emptyP #-}
{-# VECTORISE emptyP = emptyPP #-}


-- | O(1). Construct an array with a single element.
singletonP :: a -> [:a:]
singletonP      = nope
{-# NOINLINE  singletonP #-}
{-# VECTORISE singletonP = singletonPP #-}


-- | O(1). Construct an array by replicating the given element some number of times.
replicateP :: Int -> a -> [:a:]
replicateP      = nope
{-# NOINLINE  replicateP #-}
{-# VECTORISE replicateP = replicatePP #-}



-- Projections ----------------------------------------------------------------
(!:) :: [:a:] -> Int -> a
(!:)            = nope
{-# NOINLINE  (!:) #-}
{-# VECTORISE (!:) = indexPP #-}


-- Traversals -----------------------------------------------------------------
mapP :: (a -> b) -> [:a:] -> [:b:]
mapP            = nope
{-# NOINLINE  mapP #-}
{-# VECTORISE mapP = mapPP #-}
