{-# OPTIONS_GHC -fvectorise #-}

-- |This module sets up the basic vectorisation map for vectorising the DPH Prelude.

module Data.Array.Parallel.Prelude.Base
  ( PArr
  -- , ()
  , Bool(..)
  , Ordering(..)
  , Word8, Int
  , Float, Double
  , Eq(..), Ord(..)
  , Show
  , Num(..)
  )
where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Closure

import Data.Word (Word8)


-- internal types
{-# VECTORISE SCALAR type PArr = PArray #-}
{-# VECTORISE SCALAR type PArray = PArray #-}
{-# VECTORISE SCALAR type (->) = (:->) #-}

-- vectorised versions of types from the standard Prelude
{-# VECTORISE type ()       = () #-}
{-# VECTORISE type Bool     = Bool #-}
{-# VECTORISE type Ordering = Ordering #-}
{-# VECTORISE SCALAR type Word8 #-}
{-# VECTORISE SCALAR type Int #-}
{-# VECTORISE SCALAR type Float #-}
{-# VECTORISE SCALAR type Double #-}

-- FIXME: currently a fake definition to allow 'Integer' in SCALAR class instances
{-# VECTORISE SCALAR type Integer #-}

-- vectorised versions of type classes from the standard Prelude
{-# VECTORISE class Eq #-}
{-# VECTORISE class Ord #-}
{-# VECTORISE class Show #-}  -- only to facilitate 'Num', no vectorised instances provided
{-# VECTORISE class Num #-}
