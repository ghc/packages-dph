{-# OPTIONS_GHC -fvectorise #-}

-- |This module sets up the basic vectorisation map for vectorising the DPH Prelude.

module Data.Array.Parallel.Prelude.Base
  ( PArr
  -- , ()
  , Bool
  , Word8, Int
  , Float, Double
  )
where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Closure

import Data.Word (Word8)


{-# VECTORISE SCALAR type PArr = PArray #-}
{-# VECTORISE SCALAR type PArray = PArray #-}
{-# VECTORISE SCALAR type (->) = (:->) #-}

{-# VECTORISE type ()       = () #-}
{-# VECTORISE type Bool     = Bool #-}
{-# VECTORISE type Ordering = Ordering #-}
{-# VECTORISE SCALAR type Word8 #-}
{-# VECTORISE SCALAR type Int #-}
{-# VECTORISE SCALAR type Float #-}
{-# VECTORISE SCALAR type Double #-}
