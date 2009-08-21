-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Stream
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Stream combinators and fusion rules for flat unboxed arrays.
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.Stream (
  streamU, unstreamU, unstreamMU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, sndS, ST, Rebox(..))
import Data.Array.Parallel.Stream (
  Step(..), Stream(..), mapS, zipS)
import Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (
  UArr, MUArr, UA, indexU, lengthU, zipU, fstU, sndU, newDynU, writeMU)

-- | Generate a stream from an array, from left to right
--
streamU :: UA a => UArr a -> Stream a
{-# INLINE_STREAM streamU #-}
streamU !arr = Stream next 0 n
  where
    n = lengthU arr
    {-# INLINE next #-}
    next i | i == n    = Done
           | otherwise = Yield (arr `indexU` i) (i+1)

-- | Create an array from a stream, filling it from left to right
--
unstreamU :: UA a => Stream a -> UArr a
{-# INLINE_STREAM unstreamU #-}
unstreamU st@(Stream next s n) = newDynU n (\marr -> unstreamMU marr st)

-- | Fill a mutable array from a stream from left to right and yield
-- the number of elements written.
--
unstreamMU :: UA a => MUArr a s -> Stream a -> ST s Int
{-# INLINE_U unstreamMU #-}
unstreamMU marr (Stream next s n) = fill s 0
  where
    fill s i = i `seq`
               case next s of
                 Done       -> return i
                 Skip s'    -> {- s' `dseq` -} fill s' i
                 Yield x s' -> {- s' `dseq` -}
                               do
                                 writeMU marr i x
                                 fill s' (i+1)




-- | Fusion rules
-- --------------

-- The main fusion rule

{-# RULES  -- -} (for font-locking)

"streamU/unstreamU" forall s.
  streamU (unstreamU s) = s
 
  #-}

-- Zip fusion
--
-- NB: We do not separate rules for zip3U etc. because these are implemented
-- in terms of zipU

{-# RULES  -- -} (for font-locking)

"streamU/zipU" forall a1 a2.
  streamU (zipU a1 a2) = zipS (streamU a1) (streamU a2)

"fstU/unstreamU" forall s.
  fstU (unstreamU s) = unstreamU (mapS fstS s)
"sndU/unstreamU" forall s.
  sndU (unstreamU s) = unstreamU (mapS sndS s)

  #-}

