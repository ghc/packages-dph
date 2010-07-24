-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Enum
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Enum-related parallel operations on unlifted arrays
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.Enum (
  enumFromToUP, enumFromThenToUP, enumFromStepLenUP, enumFromStepLenEachUP    
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, uncurryS)
import Data.Array.Parallel.Unlifted.Sequential (
  UArr, UA, enumFromStepLenU, enumFromToEachU, enumFromStepLenEachU, sndU, sumU,
  zipU, unzipU)
import Data.Array.Parallel.Unlifted.Distributed (
  mapD, scanD, zipD, splitLenIdxD, joinD, splitD, balanced, unbalanced,
  theGang)
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP)

import GHC.Base ( divInt )

delay_inline :: a -> a
{-# INLINE [0] delay_inline #-}
delay_inline x = x

enumFromToUP :: (UA a, Enum a) => a -> a -> UArr a
{-# INLINE enumFromToUP #-}
enumFromToUP start end = mapUP toEnum (enumFromStepLenUP start' 1 len)
  where
    start' = fromEnum start
    end'   = fromEnum end
    len    = delay_inline max (end' - start' + 1) 0

enumFromThenToUP :: (UA a, Enum a) => a -> a -> a -> UArr a
{-# INLINE enumFromThenToUP #-}
enumFromThenToUP start next end =
  mapUP toEnum (enumFromStepLenUP start' delta len)
  where
    start' = fromEnum start
    next'  = fromEnum next
    end'   = fromEnum end
    delta  = next' - start'
    len    = abs (end' - start' + delta) `divInt` abs delta

enumFromStepLenUP :: Int -> Int -> Int -> UArr Int
{-# INLINE enumFromStepLenUP #-}
enumFromStepLenUP start delta len =
  -- joinD theGang balanced . mapD theGang gen $ zipD is dlen
  joinD theGang balanced
  (mapD theGang gen
  (splitLenIdxD theGang len))
  where
    gen (n,i) = enumFromStepLenU (i * delta + start) delta n
    --dlen = splitLenD theGang len
    --is   = fstS (scanD theGang (+) 0 dlen)
    --
    --gen (i :*: n) = enumFromStepLenU (i * delta + start) delta n

enumFromStepLenEachUP :: Int -> UArr Int -> UArr Int -> UArr Int -> UArr Int
{-# INLINE enumFromStepLenEachUP #-}
enumFromStepLenEachUP n starts steps lens
  = joinD theGang unbalanced
  $ mapD theGang enum
  $ splitD theGang unbalanced (zipU (zipU starts steps) lens)
  where
    enum ps = let (qs, llens) = unzipU ps
                  (lstarts, lsteps) = unzipU qs
              in enumFromStepLenEachU (sumU llens) lstarts lsteps llens

