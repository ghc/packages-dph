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
  enumFromToUP, enumFromThenToUP, enumFromStepLenUP, enumFromToEachUP, enumFromStepLenEachUP    
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS, uncurryS)
import Data.Array.Parallel.Unlifted.Sequential (
  UArr, UA, enumFromStepLenU, enumFromToEachU, enumFromStepLenEachU)
import Data.Array.Parallel.Unlifted.Distributed (
  mapD, scanD, zipD, splitLenD, joinD, splitD, balanced, unbalanced,
  theGang)
import Data.Array.Parallel.Unlifted.Parallel.Combinators (
  mapUP)

enumFromToUP :: (UA a, Enum a) => a -> a -> UArr a
{-# INLINE enumFromToUP #-}
enumFromToUP start end = enumFromThenToUP start (succ start) end

enumFromThenToUP :: (UA a, Enum a) => a -> a -> a -> UArr a
{-# INLINE enumFromThenToUP #-}
enumFromThenToUP start next end =
  mapUP toEnum (enumFromStepLenUP start' delta len)
  where
    start' = fromEnum start
    next'  = fromEnum next
    end'   = fromEnum end
    delta  = next' - start'
    len    = abs (end' - start' + delta) `div` abs delta

enumFromStepLenUP :: Int -> Int -> Int -> UArr Int
{-# INLINE enumFromStepLenUP #-}
enumFromStepLenUP start delta len =
  joinD theGang balanced . mapD theGang gen $ zipD is dlen
  where
    dlen = splitLenD theGang len
    is   = fstS (scanD theGang (+) 0 dlen)
    --
    gen (i :*: n) = enumFromStepLenU (i * delta + start) delta n

enumFromToEachUP :: Int -> UArr (Int :*: Int) -> UArr Int
{-# INLINE enumFromToEachUP #-}
enumFromToEachUP n inds =  
  joinD theGang unbalanced $ mapD theGang (uncurryS enumFromToEachU) $ 
    zipD  (splitLenD theGang n) (splitD theGang balanced inds)

enumFromStepLenEachUP :: Int -> UArr (Int :*: Int :*: Int) -> UArr Int
{-# INLINE enumFromStepLenEachUP #-}
enumFromStepLenEachUP n inds =  
  joinD theGang unbalanced $ mapD theGang (uncurryS enumFromStepLenEachU) $ 
    zipD  (splitLenD theGang n) (splitD theGang balanced inds)
