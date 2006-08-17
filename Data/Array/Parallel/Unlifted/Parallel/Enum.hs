-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Enum
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
-- Enum-related parallel operations on unlifted arrays
--

module Data.Array.Parallel.Unlifted.Parallel.Enum (
  enumFromToUP, enumFromThenToUP, enumFromStepLenUP
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS)
import Data.Array.Parallel.Unlifted.Flat (
  UArr, UA, enumFromStepLenU)
import Data.Array.Parallel.Unlifted.Distributed (
  mapD, scanD, zipD, splitLenD, joinD,
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
  joinD theGang . mapD theGang gen $ zipD is dlen
  where
    dlen = splitLenD theGang len
    is   = fstS (scanD theGang (+) 0 dlen)
    --
    gen (i :*: n) = enumFromStepLenU (i * delta + start) delta n

