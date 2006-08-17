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
  enumFromStepLenUP
) where

import Data.Array.Parallel.Base (
  (:*:)(..), fstS)
import Data.Array.Parallel.Unlifted.Flat (
  UArr, enumFromStepLenU)
import Data.Array.Parallel.Unlifted.Distributed (
  mapD, scanD, zipD, splitLenD, joinD,
  theGang)

enumFromStepLenUP :: Int -> Int -> Int -> UArr Int
{-# INLINE enumFromStepLenUP #-}
enumFromStepLenUP start delta len =
  joinD theGang . mapD theGang gen $ zipD is dlen
  where
    dlen = splitLenD theGang len
    is   = fstS (scanD theGang (+) 0 dlen)
    --
    gen (i :*: n) = enumFromStepLenU (i * delta + start) delta n

