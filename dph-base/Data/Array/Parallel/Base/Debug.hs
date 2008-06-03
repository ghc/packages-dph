-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--               (c) [2006..2007] Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
--
-- Debugging infrastructure for the parallel arrays library

module Data.Array.Parallel.Base.Debug (
    check
  , checkCritical
  , checkLen
  , checkEq
  , checkNotEmpty
  , uninitialised
) where

import Data.Array.Parallel.Base.Config  (debug, debugCritical)

outOfBounds :: String -> Int -> Int -> a
outOfBounds loc n i = error $ loc ++ ": Out of bounds (size = "
                              ++ show n ++ "; index = " ++ show i ++ ")"

-- Check that the second integer is greater or equal to `0' and less than the
-- first integer
--
check :: String -> Int -> Int -> a -> a
{-# INLINE check #-}
check loc n i v 
  | debug      = if (i >= 0 && i < n) then v else outOfBounds loc n i
  | otherwise  = v
-- FIXME: Interestingly, ghc seems not to be able to optimise this if we test
--	  for `not debug' (it doesn't inline the `not'...)

-- Check that the second integer is greater or equal to `0' and less than the
-- first integer; this version is used to check operations that could corrupt
-- the heap
--
checkCritical :: String -> Int -> Int -> a -> a
{-# INLINE checkCritical #-}
checkCritical loc n i v 
  | debugCritical = if (i >= 0 && i < n) then v else outOfBounds loc n i
  | otherwise     = v

-- Check that the second integer is greater or equal to `0' and less or equal
-- than the first integer
--
checkLen :: String -> Int -> Int -> a -> a
{-# INLINE checkLen #-}
checkLen loc n i v 
  | debug      = if (i >= 0 && i <= n) then v else outOfBounds loc n i
  | otherwise  = v

checkEq :: (Eq a, Show a) => String -> String -> a -> a -> b -> b
checkEq loc msg x y v
  | debug     = if x == y then v else err
  | otherwise = v
  where
    err = error $ loc ++ ": " ++ msg
                  ++ " (first = " ++ show x
                  ++ "; second = " ++ show y ++ ")"

checkNotEmpty :: String -> Int -> a -> a
checkNotEmpty loc n v
  | debug     = if n /= 0 then v else err
  | otherwise = v
  where
    err = error $ loc ++ ": Empty array"

uninitialised :: String -> a
uninitialised loc = error $ loc ++ ": Touched an uninitialised value"

