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

-- | Bounds check, enabled when `debug` = `True`.
-- 
--   The first integer is the length of the array, and the second
--   is the index. The second must be greater or equal to '0' and less than the
--   first integer. If the not then `error` with the `String`.
--
check :: String -> Int -> Int -> a -> a
{-# INLINE check #-}
check loc n i v 
  | debug      = if (i >= 0 && i < n) then v else outOfBounds loc n i
  | otherwise  = v
-- FIXME: Interestingly, ghc seems not to be able to optimise this if we test
--	  for `not debug' (it doesn't inline the `not'...)


-- | Bounds check, enabled when `debugCritical` = `True`.
--
--   This version is used to check operations that could corrupt the heap.
-- 
--   The first integer is the length of the array, and the second
--   is the index. The second must be greater or equal to '0' and less than the
--   first integer. If the not then `error` with the `String`.
--
checkCritical :: String -> Int -> Int -> a -> a
{-# INLINE checkCritical #-}
checkCritical loc n i v 
  | debugCritical = if (i >= 0 && i < n) then v else outOfBounds loc n i
  | otherwise     = v


-- | Length check, enabled when `debug` = `True`.
-- 
--   Check that the second integer is greater or equal to `0' and less or equal
--   than the first integer. If the not then `error` with the `String`.
--
checkLen :: String -> Int -> Int -> a -> a
{-# INLINE checkLen #-}
checkLen loc n i v 
  | debug      = if (i >= 0 && i <= n) then v else outOfBounds loc n i
  | otherwise  = v


-- | Equality check, enabled when `debug` = `True`.
--   
--   The two `a` values must be equal, else `error`.
--
--   The first `String` gives the location of the error, and the second some helpful message.
--
checkEq :: (Eq a, Show a) => String -> String -> a -> a -> b -> b
checkEq loc msg x y v
  | debug     = if x == y then v else err
  | otherwise = v
  where
    err = error $ loc ++ ": " ++ msg
                  ++ " (first = " ++ show x
                  ++ "; second = " ++ show y ++ ")"


-- | Given an array length, check it is not zero.
checkNotEmpty :: String -> Int -> a -> a
checkNotEmpty loc n v
  | debug     = if n /= 0 then v else err
  | otherwise = v
  where
    err = error $ loc ++ ": Empty array"


-- | Throw an error saying something was not intitialised.
--   
--   The `String` must contain a helpful message saying what module the error occured in, 
--   and the possible reasons for it. If not then a puppy dies at compile time.
--
uninitialised :: String -> a
uninitialised loc = error $ loc ++ ": Touched an uninitialised value"

