-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Basics
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Basic operations on flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Basics (
  lengthU, nullU, emptyU, unitsU, replicateU, (!:), (+:+),
  enumFromToU, enumFromThenToU,
  toU, fromU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..))
import Data.Array.Parallel.Base.Fusion (
  noAL, mapEFL, loopArr)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr, lengthU, indexU, newU)
import Data.Array.Parallel.Unlifted.Flat.Loop (
  unitsU, loopU)

infixl 9 !:
infixr 5 +:+

-- lengthU is reexported from UArr

-- |Test whether the given array is empty
--
nullU :: UA e => UArr e -> Bool
nullU  = (== 0) . lengthU

-- |Yield an empty array
--
emptyU :: UA e => UArr e
emptyU = newU 0 (const $ return ())

-- unitsU is reexported from Loop

-- |Yield an array where all elements contain the same value
--
replicateU :: UA e => Int -> e -> UArr e
{-# INLINE replicateU #-}
replicateU n e = loopArr . loopU (mapEFL $ const e) noAL $ unitsU n

-- |Array indexing
--
(!:) :: UA e => UArr e -> Int -> e
(!:) = indexU

-- |Concatenate two arrays
--
(+:+) :: UA e => UArr e -> UArr e -> UArr e
{-# INLINE (+:+) #-}
a1 +:+ a2 = loopArr $ loopU extract 0 (unitsU len)
  where
    len1 = lengthU a1
    len  = len1 + lengthU a2
    --
    extract i _ = (i + 1 :*: 
		   (JustS $ if i < len1 then a1!:i else a2!:(i - len1)))

-- |Enumeration functions
-- ----------------------

-- |Yield an enumerated array
--
-- FIXME: This doesn't really work at the moment (try enumFromToU 0 0)
--        and in fact, I'm not sure what its semantics should be.
enumFromToU :: (Enum e, UA e) => e -> e -> UArr e
{-# INLINE enumFromToU #-}
enumFromToU start = enumFromThenToU start (succ start)

-- |Yield an enumerated array using a specific step
--
-- FIXME: See comment about enumFromToU.
enumFromThenToU :: (Enum e, UA e) => e -> e -> e -> UArr e
{-# INLINE enumFromThenToU #-}
enumFromThenToU start next end = 
  loopArr $ loopU step start' (unitsU len)
  where
    start' = fromEnum start
    next'  = fromEnum next
    end'   = fromEnum end
    delta  = next' - start'
    len    = abs (end' - start' + delta) `div` (abs delta)
    --
    step x _ = (x + delta :*: (JustS $ toEnum x))


-- |Conversion
-- -----------

-- |Turn a list into a parallel array
--
toU :: UA e => [e] -> UArr e
{-# INLINE toU #-}
toU l = 
  loopArr $ 
    loopU (\(x:xs) (_::()) -> (xs :*: JustS x)) l (unitsU (length l))

-- |Collect the elements of a parallel array in a list
--
fromU :: UA e => UArr e -> [e]
{-# INLINE fromU #-}
fromU a = [a!:i | i <- [0..lengthU a - 1]]

