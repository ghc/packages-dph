{-# LANGUAGE PArr #-}
{-# GHC_OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module PrimesVect (primesVect)

where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int 


import qualified Prelude


primesVect:: Int -> PArray Int
primesVect n = toPArrayP (primesVect' n)

primesVect':: Int -> [:Int:]
primesVect' n  
  | n == 1    = emptyP 
  | n == 2    = singletonP 2
  | otherwise = sps +:+ [: i | i <- enumFromToP (sq+1) n, notMultiple sps i:] 
  where

    sps = primesVect' sq
    sq =  intSquareRoot n

    notMultiple :: [:Int:] -> Int -> Bool
    notMultiple ps i = andP [: mod i p /= 0 | p <- ps:]
