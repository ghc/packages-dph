module PrimPar
--  
--  TODO:
--     bpermuteDftU which does most of the work is still sequential

where

import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted


import Debug.Trace 
primes :: Int -> UArr Int
{-# NOINLINE primes #-}
primes n 
  | n <= 2    = emptyU
  | otherwise = 
    let
      sqrPrimes = primes (ceiling (sqrt (fromIntegral n)))
      sieves    = concatSU $
		    enumFromThenToSUP
		      (mapUP (*2) sqrPrimes)
		      (mapUP (*3) sqrPrimes)
		      (replicateUP (lengthU sqrPrimes) (n - 1))
      sieves'   = zipU sieves (replicateUP (lengthU sieves) False)
      flags     = bpermuteDftU n (const True) sieves'
      arg       = flags `seq` (filterUP (flags!:) (enumFromToUP 0 (n - 1)))
    in
    dropUP 2 arg 


