module PrimSeq
where

import Data.Array.Parallel.Unlifted

primes :: Int -> UArr Int
{-# NOINLINE primes #-}
primes n 
  | n <= 2    = emptyU
  | otherwise = 
    let
      sqrPrimes = primes (ceiling (sqrt (fromIntegral n)))
      sieves    = concatSU $
		    enumFromThenToSU
		      (mapU (*2) sqrPrimes)
		      (mapU (*3) sqrPrimes)
		      (replicateU (lengthU sqrPrimes) (n - 1))
      sieves'   = zipU sieves (replicateU (lengthU sieves) False)
      flags     = bpermuteDftU n (const True) sieves'
    in
    dropU 2 (filterU (flags!:) (enumFromToU 0 (n - 1)))

