module H98
where

import Data.Array

primes :: Int -> [Int]
{-# NOINLINE primes #-}
primes n 
  | n <= 2    = []
  | otherwise = 
    let
      sqrPrimes = primes (ceiling (sqrt (fromIntegral n)))
      sieves    = concat
		    [[2 * p, 3 * p..n - 1] | p <- sqrPrimes]
      sieves'   = zip sieves (repeat False)
      flags     = accumArray (&&) True (0, n - 1) sieves'
    in
    drop 2 (filter (flags!) [0..n - 1])

