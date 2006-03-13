-- Nested data parallel Sieve of Eratosthenes
--

module Main (main)
where

-- standard library
import Array
import Random
import System

-- GHC libraries
import Control.Exception (evaluate)

-- Parallel array support
import PArray
import BenchUtils

import Debug.Trace


-- Benchmarked code
-- ----------------

primes :: Int -> PArrInt
{-# NOINLINE primes #-}
primes n 
--  | n <= 2    = emptyP
  | n <= 2    = trace "emptyP" emptyP
  | otherwise = 
    let
--      sqrPrimes = primes (ceiling (sqrt (fromIntegral n)))
      sqrPrimes = primes (
		    let 
		      f = fromIntegral (trace ("n = "++show n)n)
		      c = ceiling (sqrt (trace ("f = "++show f) f))
		    in
		    trace ("c = "++show c) c)
		    
      sieves    = concatP $
--		    trace "sieves"$
-- doesn't die when the above trace is activated or when compiled with -Onot
-- should try running with bounds checks activated
		    enumFromThenToSP 
		      (mapP (*2) sqrPrimes)
		      (mapP (*3) sqrPrimes)
		      (replicateP (lengthP sqrPrimes) (n - 1))
      sieves'   = zipP sieves (replicateP (lengthP sieves) False)
      flags     = bpermuteDftP n (const True) sieves'
    in
    dropP 2 (filterP (flags!:) (enumFromToP 0 (n - 1)))


-- H98 array version for comparison
--

primes_h98 :: Int -> [Int]
{-# NOINLINE primes_h98 #-}
primes_h98 n 
  | n <= 2    = []
  | otherwise = 
    let
      sqrPrimes = primes_h98 (ceiling (sqrt (fromIntegral n)))
      sieves    = concat
		    [[2 * p, 3 * p..n - 1] | p <- sqrPrimes]
      sieves'   = zip sieves (repeat False)
      flags     = accumArray (&&) True (0, n - 1) sieves'
    in
    drop 2 (filter (flags!) [0..n - 1])


-- Benchmark infrastructure
-- ------------------------

-- Execute and time the benchmark
--
primes_bench :: Int -> IO (Integer, String)
primes_bench size = 
  timeIt ("N = " ++ show size) $
    do
      let result = primes size			-- won't be fused due...
	  sum    = sumP result			-- ...to the NOINLINE
      evaluate $ sum			        -- make sure the work is done
      return   $ show sum

-- Execute and time the H98 benchmark
--
primes_bench_h98 :: Int -> IO (Integer, String)
primes_bench_h98 size = 
  timeIt ("N = " ++ show size) $
    do
      let result = primes_h98 size		-- won't be fused due...
	  s      = sum result			-- ...to the NOINLINE
      evaluate $ s			        -- make sure the work is done
      return   $ show s

-- Main routine
--
main :: IO ()
main =
  do
    host <- getEnv "HOSTNAME"
    putStrLn "Primes Benchmark"
    putStrLn "================"

    doBenchmarks [(primes_bench_h98, "H98", "h98"),
		  (primes_bench    , "PArr", "parr")]
--		 [100000]
		 [5]
		 ("Primes up to 100,000\n\
		  \on " ++ host)
		 "primes"
