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
import Data.Array.Parallel.Unlifted
import BenchUtils

import Debug.Trace

xtrace s x = x

-- Benchmarked code
-- ----------------

primes :: Int -> UArr Int
{-# NOINLINE primes #-}
primes n 
--  | n <= 2    = emptyP
  | n <= 2    = xtrace "emptyP" emptyU
  | otherwise = 
    let
--      sqrPrimes = primes (ceiling (sqrt (fromIntegral n)))
      sqrPrimes = primes (
		    let 
		      f = fromIntegral (xtrace ("n = "++show n)n)
		      c = ceiling (sqrt (xtrace ("f = "++show f) f))
		    in
		    xtrace ("c = "++show c) c)
		    
      sieves    = concatSU $
--		    trace "sieves"$
-- doesn't die when the above trace is activated or when compiled with -Onot
-- should try running with bounds checks activated
		    enumFromThenToSU
		      (mapU (*2) sqrPrimes)
		      (mapU (*3) sqrPrimes)
		      (replicateU (lengthU sqrPrimes) (n - 1))
      sieves'   = zipU sieves (replicateU (lengthU sieves) False)
      flags     = bpermuteDftU n (const True) sieves'
    in
    dropU 2 (filterU (flags!:) (enumFromToU 0 (n - 1)))


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
	  sum    = sumU result			-- ...to the NOINLINE
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
		 [100000]
--		 [5]
		 ("Primes up to 100,000\n\
		  \on " ++ host)
		 "primes"
