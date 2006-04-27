module Main (main)
where

-- standard library
import Random
import System

-- GHC libraries
import Control.Exception (evaluate)

-- Parallel array support
import Data.Array.Parallel.Unlifted
import BenchUtils


-- Benchmarked code
-- ----------------

type Vector = UArr Double

-- !!!FIXME: Fusion doesn't seem to work properly; it's a factor 2 too slow.
dotp :: Vector -> Vector -> Double
{-# NOINLINE dotp #-}
dotp v w = sumU (zipWithU (*) v w)

 {- This corresponds to the fused version.
dotp v w = loopAcc
           . loopU (\a (x:*:y) -> (a + x * y, Nothing::Maybe ())) 0
	   $ zipU v w
  -}


-- Benchmark infrastructure
-- ------------------------

-- generates a random vector of the given length in NF
--
generateVector :: Int -> IO Vector
generateVector n =
  do
    rg <- newStdGen
    let fs  = take n $ randomRs (-100, 100) rg
	vec = toU fs
    evaluate $ sumU vec    -- make sure it is brought into NF
    return vec

-- Execute and time the benchmark
--
dotp_bench :: Int -> IO (Integer, String)
dotp_bench size = 
  do
    vec1 <- generateVector size
    vec2 <- generateVector size
    timeIt ("Length = " ++ show size) $
      do
	let result = dotp vec1 vec2
	evaluate result			-- make sure the work is done
	return (show result)

-- Main routine
--
main :: IO ()
main =
  do
    host <- getEnv "HOSTNAME"
    putStrLn "Dot Product Benchmark"
    putStrLn "====================="

    doBenchmarks [(dotp_bench, "Dot product", "uarr")]
		 [100000, 200000..500000]
		 ("Dot product with 100,000 to 500,000 element vectors\n\
		  \on " ++ host)
		 "dotp"
