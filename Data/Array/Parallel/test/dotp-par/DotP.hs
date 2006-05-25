module Main 
where

import DotPImpl

-- standard library
import Random
import System

-- GHC libraries
import Control.Exception (evaluate)
import System.Console.GetOpt

-- Parallel array support
import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed
import BenchUtils


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
--    evaluate $ sumU vec    -- make sure it is brought into NF
    evaluate vec
    return vec

-- Execute and time the benchmark
--
dotp_bench :: Int -> IO (Integer, String)
dotp_bench size = 
  do
    vec1 <- generateVector size
    vec2 <- generateVector size
    (i,s) <- timeIt ("Length = " ++ show size) $
      do
	let result = dotp vec1 vec2
	evaluate result			-- make sure the work is done
	return $ show result
    return (i, s ++ " / (" ++ show (lst vec1 vec2) ++ ")")
  where
    lst vec1 vec2 = sum (zipWith (*) (fromU vec1) (fromU vec2))

-- Main routine
--
main :: IO ()
main =
  do
    args <- getArgs
    case getOpt Permute opts args of
      (os, [], []) ->
        let opts = foldr ($) dftOpts os
        in
        do
          optSetGang opts
          putStrLn "Dot Product Benchmark"
          putStrLn "====================="
    
          doBenchmarks [(dotp_bench, "Dot product", "uarr")]
                       (optPoints opts)
                       "Dot product"
		       "dotp"

data Opts = Opts { optSetGang :: IO ()
                 , optPoints  :: [Int]
                 }

dftOpts = Opts { optSetGang = setGang 2
               , optPoints  = [100000, 200000..1000000]
               }

opts = [Option ['p'] ["par"]
           (ReqArg (\s o -> o { optSetGang = setGang (read s) }) "N")
           "run in parallel with N threads"
       ,Option ['s'] ["seq"]
           (ReqArg (\s o -> o { optSetGang = setSequentialGang (read s) }) "N")
           "run sequentially simulating N threads"
       ,Option ['l'] ["lengths"]
           (ReqArg (\s o -> o { optPoints = read s }) "LIST")
           "run with specified array lengths"
       ]

