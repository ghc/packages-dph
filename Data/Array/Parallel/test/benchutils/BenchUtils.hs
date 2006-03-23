-- |Support routines for benchmarks
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  $Id: BenchUtils.hs,v 1.2 2002/08/10 09:19:15 chak Exp $
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98
--
--  Simple benchmarking framework that generates file suitable for plotting
--  graphs with gnuplot.
--
--- Todo ----------------------------------------------------------------------
--

module BenchUtils (
  -- * Benchmark specification
  BenchFun, Comment, Suffix, Benchmark, BenchPoints,

  -- * Support functions to define benchmarks
  timeIt,

  -- * Functions to run benchmarks
  doBenchmarks
) where

-- standard libraries
import CPUTime
import IO
import Monad

-- GHC-specific modules
import Control.Exception  (evaluate)
import System.Mem	  (performGC)


-- |Benchmark descriptions
-- -----------------------

-- |Benchmark functions with size parameter returning its execution time (in
-- milliseconds) and result (in string form suitable for printing).
--
-- * It must be ensured that by running this action, the benchmark is run in
--   its entirety; ie, no part of the evaluation of the actual benchmarked
--   routine may depend on demanding the result string.
--
type BenchFun = Int -> IO (Integer, String)

-- |Gnuplot comment (may include line breaks)
--
type Comment = String

-- |Benchmark-specific file suffix
--
type Suffix = String

-- |Benchmark, gnuplot comment, and file suffix characterise a benchmark
--
-- * The first line of the comment should be a short description of the
--   benchmark
--
type Benchmark = (BenchFun, Comment, Suffix)

-- |Points at which a benchmark is to be tested
--
type BenchPoints = [Int]


-- Benchmarking functions
-- ----------------------

-- |Execute a single benchmark run (it's run twice and the shorter runtime
-- returned) 
--
timeIt :: String -> IO a -> IO (Integer, a)
timeIt desc comp = 
  do 
    putStr $ desc ++ "...1st..."
    hFlush stdout
    performGC
    startTime <- getCPUTime
    result <- comp
    endTime   <- getCPUTime
    let timeInMS1 =  (endTime - startTime) `div` 1000000000
    putStr "2nd..."
    hFlush stdout
    performGC
    startTime <- getCPUTime
    result <- comp
    endTime   <- getCPUTime
    let timeInMS2 =  (endTime - startTime) `div` 1000000000
    putStrLn "done."
    return (timeInMS1 `min` timeInMS2, result)

-- |Given a benchmark family (ie, list of benchmarks) and a set of benchmark
-- points, test the family on these points
--
-- * The comment is common to all benchmarks (e.g., date and machine,
--   compiler, etc)
--
-- * During the benchmark run, progress is logged to stdout, and the results
--   are dumped into a file suitable for gnuplot
--
doBenchmarks :: [Benchmark] -> BenchPoints -> Comment -> FilePath -> IO ()
doBenchmarks benchs points comment fname = mapM_ oneBench benchs
  where
    oneBench (benchfun, specificComment, suffix) =
      do
        let desc = takeWhile (/= '\n') specificComment
	putStrLn $ "\n*** " ++ desc
        rtimes <- mapM oneRun points
	writeGPFile (zip points rtimes) 
		    (fname ++ "." ++ suffix) 
		    (comment ++ "\n" ++ specificComment)
      where
        oneRun size = 
	  do
	    (rtime, result) <- benchfun size
	    putStrLn $ 
	      "Time: " ++ show rtime ++ "ms (Result: " ++ result ++ ")"
	    return rtime

-- Write a file suitable for producing a graph with gnuplot
--
writeGPFile :: Show a => [(Int, a)] -> String -> String -> IO ()
writeGPFile rtimes fname comment =
  do  
    let comment' = comment ++ "\nRuntimes are in milliseconds"
    file <- openFile fname WriteMode
    hPutStr file . unlines . map ("# " ++) . lines $ comment'
    hPutStr file $ unlines [show x ++ "\t" ++ show y | (x, y) <- rtimes]
    hClose file
