module Bench where

import Random
import CPUTime
import Monad
import IO
import System.Console.GetOpt
import System.Environment
import System.Exit

import Control.Exception (evaluate)
import System.Mem        (performGC)

import Data.Array.Parallel.Unlifted

class Arbitrary a where
  arbitraries :: (Int,Int) -> Int -> StdGen -> [a]
  

generateMany :: Arbitrary a => (Int,Int) -> Int -> IO [a]
generateMany b n = liftM (arbitraries b n) newStdGen

generate :: Arbitrary a => (Int,Int) -> Int -> IO a
generate b n = liftM head $ generateMany b n

instance Arbitrary () where
  arbitraries _ _ _ = repeat ()

instance Arbitrary Bool where
  arbitraries _ _ = randoms

instance Arbitrary Int where
  arbitraries b _ = randomRs b

instance Arbitrary Float where
  arbitraries (l,h) _ = randomRs (fromIntegral l, fromIntegral h)

instance Arbitrary Double where
  arbitraries (l,h) _ = randomRs (fromIntegral l, fromIntegral h)

instance Arbitrary a => Arbitrary [a] where
  arbitraries b n g = split (arbitraries b n g)
    where
      split xs = let (ys, zs) = splitAt n xs in ys : split zs

instance (UA a, Arbitrary a) => Arbitrary (UArr a) where
  arbitraries b n g = map toU (arbitraries b n g)

class Bench a where
  bench :: (Int,Int) -> Int -> IO a -> IO Integer
  bench b n p = do
                  performGC
                  start <- getCPUTime
                  x <- p
                  evaluate x
                  end   <- getCPUTime
                  return $ (end - start) `div` 1000000000


instance Bench ()
instance Bench Bool
instance Bench Int
instance Bench Float
instance Bench Double
instance Bench (UArr a)

instance (Arbitrary a, Bench b) => Bench (a -> b) where
  bench b n p = do
                  f <- p
                  evaluate f
                  x <- generate b n
                  evaluate x
                  bench b n (return $ f x)

benchmark :: Bench (a -> b) => Int -> (a -> b) -> (Int,Int) -> Int -> IO ()
benchmark rs f b n =
  do
    putStr $ "Length = " ++ show n
    ts <- mapM run [1..rs]
    putStrLn "...done"
    putStrLn $ "Time: " ++ show (minimum ts) ++ "ms"

  where
    run r = do
              putStr $ "..." ++ show r
              hFlush stdout
              bench b n (return f)

benchmarks :: Bench (a -> b) => String -> (a -> b) -> (Int,Int) -> IO ()
{-# NOINLINE benchmarks #-}
benchmarks s f b =
  do
    args <- getArgs
    case getOpt Permute opts args of
      (os, [], []) ->
        let Opts seed runs ns = foldr ($) dftOpts os
        in
        if null ns
          then err "No points specified"
          else do
                 setStdGen (mkStdGen seed)
                 putStrLn $ s ++ "\n============\nSeed: "
                              ++ show seed ++ "\n"
                 mapM_ (benchmark runs f b) ns
      (_, (a : _), []) -> err $ "Invalid argument: " ++ a
      (_, _, es)       -> err $ concat es
  where
    err s = do
              hPutStrLn stderr s
              hPutStrLn stderr $ usageInfo "<benchmark> OPTIONS" opts
              exitFailure

data Opts = Opts { optSeed   :: Int
                 , optRuns   :: Int
                 , optPoints :: [Int]
                 }

dftOpts = Opts { optSeed = 1, optRuns = 1, optPoints = [] }

type OptsM = Opts -> Opts

opts = [Option ['s'] ["seed"]   (ReqArg (\s o -> o { optSeed = read s })
                                        "N")
               "seed for the random generator"
       ,Option ['r'] ["runs"]   (ReqArg (\s o -> o { optRuns = read s })
                                        "N")
               "number of runs per benchmark"
       ,Option ['p'] ["points"] (ReqArg (\s o -> o { optPoints = read s })
                                        "[N1,...,Nn]")
               "data points at which to run the benchmark"
       ]
