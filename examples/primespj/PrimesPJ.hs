module Main where


import Control.Exception (evaluate)
import System.Console.GetOpt

import Data.Array.Parallel.PArray (PArray)
import qualified Data.Array.Parallel.PArray as P

import Bench.Benchmark
import Bench.Options

import PrimesVect (primesVect)
import Debug.Trace


algs = [("list", primesList), ("vect", primesVect')]

primesList:: Int -> PArray Int
primesList n = res
  where
    res = P.fromList $ primesList' n

primesList' :: Int -> [Int]
primesList' 1 = []
primesList' n = sps ++ [ i | i <- [sq+1..n], multiple sps i ]
  where
    sps = primesList' sq 
    sq  = floor $ sqrt $ fromIntegral n

    multiple :: [Int] -> Int -> Bool
    multiple ps i = and [i `mod` p /= 0 | p <- ps]

primesVect':: Int -> PArray Int
primesVect' n = res 
  where res = primesVect n


simpleTest:: Int -> IO (Bench.Benchmark.Point Int)
simpleTest n = return $ ("N = " ) `mkPoint` n

main = ndpMain "Primes"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "list" 


run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []    -> failWith ["No sizes specified"]
                 [szs] -> do 
                            benchmark opts f [simpleTest szs] P.nf show
                            return ()
