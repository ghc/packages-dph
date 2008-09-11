module Main where


import Control.Exception (evaluate)
import System.Console.GetOpt

import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark
import Bench.Options
import Data.Array.Parallel.Prelude (toUArrPA, fromUArrPA_3')


import PrimesVect (primesVect)
import Debug.Trace


algs = [("list", primesList), ("vect", primesVect')]

primesList:: Int -> U.Array Int
primesList n = trace (show res) res
  where
    res = U.fromList $ primesList' n

primesList' :: Int -> [Int]
primesList' 1 = []
primesList' n = sps ++ [ i | i <- [sq+1..n], multiple sps i ]
  where
    sps = primesList' sq 
    sq  = floor $ sqrt $ fromIntegral n

    multiple :: [Int] -> Int -> Bool
    multiple ps i = and [i `mod` p /= 0 | p <- ps]

primesVect':: Int -> U.Array Int
primesVect' n = trace (show res) res 
  where res = toUArrPA (primesVect n) 


simpleTest:: Int -> IO (Bench.Benchmark.Point ( Int))
simpleTest n =
  do
    evaluate testData
    return $ ("N = " ) `mkPoint` testData
  where
    testData:: Int
    testData = n

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
                            benchmark opts f [simpleTest szs] show
                            return ()
