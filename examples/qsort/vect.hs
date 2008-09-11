module Main where
import QSortVect

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import Data.Array.Parallel.Prelude (PArray, toUArrPA, fromUArrPA')
import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark
import Bench.Options

import Debug.Trace

qsortVect' :: U.Array Double -> U.Array Double
qsortVect' xs = res
  where res = toUArrPA . qsortVect $ fromUArrPA' xs

generateVector :: Int -> IO (Point (U.Array Double))
generateVector n =
  do
    evaluate vec
    return $ ("N = " ++ show n) `mkPoint` vec
  where
    vec = U.fromList (reverse [1..fromInteger (toInteger n)])

main = ndpMain "QSort"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts qsortVect' (map generateVector szs) show
             return ()

