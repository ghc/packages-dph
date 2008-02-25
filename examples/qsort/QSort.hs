module Main where
import QSortSeq
import QSortPar

import Control.Exception (evaluate      )
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted

import Bench.Benchmark
import Bench.Options


algs = [("seq", qsortSeq), ("par", qsortPar), ("list", qsortList . fromU)]

generateVector :: Int -> IO (Point (UArr Double))
generateVector n =
  do
    evaluate vec
    return $ ("N = " ++ show n) `mkPoint` vec
  where
    vec = toU [1..fromInteger (toInteger n)]

main = ndpMain "QSort"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq"

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []  -> failWith ["No sizes specified"]
                 szs -> do
                          benchmark opts f (map generateVector szs) show
                          return ()

