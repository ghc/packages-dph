import MatrixPrim

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark
import Bench.Options

import Debug.Trace

algs = [(" vecMult", mmMult)]


mmMult'':: (Int, U.Array Double, U.Array Double) -> U.Array Double
--mmMult'' (order, m, n) = 
mmMult'' (order, m, n) = mmMult order m n
{-
  let 
    xs = mmMult order m n
    lres = (listMult order (U.toList m) (U.toList n)) 
    ares = (U.toList xs)
  in if (lres == ares)
       then m
       else error ("div res:\n" ++ (show (sum $ zipWith (-) ares lres)))
-}


listMult:: Int -> [Double] -> [Double] -> [Double]
listMult order mA mB = [sum (zipWith (*) (mmA!!i) (mmB!!j)) | i<- [0..(order-1)], j <- [0..(order-1)]]
  where
    mmB  = [[mB!!(j*order+i)| j <- [0..(order-1)]] | i <- [0..(order-1)]]
    mmA  = [[mA!!(i*order+j)| j <- [0..(order-1)]] | i <- [0..(order-1)]]              

generateVector :: Int -> IO (U.Array Double)
generateVector n =
  do
    rg <- R.newStdGen
    let vec = U.randomRs n (-100, 100) rg
    evaluate vec
    return vec

generateVectors :: Int -> IO (Point (Int, U.Array Double, U.Array Double))
generateVectors n =
  do
    v <- generateVector (n*n)
    w <- generateVector (n*n)
    return $ ("N = " ++ show n) `mkPoint` (n,v,w)




main = ndpMain "Matrix Mult"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts  mmMult''
                (map generateVectors szs)
                (`seq` ()) show
             return ()


