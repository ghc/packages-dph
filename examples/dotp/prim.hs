import DotPPrim ( dotp )

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark
import Bench.Options

generateVector :: Int -> IO (U.Array Double)
generateVector n =
  do
    rg <- R.newStdGen
    let vec = U.randomRs n (-100, 100) rg
    evaluate vec
    return vec

generateVectors :: Int -> IO (Point (U.Array Double, U.Array Double))
generateVectors n =
  do
    v <- generateVector n
    w <- generateVector n
    return $ ("N = " ++ show n) `mkPoint` (v,w)


main = ndpMain "Dot product"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts (uncurry dotp)
                (map generateVectors szs)
                 show
             return ()

