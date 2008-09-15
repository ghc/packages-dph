import DotPVect ( dotp )

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import Data.Array.Parallel.PArray (PArray, randomRs, nf)

import Bench.Benchmark
import Bench.Options

generateVector :: Int -> IO (PArray Double)
generateVector n =
  do
    rg <- R.newStdGen
    let vec = randomRs n (-100, 100) rg
    evaluate (nf vec)
    return vec

generateVectors :: Int -> IO (Point (PArray Double, PArray Double))
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
                (`seq` ()) show
             return ()

