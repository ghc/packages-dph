module Main 
where

import qualified DotPSeq
import qualified DotPPar

-- standard library
import Random
import System

-- GHC libraries
import Control.Exception (evaluate)
import System.Console.GetOpt

-- Parallel array support
import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed
--import BenchUtils
import Bench.Benchmark
import Bench.Options

algs = [("par", DotPPar.dotp)
       ,("seq", DotPSeq.dotp)]

type Vector = UArr Double

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

generateVectors :: Int -> IO (Point (Vector, Vector))
generateVectors n =
  do
    v <- generateVector n
    w <- generateVector n
    return $ ("N = " ++ show n) `mkPoint` (v,w)


main = ndpMain "Dot product"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                      "use the specified algorithm"]
                   "par"

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f -> case map read sizes of
                []  -> failWith ["No sizes specified"]
                szs -> do
                         benchmark opts (uncurry f)
                            (map generateVectors szs)
                            show
                         return ()
    

