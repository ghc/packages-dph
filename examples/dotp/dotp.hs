import qualified DotPSeq
import qualified DotPPar
import qualified DotPVect

import Control.Exception (evaluate)
import System.Console.GetOpt
import System.Random

import Data.Array.Parallel.Prelude (fromUArrPA')
import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed

import Bench.Benchmark
import Bench.Options

algs = [("par", DotPPar.dotp)
       ,("seq", DotPSeq.dotp)
       ,("vect", dotp_vect)]

type Vector = UArr Double

dotp_vect :: Vector -> Vector -> Double
dotp_vect xs ys = DotPVect.dotp (fromUArrPA' xs) (fromUArrPA' ys)

-- generates a random vector of the given length in NF
--
generateVector :: Int -> IO Vector
generateVector n =
  do
    rg <- newStdGen
    let fs  = take n $ randomRs (-100, 100) rg
	vec = toU fs
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
    

