module Main where
import BarnesHutSeq
import BarnesHutPar
import BarnesHutGen

import Control.Exception (evaluate)
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted

import Bench.Benchmark
import Bench.Options

type BhAlg = Int -> Double -> Double -> IO (UArr Double)

algs = [("seqSimple", bhStepSeq), ("parSimple", bhStepPar)]

bhStepSeq (dx, dy, particles) = 
  calcAccel (splitPointsL (singletonU ((0.0 :*: 0.0) :*: (dx :*: dy)))
                        particles) (flattenSU particles)


bhStepPar (dx, dy, particles) = 
  calcAccel (splitPointsLPar (singletonU ((0.0 :*: 0.0) :*: (dx :*: dy)))
                        particles) (flattenSU particles)


mapData:: IO (Bench.Benchmark.Point (UArr Double))
mapData = do
  evaluate testData
  return $ ("N = " ) `mkPoint` testData
  where
    testData:: UArr Double
    testData = toU $ map fromIntegral [0..10000000]



-- simpleTest:: 
simpleTest:: [Int] -> Double -> Double -> IO (Bench.Benchmark.Point (Double, Double, SUArr MassPoint))
simpleTest _ _ _=
  do
    evaluate testData
    return $ ("N = " ) `mkPoint` testData
  where
    testData = (1.0, 1.0,  singletonSU testParticles)
    -- particles in the bounding box 0.0 0.0 1.0 1.0
    testParticles:: UArr MassPoint
    testParticles = toU [
       0.3 :*: 0.2 :*: 5.0,
       0.2 :*: 0.1 :*: 5.0,
       0.1 :*: 0.2 :*: 5.0,
       0.8 :*: 0.8 :*: 5.0,
       0.7 :*: 0.9 :*: 5.0,
       0.8 :*: 0.9 :*: 5.0,
       0.6 :*: 0.6 :*: 5.0,
       0.6 :*: 0.7 :*: 5.0,
       0.7 :*: 0.7 :*: 5.0,
       0.6 :*: 0.9 :*: 5.0]


randomDistTest n dx dy = 
  do
    testParticles <- randomMassPointsIO dx dy 
    let testData = (singletonU testBox,  singletonSU $ toU $ take n testParticles)
    evaluate testData
    return $ ("N = " ) `mkPoint` testData
       
  where
    testBox = (0.0 :*: 0.0) :*: (dx :*: dy)       
   

main = ndpMain "BarnesHut"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq" 

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []    -> failWith ["No sizes specified"]
                 szs -> do 
                          benchmark opts f [simpleTest szs undefined undefined] show
                          return ()

