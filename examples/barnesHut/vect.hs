module Main where
import qualified BarnesHutVect as V

import Data.Array.Parallel.PArray (PArray)
import qualified Data.Array.Parallel.PArray as P

import Control.Exception (evaluate)
import System.Console.GetOpt

import Bench.Benchmark
import Bench.Options

import Debug.Trace

type MassPoint = (Double, Double, Double)

bhStepVect (dx, dy, particles) = trace (show  accs) accs  
  where
    accs = V.oneStep 0.0 0.0 dx dy particles

mapData:: IO (Bench.Benchmark.Point (PArray Double))
mapData = do
  evaluate (P.nf testData)
  return $ ("N = " ) `mkPoint` testData
  where
    testData:: PArray Double
    testData = P.fromList $ map fromIntegral [0..10000000]



-- simpleTest:: 
simpleTest:: [Int] -> Double -> Double -> IO (Bench.Benchmark.Point (Double, Double, PArray MassPoint))
simpleTest _ _ _=
  do
    evaluate testData
    return $ ("N = " ) `mkPoint` testData
  where
    testData = (1.0, 1.0, testParticles)
    -- particles in the bounding box 0.0 0.0 1.0 1.0
    testParticles:: PArray MassPoint
    testParticles = P.fromList [
       (0.3, 0.2, 5.0),
       (0.2, 0.1, 5.0),
       (0.1, 0.2, 5.0),
       (0.8, 0.8, 5.0),
       (0.7, 0.9, 5.0), 
       (0.8, 0.9, 5.0),
       (0.6, 0.6, 5.0),
       (0.7, 0.7, 5.0),
       (0.8, 0.7, 5.0),
       (0.9, 0.9, 5.0)]

main = ndpMain "BarnesHut"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
    case map read sizes of
      []  -> failWith ["No sizes specified"]
      szs -> do 
               benchmark opts bhStepVect [simpleTest szs 0  0] P.nf show
               return ()

