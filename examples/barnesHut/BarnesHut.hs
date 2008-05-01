module Main where
import BarnesHutSeq
import BarnesHutPar
import qualified BarnesHutVect as V
import qualified BarnesHutList as L
import BarnesHutGen

import Control.Exception (evaluate)
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel

import Bench.Benchmark
import Bench.Options
import Data.Array.Parallel.Prelude (toUArrPA, fromUArrPA_3')

import Debug.Trace



algs = [("seqSimple", bhStepSeq), ("parSimple", bhStepPar), ("vect", bhStepVect), ("list", bhStepList)]

bhStepSeq (dx, dy, particles) = trace (showBHTree bhtree) accs
  where
   accs   = calcAccel bhtree  (flattenSU particles)
   bhtree = splitPointsL (singletonU ((0.0 :*: 0.0) :*: (dx :*: dy))) particles

bhStepPar (dx, dy, particles) = trace (showBHTree bhTree) accs
  where 
    accs     = calcAccel bhTree (flattenSU particles)
    bhTree    = splitPointsLPar (singletonU ((0.0 :*: 0.0) :*: (dx :*: dy)))
                        particles

bhStepVect (dx, dy, particles) = trace (show  accs) accs  
  where
    accs       = zipU (toUArrPA xs) (toUArrPA ys) 
    (xs, ys)   = V.oneStep 0.0 0.0 dx dy particles'
    particles' = (fromUArrPA_3' $ flattenSU particles) 

bhStepList (dx, dy, particles) = trace (show  accs) accs  
  where
    accs       = zipU (toU xs) (toU ys) 
    (xs, ys)   = L.oneStep 0.0 0.0 dx dy particles'
    (p1 :*: p2 :*: p3) = unzip3U $ concatSU particles
    particles' = zip3 (fromU p1) (fromU p2) (fromU p3) 



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
       0.7 :*: 0.7 :*: 5.0,
       0.8 :*: 0.7 :*: 5.0,
       0.9 :*: 0.9 :*: 5.0]


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
                          benchmark opts f [simpleTest szs 0  0] show
                          return ()

