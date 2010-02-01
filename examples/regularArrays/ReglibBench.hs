{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Array 
import qualified Array as A
import qualified ArrayExamples  as AE
import qualified DArray as DA
import qualified DArrayExamples as DAE
import Hierarchical

--import qualified DArray as DA

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R
-- import qualified Hierarchical as H


import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U

import Control.Exception (evaluate)
import Debug.Trace

import Bench.Benchmark
import Bench.Options




algs = [ ("0", transposeT) 
       , ("1", transposePT) 
       , ("2", transposeDFT)
       , ("3", transposeDT) 
       , ("4", transposePDT) 
       , ("5", relaxT) 
       , ("6", relaxDT) 
       , ("7", relaxDMS) 
       , ("8", mmT) 
       , ("9", mmDT)
       , ("10", mmDPT)
       , ("11", hmDT) 
       , ("12", hhmDT)
       , ("13", fft3d)
       , ("14", redBlack)
       ]
  

  
transposeT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
transposeT (n,(arrData,_)) = 
  res
  where  
    res = A.arrayData $ AE.transpose arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData


transposePT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
transposePT (n,(arrData,_)) = 
  res
  where  
    res = A.arrayData $ AE.transposePrim arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData

transposeDFT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
transposeDFT (n,(arrData,_)) = 
  res
  where  
    res = A.arrayData $ AE.transposeDFT arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int)) arrData

transposeDT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
transposeDT (n,(arrData,_)) = 
  res
  where  
    res = A.arrayData $ DA.fromDArray $ DAE.transpose arr
    arr = DA.toDArray $ A.toArray (() :*: (n:: Int) :*: (n::Int)) arrData

transposePDT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
transposePDT (n,(arrData,_)) = 
  res
  where  
    res = U.map  (\i -> (arrData U.!: (A.toIndex sh (flip i)))) (A.range sh)
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int)) arrData
    sh = (() :*: (n:: Int) :*: (n::Int))
    flip (() :*: (i) :*: (j)) = (() :*: (j) :*: (i))


-- insert an array into one twice the size
backpermuteDftT:: Int -> U.Array Double
backpermuteDftT n = --trace (show res) res
  res
  where
    res = A.arrayData $ A.backpermuteDft arr  0.0 (() :*: 2*n) fn
    fn:: A.DIM1 -> Maybe A.DIM1
    fn (() :*: m) = if m < n then Just (() :*: m) else Nothing
    arr = A.toArray (() :*: n)  (U.fromList ([1.0..(fromIntegral n)]::[Double]))


relaxT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
relaxT (n,( arrData,_)) = --trace (show res) res
  res
  where
    res = A.arrayData $ AE.relax arr
    arr = A.toArray ((() :*: n) :*: n) arrData

relaxDT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
relaxDT (n,( arrData,_)) = --trace (show res) res
  res
  where
    res = A.arrayData $ DAE.relaxShift arr
    arr = DA.toDArray $ A.toArray ((() :*: n) :*: n) arrData 

relaxDMS:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
relaxDMS (n,( arrData,_)) = --trace (show res) res
  res
  where
    res = A.arrayData $ DA.fromDArray $ DAE.relaxMS arr
    arr = DA.toDArray $ A.toArray ((() :*: n) :*: n) arrData 


selectT:: Int -> U.Array Double
selectT n = 
  res 
  where
    res = A.arrayData arr
    arr = A.select (A.toArray ((() :*: n) :*: n) (U.fromList ([1..(fromIntegral (n*n))]::[Double])))
                 (A.IndexFixed 0 (A.IndexFixed (n-1) A.IndexNil))

replicateT:: Int -> U.Array Double
replicateT n = -- trace (show res)
  res 
  where
    res = A.arrayData arr
    arr = A.replicate  (A.toArray (() :*: n) (U.fromList ([1..(fromIntegral n)]::[Double])))
                 (A.IndexFixed 3 (A.IndexAll (A.IndexNil)))

sumT:: Int -> U.Array Double
sumT n = -- trace (show res)
  res
  where
    res = A.arrayData arr
    arr:: A.Array (() :*: Int) Double
    arr = A.mapFold (+) 0  (A.toArray ((() :*: 5) :*: 2) (U.fromList ([1..(fromIntegral n)*2*5]::[Double])))


mmT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
mmT (n,(arrData1,arrData2)) = 
  res
  where
  res = A.arrayData arr
  arr = AE.mmMult  a1 a2
  a1 =  A.toArray (() :*: n :*: n) arrData1
  a2 =  A.toArray (() :*: n :*: n) arrData2

mmDT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
mmDT (n,(arrData1,arrData2)) = 
  res
  where
  res = A.arrayData arr
  arr = DAE.mmMult'  a1 a2
  a1  = DA.toDArray $ A.toArray (() :*: n :*: n) arrData1
  a2  = DA.toDArray $ A.toArray (() :*: n :*: n) arrData2

mmDPT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
mmDPT (n,(arrData1,arrData2)) = 
  res
  where
  res = A.arrayData arr
  arr = DAE.mmMultP'  a1 a2
  a1  = DA.toDArray $ A.toArray (() :*: n :*: n) arrData1
  a2  = DA.toDArray $ A.toArray (() :*: n :*: n) arrData2

hmDT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
hmDT (n,(arrData,_)) = 
  res 
  where
  res = A.arrayData arr
  arr = DA.fromDArray $ hmDA $ hmmult  (toHMatrix 128 a1)(toHMatrix 128 a1)
  a1  = A.toArray (() :*: n :*: n) arrData
  a2  = A.toArray (() :*: n :*: n) arrData

hhmDT:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double 
hhmDT (n,(arrData1,arrData2)) = 
  res 
  where
  res = A.arrayData arr
  arr = DA.fromDArray $ hhmDA $ hhmmult  (toHHMatrix n 128 a1)(toHHMatrix n 128 a1)
  a1  = A.toArray (() :*: n*n) arrData1
  a2  = A.toArray (() :*: n*n) arrData2

fft3d:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double
fft3d _ =  res
  where
    size = 64
{-
    res = A.arrayData $ DA.fromDArray $ DA.map (\(_ :*: x) -> x) $ DAE.fft2D 1 arr
    arr = DA.toDArray $ A.toArray (() :*: size :*: size) $ 
             U.fromList $  Prelude.map (\x -> (x :*: x)) $ take (size*size) [1.0,1.05..]
-}

    res = A.arrayData $ DA.fromDArray $ DA.map (\(_ :*: x) -> x) $ DAE.fft3D 1 arr
    arr = DA.toDArray $ A.toArray (() :*: size :*: size :*: size) $ 
             U.fromList $  Prelude.map (\x -> (x :*: x)) $ take (size*size*size) [1.0,1.05..]

{-
    res = A.arrayData $ DA.fromDArray $ DA.map (\(_ :*: x) -> x) $ DAE.fftRofu arr  
    arr = DA.toDArray $ A.toArray (() :*: size) $ U.fromList $ 
             Prelude.map (\x -> (x :*: x)) $ take (size) ([1.0,1.05..]::[Double])
 -}



redBlack:: (Int, (U.Array Double, U.Array Double)) -> U.Array Double 
redBlack (n, (arrData1, arrData2)) 
  | False = error "redBlack : n size needs to be muliple of 4\n"
  | otherwise      =  
      A.arrayData $ DA.fromDArray $ DAE.redBlack 0.25 0.123 a1 a2
  where
    a1  = DA.toDArray $ A.toArray (() :*: n :*: n :*: n) arrData1
    a2  = DA.toDArray $ A.toArray (() :*: n :*: n :*: n) arrData2
    
generatePoints :: Int -> IO (Point (Int, (U.Array Double, U.Array Double)))
generatePoints n =
  do 
    let pts1 = (U.fromList [1.0..(fromIntegral (n*n*n))])
    evaluate $ force pts1     
    let pts2 = (U.fromList [1.0..(fromIntegral (n*n*n))])
    evaluate $ force pts2 
    return $  ("N = " ++ show n) `mkPoint` (n, (pts1, pts2))
  where
    force pts = pts U.!: n

{-
lmmT:: Int -> U.Array Double
lmmT n = -- trace (show res)
  res 
  where
  res = A.arrayData arr
  arr = DA.mmMult  a1 a2
  a1 = A.toArray (() :*: n :*: n) (U.fromList [1..n])
  a2 = A.toArray (() :*: n :*: n) (U.fromList [1..n])
  -}
 
main = ndpMain "RArray Test"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq" 

run opts alg sizes =
  case Prelude.map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> case lookup alg algs of
             Nothing -> do
                          runMMBench opts

             Just f  -> do
                          benchmark opts f
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()

runMMBench opts =
  do
     putStrLn "strict rarrays :" 
     benchmark opts mmT (Prelude.map generatePoints [128,256,512])
        (`seq` ()) show
     putStrLn "delayed :" 
     benchmark opts mmDT (Prelude.map generatePoints [128,256,512,1024])
        (`seq` ()) show
     putStrLn "hier. alg delayed :" 
     benchmark opts hmDT (Prelude.map generatePoints [128,256,512,1024])
        (`seq` ()) show
     putStrLn "hier. rep delayed :" 
     benchmark opts hhmDT (Prelude.map generatePoints [128,256,512,1024])
        (`seq` ()) show
     return ()