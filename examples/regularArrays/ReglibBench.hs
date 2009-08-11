{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Array 
import qualified Array as A
import Array (ae_mmMult, ae_transpose, ae_transposeDFT, ae_transposePrim,
             fromDArray, DArray(..), toDArray,
             hmDA, hmJoin, hmSplit, toHMatrix, hmmult)


--import qualified DArray as DA

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R
-- import qualified Hierarchical as H


import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U

import Control.Exception (evaluate)


import Bench.Benchmark
import Bench.Options




algs = [ ("1", transposeT) 
       , ("2", transposePT) 
       , ("3", transposeDFT)
       , ("4", transposeDT) 
       , ("5", transposePDT) 
       , ("6", relaxT) 
       , ("7", relaxDT) 
       , ("8", mmT) 
       , ("9", mmDT) 
       , ("10", hmDT) 
       , ("11", hhmDT) ]
  

  
transposeT:: (Int, U.Array Double) -> U.Array Double
transposeT (n,arrData) = 
  res
  where  
    res = A.arrayData $ ae_transpose arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData


transposePT:: (Int, U.Array Double) -> U.Array Double
transposePT (n,arrData) = 
  res
  where  
    res = A.arrayData $ ae_transposePrim arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData

transposeDFT:: (Int, U.Array Double) -> U.Array Double
transposeDFT (n,arrData) = 
  res
  where  
    res = A.arrayData $ ae_transposeDFT arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int)) arrData

transposeDT:: (Int, U.Array Double) -> U.Array Double
transposeDT (n,arrData) = 
  res
  where  
    res = A.arrayData $ fromDArray $ da_transpose arr
    arr = toDArray $ A.toArray (() :*: (n:: Int) :*: (n::Int)) arrData

transposePDT:: (Int, U.Array Double) -> U.Array Double
transposePDT (n,arrData) = 
  res
  where  
    res = U.map  (\i -> (arrData U.!: (A.index sh (flip i)))) (A.range sh)
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


relaxT:: (Int, U.Array Double) -> U.Array Double
relaxT (n, arrData) = --trace (show res) res
  res
  where
    res = A.arrayData $ ae_relax arr
    arr = A.toArray ((() :*: n) :*: n) arrData

relaxDT:: (Int, U.Array Double) -> U.Array Double
relaxDT (n, arrData) = --trace (show res) res
  res
  where
    res = A.arrayData $ da_relaxShift arr
    arr = A.toArray ((() :*: n) :*: n) arrData 



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


mmT:: (Int, U.Array Double) -> U.Array Double
mmT (n,arrData) = 
  res 
  where
  res = A.arrayData arr
  arr = ae_mmMult  a1 a2
  a1 =  A.toArray (() :*: n :*: n) arrData
  a2 =  A.toArray (() :*: n :*: n) arrData

mmDT:: (Int, U.Array Double) -> U.Array Double
mmDT (n,arrData) = 
  res 
  where
  res = A.arrayData arr
  arr = fromDArray $ da_mmMult  a1 a2
  a1  = toDArray $ A.toArray (() :*: n :*: n) arrData
  a2  = toDArray $ A.toArray (() :*: n :*: n) arrData

hmDT:: (Int, U.Array Double) -> U.Array Double
hmDT (n,arrData) = 
  res 
  where
  res = A.arrayData arr
  arr = fromDArray $ hmDA $ hmmult  (toHMatrix 128 a1)(toHMatrix 128 a1)
  a1  = A.toArray (() :*: n :*: n) arrData
  a2  = A.toArray (() :*: n :*: n) arrData

hhmDT:: (Int, U.Array Double) -> U.Array Double
hhmDT (n,arrData) = 
  res 
  where
  res = A.arrayData arr
  arr = fromDArray $ hhmDA $ hhmmult  (toHHMatrix n 128 a1)(toHHMatrix n 128 a1)
  a1  = A.toArray (() :*: n*n) arrData
  a2  = A.toArray (() :*: n*n) arrData

generatePoints :: Int -> IO (Point (Int, (U.Array Double)))
generatePoints n =
  do 
    let pts = (U.fromList [1.0..(fromIntegral (n*n))])
    evaluate $ force pts 
    return $  ("N = " ++ show n) `mkPoint` (n, pts)
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
                          benchmark opts transposeT
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()

             Just f  -> do
                          benchmark opts f
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()


