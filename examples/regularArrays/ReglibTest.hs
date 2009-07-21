{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Array as A
-- import qualified LArray as LA

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U

import Control.Exception (evaluate)


import Bench.Benchmark
import Bench.Options


import Debug.Trace

algs = [("1", transposeT)
        , ("2", transposePT)
        , ("3", transposeDFT)
        , ("4", relaxT)
        , ("5", relaxShiftT)
--        , ("4", backpermuteDftT)
--        , ("7", mmT)
--        , ("8", replicateT)
--        , ("9", sumT)
--        , ("10", selectT)
--        , ("11", lmmT)
       ]


transposeT:: (Int, U.Array Int) -> U.Array Int
transposeT (n,arrData) = 
  res
  where  
    res = A.arrayData $ A.transpose arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData


transposePT:: (Int, U.Array Int) -> U.Array Int
transposePT (n,arrData) = 
  res
  where  
    res = A.arrayData $ A.transposePrim arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData

transposeDFT:: (Int, U.Array Int) -> U.Array Int
transposeDFT (n,arrData) = 
  res
  where  
    res = A.arrayData $ A.transposeDFT arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int)) arrData


-- insert an array into one twice the size
backpermuteDftT:: Int -> U.Array Int
backpermuteDftT n = --trace (show res) res
  res
  where
    res = A.arrayData $ A.backpermuteDft arr  0 (() :*: 2*n) fn
    fn:: A.DIM1 -> Maybe A.DIM1
    fn (() :*: m) = if m < n then Just (() :*: m) else Nothing
    arr = A.toArray (() :*: n)  (U.fromList ([1..n]::[Int]))


relaxT:: (Int, U.Array Int) -> U.Array Int
relaxT (n, arrData) = --trace (show res) res
  res
  where
    res = A.arrayData $ A.relax arr
    arr = A.toArray ((() :*: n) :*: n) arrData

relaxShiftT:: (Int, U.Array Int) -> U.Array Int
relaxShiftT (n, arrData) = --trace (show res) res
  res
  where
    res = A.arrayData $ A.relaxShift arr
    arr = A.toArray ((() :*: n) :*: n) arrData 



selectT:: Int -> U.Array Int
selectT n = 
  res 
  where
    res = A.arrayData arr
    arr = A.select (A.toArray ((() :*: n) :*: n) (U.fromList ([1..(n*n)]::[Int])))
                 (A.IndexFixed 0 (A.IndexFixed (n-1) A.IndexNil))

replicateT:: Int -> U.Array Int
replicateT n = trace (show res)
  res 
  where
    res = A.arrayData arr
    arr = A.replicate  (A.toArray (() :*: n) (U.fromList ([1..n]::[Int])))
                 (A.IndexFixed 3 (A.IndexAll (A.IndexNil)))

sumT:: Int -> U.Array Int
sumT n = trace (show res)
  res
  where
    res = A.arrayData arr
    arr:: A.Array (() :*: Int) Int
    arr = A.mapFold (+) 0  (A.toArray ((() :*: 5) :*: 2) (U.fromList ([1..n*2*5]::[Int])))


mmT:: Int -> U.Array Int
mmT n = -- trace (show res)
  res 
  where
  res = A.arrayData arr
  arr = A.mmMult  a1 a2
  a1 = A.toArray (() :*: n :*: n) (U.fromList [1..n])
  a2 = A.toArray (() :*: n :*: n) (U.fromList [1..n])

generatePoints :: Int -> IO (Point (Int, (U.Array Int)))
generatePoints n =
  do 
    let pts = (U.fromList [1..(n*n)])
    evaluate $ force pts 
    return $  ("N = " ++ show n) `mkPoint` (n, pts)
  where
    force pts = pts U.!: n

{-
lmmT:: Int -> U.Array Int
lmmT n = -- trace (show res)
  res 
  where
  res = A.arrayData arr
  arr = LA.mmMult  a1 a2
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


