{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
 



import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U

--import qualified DArray as DA
import qualified Array as A
import Array (ae_mmMult, ae_transpose, ae_transposeDFT, ae_transposePrim,
             fromDArray, DArray(..), toDArray, da_append, da_tile, da_mmMult, da_mapFold,
             hmDA, hmJoin, hmSplit, toHMatrix, hmmult)
--import qualified ArrayExamples as A

import Foo

-- import qualified Hierarchical as H

import Control.Exception (evaluate)


import Bench.Benchmark
import Bench.Options

  
import Debug.Trace

algs = [ ("1", transposeTest1)
       , ("2", transposeTest2)
       , ("3", transposeTest3)
       , ("4", appendTest1)
       , ("5", appendTest2)
       , ("6", tileTest)
       , ("7", mmMultTest)
       , ("8", lmMultTest)
       , ("9", hmMultTest)
       , ("10", hmSplitJoinTest)
       , ("11", mfTest)
       , ("12", toHMTest)
--       , ("4", relaxT)
--       , ("5", relaxShiftT)
--        , ("4", backpermuteDftT)
--        , ("7", mmT)
--        , ("8", replicateT)
--        , ("9", sumT)
--        , ("10", selectT)
--        , ("11", lmmT)
       ]
  

transposeTest1:: (Int, U.Array Double) -> U.Array Double
transposeTest1 (n, arrData) = trace (
    if (res == arrData) 
      then "OK: transpose . transpose = id"
      else "Error:  transpose . transpose /= id")
  res
  where  
    i = ((() :*:  3 :*: 4 ):*:7) :: A.DIM3
    i' = ((() :*: 1 :*: 1):*:5) :: A.DIM3
    res = A.arrayData $ ae_transpose $ ae_transpose arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData
 
transposeTest2:: (Int, U.Array Double) -> U.Array Double
transposeTest2 (n, arrData) = trace (
    if (res == arrData) 
      then "OK: transposeDFT . transposeDFT = id"
      else "Error:  transposeDFT . transposeDFT /= id")
  res
  where  
    res = A.arrayData $ ae_transposeDFT $ ae_transposeDFT arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData

transposeTest3:: (Int, U.Array Double) -> U.Array Double
transposeTest3 (n, arrData) = trace (
    if (res == arrData) 
      then "OK: transposePrim . transposePrim = id"
      else "Error:  transposePrim . transposePrim /= id")
  res 
  where  
    res = A.arrayData $ ae_transposeDFT $ ae_transposeDFT arr
    arr = A.toArray (() :*: (n:: Int) :*: (n::Int))  arrData

appendTest1:: (Int, U.Array Double) -> U.Array Double
appendTest1 _ = trace (
   if (res == expected) 
      then "OK"
      else "Error: " ++ (show res))
  res 

  where 
    res = A.arrayData $ fromDArray $ da_append arr1 arr2 ((() :*: (2::Int)) :*: (8::Int))
    expected = U.fromList [1,2,3,4,5,1,2,3,6,7,8,9,10,4,5,6]
    arr1 = toDArray $ A.toArray ((() :*: (2::Int)) :*: (5::Int)) $ U.fromList [1..10]
    arr2 = toDArray $ A.toArray ((() :*: (2::Int)) :*: (3::Int)) $ U.fromList [1..6]


appendTest2:: (Int, U.Array Double) -> U.Array Double
appendTest2 _ = trace (
   if (res == expected) 
      then "OK"
      else "Error: " ++ (show res))
  res 
 
  where 
    expected = U.fromList [1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6]
    res = A.arrayData $ fromDArray $ da_append arr1 arr2 ((() :*: (8::Int)) :*: (2::Int))
    arr1 = toDArray $ A.toArray ((() :*: (5::Int)) :*: (2::Int)) $ U.fromList [1..10]
    arr2 = toDArray $ A.toArray ((() :*: (3::Int)) :*: (2::Int)) $ U.fromList [1..6]


  
tileTest::  (Int, U.Array Double) -> U.Array Double
tileTest _ = trace (show res) res
  where 
    res = A.arrayData $ fromDArray $ da_tile arr1 (() :*: 0 :*: 0)     (() :*: 2 :*: 2)
    arr1 = toDArray $ A.toArray ((() :*: (4::Int)):*: (4::Int)) $ U.fromList [1..16]

  

hmSplitJoinTest:: (Int, U.Array Double) -> U.Array Double
hmSplitJoinTest (n, arr) = trace( 
  if (res == arr)
    then ("hmSplitJoin test ok, join . split $ m = m\n")
    else ("hmSplitJoin test error, join . split $ m /= m\n"))
  res
  where 
    res = A.arrayData $ fromDArray $ hmDA $ hmJoin $ hmSplit $ 
            toHMatrix 128 $ A.toArray ((():*:n :*: n)::A.DIM2) $  arr

toHMTest:: (Int, U.Array Double) -> U.Array Double
toHMTest (n, arr) = trace (show res)
  res
  where
    res = A.arrayData $ fromDArray $ hmDA $ toHMatrix 128 arr
    arr = A.toArray ((():*:n :*: n)::A.DIM2) $ 
            (U.fromList ((concat $ replicate (n-1) ([1.0::Double] ++ (replicate n 0.0))) ++ 
            [1::Double]))



hmMultTest:: (Int, U.Array Double) -> U.Array Double
hmMultTest (n, arr) = trace( 
  if (res == arr)
    then ("hmMult test ok, m * id = m\n")
    else ("hmMult test error, m * id /= m\n"))
  res
  where 
    res = A.arrayData $ fromDArray $ hmDA $ hmmult arr1 arr2
    arr1 = toHMatrix 128 $ A.toArray ((() :*: (n::Int)):*: (n::Int)) arr
    arr2 = toHMatrix 128 $ fromDArray $ DArray ((() :*: (n::Int)):*: (n::Int)) 
                (\(() :*: i :*: j) -> if (i==j) then 1 else 0)

lmMultTest:: (Int, U.Array Double) -> U.Array Double
lmMultTest (n, arr) = trace( 
  if (res == arr)
    then ("lmMult test ok, m * id = m\n")
    else ("lmMult test error, m * id /= m\n"))
  res
  where   
    res = A.arrayData $ fromDArray $ da_mmMult arr1 arr2
    arr1 = toDArray $ A.toArray ((() :*: (n::Int)):*: (n::Int)) arr
    arr2 = DArray ((() :*: (n::Int)):*: (n::Int)) 
                (\(() :*: i :*: j) -> if (i==j) then 1 else 0)


mmMultTest:: (Int, U.Array Double) -> U.Array Double
mmMultTest (n, arr) = trace( 
  if (res == arr)
    then ("mmMult test ok, m * id = m\n")
    else ("mmMult test error, m * id /= m\n"))
  res
  where 
    res = A.arrayData $ ae_mmMult arr1 arr2
    arr1 = A.toArray ((() :*: (n::Int)):*: (n::Int)) arr
    arr2 = fromDArray $ DArray ((() :*: (n::Int)):*: (n::Int)) 
                (\(() :*: i :*: j) -> if (i==j) then 1 else 0)

mfTest:: (Int, U.Array Double) -> U.Array Double
mfTest _ = trace (show res) res
  where 
    res = A.arrayData $ fromDArray $ da_mapFold (+) 0 arr1
    arr1 = toDArray $ A.toArray ((() :*: (2::Int)):*: (8::Int)) $ U.fromList [1..16]
    

{-
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

-}

generatePoints :: Int -> IO (Point (Int, (U.Array Double)))
generatePoints n =
  do 
    let pts = (U.fromList [1..(fromIntegral $ n*n)])
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
                          benchmark opts transposeTest1
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()

             Just f  -> do
                          benchmark opts f
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()


