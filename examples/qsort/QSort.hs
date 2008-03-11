{-# OPTIONS -fno-spec-constr-count #-}
module Main where
import QSortSeq
import QSortPar
import QSortVect

import Control.Exception (evaluate      )
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Prelude (toUArrPA, fromUArrPA')

import Bench.Benchmark
import Bench.Options


algs = [("seq", qsortSeq), ("par", qsortPar), ("list", qsortList . fromU), ("vect", qsortVect'),
     ("pt", parMapTest), ("sq", seqMapTest)]

qsortVect':: UArr Double -> Int
qsortVect' xs = lengthU $ toUArrPA $ qsortVect $ fromUArrPA' xs

parMapTest:: UArr Double -> Int
parMapTest xsArr =  (mapUP isPrime xsArr) !: 0
  where
   isPrime x = if (divAny x 2) then 1 else 0
   divAny:: Double -> Int -> Bool
   divAny x y | (floor x) <= y    = False
              | otherwise = (floor x) `mod` y == 0 || divAny x (y+1)

seqMapTest:: UArr Double -> Int
seqMapTest xsArr = sumU $ mapU isPrime xsArr
  where
   isPrime x = if (divAny x 2) then 1 else 0
   divAny:: Double -> Int -> Bool
   divAny x y | (floor x) <= y    = False
              | otherwise = (floor x) `mod` y == 0 || divAny x (y+1)

generateVector :: Int -> IO (Point (UArr Double))
generateVector n =
  do
    evaluate vec
    return $ ("N = " ++ show n) `mkPoint` vec
  where
    vec = toU [1..fromInteger (toInteger n)]

main = ndpMain "QSort"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq"

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []  -> failWith ["No sizes specified"]
                 szs -> do
                          benchmark opts f (map generateVector szs) show
                          return ()

