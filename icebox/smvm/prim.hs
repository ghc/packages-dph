{-# LANGUAGE TypeOperators #-}

import SMVMPrim (smvm)

import Control.Exception (evaluate)
import System.IO

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.PArray as P

import Bench.Benchmark
import Bench.Options


main = ndpMain "Sparse matrix/vector multiplication (primitives)"
               "[OPTION] ... FILES ..."
               run [] ()

run opts () files
  = do
      benchmark opts (\(segd, m, v) -> smvm segd m v)
                (map loadSM files)
                (`seq` ()) showRes
      return ()
  where
    showRes arr = "sum = " ++ show (U.sum arr)

loadSM :: String -> IO (Point (U.Segd, U.Array (Int, Double), U.Array Double))
loadSM fname 
 = do
    h <- openBinaryFile fname ReadMode

    -- number of elements in each row of the matrix.
    lengths <- U.hGet h

    -- indices of all the elements.
    indices <- U.hGet h

    -- values of the matrix elements.
    values  <- U.hGet h

    -- the dense vector.
    vector  <- U.hGet h


    let segd    = U.lengthsToSegd lengths
        matrix  = U.zip indices values

    evaluate lengths
    evaluate indices
    evaluate values
    evaluate vector

    print (U.sum values)
    print (U.sum vector)

    return $ mkPoint (  "cols =" ++ show (U.length vector)  ++ ", "
                     ++ "rows =" ++ show (U.length lengths) ++ ", "
                     ++ "elems=" ++ show (U.length matrix))
             (segd, matrix, vector)
