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

loadSM :: String -> IO (Point (U.Segd, U.Array (Int U.:*: Double), U.Array Double))
loadSM fname =
  do
    h <- openBinaryFile fname ReadMode
    lengths <- U.hGet h
    indices <- U.hGet h
    values  <- U.hGet h
    dv      <- U.hGet h
    let segd = U.lengthsToSegd lengths
        m    = U.zip indices values
    evaluate lengths
    evaluate indices
    evaluate values
    evaluate dv
    -- print (sumU values)
    -- print (sumU dv)
    return $ mkPoint (  "cols=" ++ show (U.length dv) ++ ", "
                     ++ "rows=" ++ show (U.length lengths) ++ ", "
                     ++ "elems=" ++ show (U.length m))
              (segd, m, dv)
