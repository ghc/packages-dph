{-# LANGUAGE TypeOperators #-}

import SMVMVect (smvm)

import Control.Exception (evaluate)
import System.IO

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude

import Bench.Benchmark
import Bench.Options


main = ndpMain "Sparse matrix/vector multiplication (vectorised)"
               "[OPTION] ... FILES ..."
               run [] ()

run opts () files
  = do
      benchmark opts smvm
                (map loadSM files)
                showRes
      return ()
  where
    showRes arr = "sum = " ++ show (U.sum arr)

loadSM :: String -> IO (Point (U.SArray (Int U.:*: Double), U.Array Double))
loadSM s@('(' : _) =
  case reads s of
    [((lm,lv), "")] -> return $ mkPoint "input" (U.fromList_s lm, U.fromList lv)
    _         -> failWith ["Invalid data " ++ s]
loadSM fname =
  do
    h <- openBinaryFile fname ReadMode
    lengths <- U.hGet h
    indices <- U.hGet h
    values  <- U.hGet h
    dv      <- U.hGet h
    let sm = U.lengthsToSegd lengths U.>: U.zip indices values
    return (sm, values)
    evaluate lengths
    evaluate indices
    evaluate values
    evaluate dv
    -- print (sumU values)
    -- print (sumU dv)
    return $ mkPoint (  "cols=" ++ show (U.length dv) ++ ", "
                     ++ "rows=" ++ show (U.length_s sm) ++ ", "
                     ++ "elems=" ++ show (U.length (U.concat sm)))
              (sm,dv)
