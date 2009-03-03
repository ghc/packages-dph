import SMVMPrim (smvm)

import Control.Exception (evaluate)

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Prelude

import Bench.Benchmark
import Bench.Options


main = ndpMain "Sparse matrix/vector multiplication (primitives)"
               "[OPTION] ... FILES ..."
               run [] ()

run opts () files
  = do
      benchmark opts smvm
                (map loadSM files)
                showRes
      return ()
  where
    showRes arr = "sum = " ++ show (sumU arr)

loadSM :: String -> IO (Point (SUArr (Int :*: Double), UArr Double))
loadSM s@('(' : _) =
  case reads s of
    [((lm,lv), "")] -> return $ mkPoint "input" (toSU lm, toU lv)
    _         -> failWith ["Invalid data " ++ s]
loadSM fname =
  do
    h <- openBinaryFile fname ReadMode
    lengths <- hGetU h
    indices <- hGetU h
    values  <- hGetU h
    dv      <- hGetU h
    let sm = lengthsToUSegd lengths >: zipU indices values
    return (sm, values)
    evaluate lengths
    evaluate indices
    evaluate values
    evaluate dv
    -- print (sumU values)
    -- print (sumU dv)
    return $ mkPoint (  "cols=" ++ show (lengthU dv) ++ ", "
                     ++ "rows=" ++ show (lengthSU sm) ++ ", "
                     ++ "elems=" ++ show (lengthU (concatSU sm)))
              (sm,dv)
