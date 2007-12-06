import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Prelude
import qualified SMVMPar
import qualified SMVMSeq
import qualified SMVMVect
--import Timing

import System.Console.GetOpt
import System.IO
{-
import System.Exit
import System.Environment  (getArgs)
-}
import Control.Exception   (evaluate)
{-
import System.Mem          (performGC)
-}

import Bench.Benchmark
import Bench.Options

type Alg = SUArr (Int :*: Double) -> UArr Double -> UArr Double

algs = [("smvmp",  SMVMPar.smvm)
       ,("smvms",  SMVMSeq.smvm)
       ,("smvmv",  smvm_vect)
       ]

smvm_vect m v = toUArrPA (SMVMVect.smvm (fromSUArrPA_2 m) (fromUArrPA v))

main = ndpMain "Sparse matrix/vector multiplication"
               "[OPTION] ... FILE ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                      "use the specified algorithm"]
                   "smvmp"

run opts alg files =
  case lookup alg algs of
    Just f  -> procFiles opts f files
    Nothing -> failWith ["Unknown algorithm " ++ alg]

procFiles :: Options -> Alg -> [String] -> IO ()
procFiles opts alg fs =
  do
    benchmark opts
              (uncurry alg)
              (map loadSM fs)
              showRes
    return ()
  where
    arg s = (cols, rows, ratio)
      where
        ((cols,('x':s')):_)  = reads s
        ((rows,('@':s'')):_) = reads s'
        ratio                = read s''

    showRes arr = "sum=" ++ show (sumU arr)

loadSM :: String -> IO (Point (SUArr (Int :*: Double), UArr Double))
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

