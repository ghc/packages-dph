import Control.Exception (evaluate)
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted

import Bench.Benchmark
import Bench.Options

import qualified H98
import qualified Seq

type Alg = Int -> ()

seqList :: [Int] -> ()
seqList [] = ()
seqList (x:xs) = x `seq` seqList xs

algs = [("h98",  seqList . H98.primes)
       ,("seq",  \n -> Seq.primes n `seq` ())
       ]

main = ndpMain "Sieve of Eratosthenes"
               "[OPTION] ... N ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                      "use the specified algorithm"]
                   "seq"

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm " ++ alg]
    Just f  -> case map read sizes of
                 []  -> failWith ["No sizes specified"]
                 ns  -> do
                          benchmark opts f
                            (map (return . labelPoint showN) ns)
                            (const "")
                          return ()
  where
    showN n = "N=" ++ show n
 
