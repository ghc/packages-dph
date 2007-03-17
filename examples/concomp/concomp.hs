import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed
import Graph
import qualified AwShU
import qualified AwShUP
import qualified HybU
import qualified HybUP

import System.Console.GetOpt
import System.IO
import Control.Exception   (evaluate)

import Bench.Benchmark
import Bench.Options


type Alg = UArr (Int :*: Int) -> Int -> Int :*: UArr Int

algs = [("awshu",  AwShU.aw_connected_components)
       ,("awshup", AwShUP.aw_connected_components)
       ,("hybu",   HybU.hybrid_connected_components)
       ,("hybup",  HybUP.hybrid_connected_components)
       ]

main = ndpMain "Connected components"
               "[OPTION] ... FILES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                      "use the specified algorithm"]
                   "<none>"

run opts alg files =
  case lookup alg algs of
    Just f  -> procFiles opts f files
    Nothing -> failWith ["Unknown algorithm " ++ alg]

procFiles :: Options -> Alg -> [String] -> IO ()
procFiles opts alg fs =
  do
    benchmark opts (uncurry alg)
              (map loadGraph $ files fs)
              showRes
    return ()
  where
    files [] = [""]
    files fs = fs

    showRes (r :*: _) = "d=" ++ show r

loadGraph :: String -> IO (Point (UArr (Int :*: Int), Int))
loadGraph fname =
  do
    s <- if null fname then getContents else readFile fname
    let g = read s
    evaluate (edges g)
    return $ mkPoint (  "n=" ++ show (nodeCount g) ++ ", "
                     ++ "e=" ++ show (edgeCount g))
              (edges g, nodeCount g)

