import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Distributed
import Graph
import qualified AwShU
import qualified AwShUP
import qualified HybU
import qualified HybUP
import Timing

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment  (getArgs)
import Control.Exception   (evaluate)
import System.Mem          (performGC)

import Bench

type Alg = UArr (Int :*: Int) -> Int -> Int :*: UArr Int

algs = [("awshu",  AwShU.aw_connected_components)
       ,("awshup", AwShUP.aw_connected_components)
       ,("hybu",   HybU.hybrid_connected_components)
       ,("hybup",  HybUP.hybrid_connected_components)
       ]

data Opts = Opts { optAlg       :: String
                 , optRuns      :: Int
                 , optVerbosity :: Int
                 , optSetGang   :: IO ()
                 }

dftOpts = Opts { optAlg       = "<none>"
               , optRuns      = 1
               , optVerbosity = dftVerbosity
               , optSetGang   = setSequentialGang 1
               }

opts = [Option ['a'] ["alg"]
          (ReqArg (\s o -> o { optAlg = s }) "ALG")
          "use the specified algorithm"
       ,Option ['r'] ["runs"]
          (ReqArg (\s o -> o { optRuns = read s }) "N")
          "repeat each benchmark N times"
       ,Option ['t'] ["threads"]
          (ReqArg (\s o -> o { optSetGang = setGang (read s)}) "N")
          "use N threads"
       ,Option ['s'] ["seq"]
          (OptArg (\r o -> let n = case r of
                                     Nothing -> 1
                                     Just s  -> read s
                           in o { optSetGang = setSequentialGang n}) "N")
          "simulate N threads (default 1)"
       ,Option ['v'] ["verbose"]
          (OptArg (\r o -> let n = case r of
                                     Nothing -> dftVerbosity
                                     Just s  -> read s
                           in o { optVerbosity = n }) "N")
          "verbosity level"
       ]
          

measure :: Alg -> UArr (Int :*: Int) -> Int -> IO (Time,Int)
measure alg es n =
  es `seq`
  do
    performGC
    start <- getTime
    let r :*: cs = alg es n
    evaluate cs
    end <- getTime
    return (end `minusT` start,r)
    

main =
  do
    args <- getArgs
    case getOpt Permute opts args of
      (fs, files, []) -> let os = foldr ($) dftOpts fs
                         in
                         case lookup (optAlg os) algs of
                           Just alg -> do
                                         optSetGang os
                                         procFiles os alg files
                           Nothing  -> err ["Unknown algorithm " ++ optAlg os]
      (_, _, errs)    -> err errs
  where
    err ss = do
               mapM_ (hPutStrLn stderr) ss
               exitFailure

{-
procFiles :: Opts -> Alg -> [String] -> IO ()
procFiles os alg [] = do
                        s <- getContents
                        process os alg "<stdin>" s
procFiles os alg fs = mapM_ (\f -> do
                                    s <- readFile f
                                    process os alg f s) fs

process :: Opts -> Alg -> String -> String -> IO ()
process os alg fname s =
  do
    let g = read s
    putStr $ concat [ "n=" ++ show (nodeCount g) ++ ", "
                    , "e=" ++ show (edgeCount g)
                    , ": "
                    ]
    hFlush stdout
    (t,r) <- measure alg (edges g) (nodeCount g)
    putStrLn $ concat [ show (milliseconds $ clockTime t) ++ "ms"
                      , " (d=" ++ show r ++ ")"
                      ]
    hFlush stdout
-}

procFiles :: Opts -> Alg -> [String] -> IO ()
procFiles os alg fs =
  do
    benchmark (dftBenchOpts { runsB      = optRuns os
                            , verbosityB = optVerbosity os })
              (uncurry alg)
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

