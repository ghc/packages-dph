module Bench
where

import System.IO
import System.Exit
import Control.Exception (evaluate)
import System.Mem        (performGC)

import Timing

data BenchOpts = BenchOpts { runsB      :: Int
                           , verbosityB :: Int
                           }

dftBenchOpts = BenchOpts { runsB      = 1
                         , verbosityB = 1
                         }

dftVerbosity :: Int
dftVerbosity = 1

showTime :: Time -> String
showTime t = (show . milliseconds $ cpuTime t)
          ++ "/"
          ++ (show . milliseconds $ clockTime t)

showTimes :: [Time] -> String
showTimes ts = unwords [ showTime (minimumT ts)
                       , showTime (averageT ts)
                       , showTime (maximumT ts)
                       ]
              

say :: String -> IO ()
say s = do
          hPutStr stdout s
          hFlush stdout

sayLn :: String -> IO ()
sayLn s = do
            hPutStrLn stdout s
            hFlush stdout

time :: BenchOpts -> (a -> b) -> a -> (b -> String) -> IO [Time]
time o f x outp = sequence $ map time1 [1 .. runsB o]
  where
    time1 n =
      do
        sayRun n
        performGC
        (x,t) <- timeIO $ eval f x n
        sayResult $ showTime t ++ res (outp x)
        return t

    sayRun n | verbosityB o < 2  = return ()
             | verbosityB o == 2 = say $ "."
             | otherwise         = say $ "  run " ++ show n ++ ": "

    sayResult s | verbosityB o < 3 = return ()
                | otherwise        = sayLn s

    res s | not (null s) && verbosityB o > 2 = " (" ++ s ++ ")"
          | otherwise                        = ""

eval :: (a -> b) -> a -> Int -> IO b
{-# NOINLINE eval #-}
eval f x n = evaluate (f x)


data Point a = Point String a

point :: Show a => a -> Point a
point = labelPoint show

labelPoint :: (a -> String) -> a -> Point a
labelPoint f x = Point (f x) x

mkPoint :: String -> a -> Point a
mkPoint s x = Point s x

benchmark :: BenchOpts
          -> (a -> b)
          -> [IO (Point a)]
          -> (b -> String)
          -> IO [[Time]]
benchmark o f ps outp = mapM bench1 ps
  where
    sayPoint s | verbosityB o == 0 = return ()
               | verbosityB o == 1 = say $ s ++ ": "
               | verbosityB o == 2 = say $ s ++ " "
               | otherwise         = sayLn $ s ++ " ..."

    sayResult s
      | verbosityB o == 0 = return ()
      | verbosityB o == 1 = sayLn s
      | verbosityB o == 2 = sayLn $ " " ++ s
      | otherwise         = sayLn $ "... " ++ s

    bench1 p =
      do
        Point s x <- p
        sayPoint s
        ts <- time o f x outp
        sayResult $ showTimes ts
        return ts

