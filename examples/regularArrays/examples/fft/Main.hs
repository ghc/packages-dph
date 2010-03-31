module Main
where

import FFTCArray ( fft3DC )

import CArray
import Array ( (:.)(..) )

import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark ( timeFn_, showTime )

import System.Environment ( getArgs )


fft :: (Int, U.Array Double, U.Array Double) -> U.Array Double
{-# NOINLINE fft #-}
fft (n, xs, ys) = U.snds
                $ carrayData
                $ fft3DC 1
                $ mkCArray (() :. n :. n :. n)
                $ U.zip xs ys

main = do
         [s] <- getArgs
         let n = read s
             xs = U.map fromIntegral
                $ U.enumFromTo 1 (n*n*n)
         xs `seq` return ()
         t <- timeFn_ fft (`seq` ()) (n,xs,xs)
         putStrLn (showTime t)
         
