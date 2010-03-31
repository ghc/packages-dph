module Main
where

import FFTCArray

import CArray
import Array ( (:.)(..) )

import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark ( timeFn_, showTime )
import Data.Char ( toLower )
import System.Environment ( getArgs )


fft :: (Int -> CArray DIM3 Complex -> CArray DIM3 Complex)
    -> (Int, U.Array Double, U.Array Double) -> U.Array Double
{-# NOINLINE fft #-}
fft f (n, xs, ys) = U.snds
                  $ carrayData
                  $ f 1
                  $ mkCArray (() :. n :. n :. n)
                  $ U.zip xs ys

algs = [('d', fft3D),
        ('s', fft3DS),
        ('c', fft3DC)]



main = do
         [[c],s] <- getArgs
         let n = read s
             xs = U.map fromIntegral
                $ U.enumFromTo 1 (n*n*n)
             fn = case lookup (toLower c) algs of
                    Just f  -> f
                    Nothing -> error $ "Unknown algorithm " ++ [c]
         xs `seq` fn `seq` return ()
         t <- timeFn_ (fft fn) (`seq` ()) (n,xs,xs)
         putStrLn (showTime t)
         
