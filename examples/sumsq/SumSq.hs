-- the infamous sum square fusion example

module Main (main)
where

import Data.Array.Parallel.Unlifted as U

sumSq :: Int -> Int
{-# NOINLINE sumSq #-}
sumSq n = U.sum (U.map (\x -> x * x) (U.enumFromTo 1 n))

main = print $ sumSq 100

