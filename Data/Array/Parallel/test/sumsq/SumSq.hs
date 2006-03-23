-- the infamous sum square fusion example

module Main (main)
where

import Data.Array.Parallel.Unlifted

sumSq :: Int -> Int
{-# NOINLINE sumSq #-}
--sumSq = sumP . mapP (\x -> x * x) . enumFromToP 1
sumSq n = sumU (mapU (\x -> x * x) (enumFromToU 1 n))

main = print $ sumSq 100

