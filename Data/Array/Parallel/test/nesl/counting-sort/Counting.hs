import Data.Array.Parallel.Unlifted
import Bench

-- ----------------------------------------------------------------------
-- counting sort from http://www.cs.cmu.edu/~scandal/nesl/algorithms.html
-- ----------------------------------------------------------------------
position :: (UA a, Ord a) => a -> UArr a -> Int
position x = lengthU' . filterU (<x)

counting :: (UA a, Ord a) => UArr a -> UArr a
{-# NOINLINE counting #-}
counting a =
  let ai = zipU a (enumFromToU 0 (lengthU a))
  in
  permuteU a $ mapU (`position` ai) ai
-- ----------------------------------------------------------------------

main = benchmarks "Counting sort"
                  (counting :: UArr Int -> UArr Int)
                  (minBound,maxBound)

