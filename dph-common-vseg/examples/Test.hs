
module Test where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Nested


ex_1 = replicatePR 10 (5 :: Int)

tagsI5 = [0, 1, 0, 1, 0 :: Int]

arrI1  = fromListPA [0 :: Int]
arrI3  = fromListPA [1, 2, 3 :: Int]
arrI5  = fromListPA [5, 6, 7, 8, 9 :: Int]
arrI5' = fromListPA [1, 2, 3, 4, 5 :: Int]

arrI7 = fromListPA [7, 8, 9, 10, 11, 12, 13 :: Int]

arrN1  = fromListPA [arrI5]
arrN2  = fromListPA [arrI1, arrI3]
arrN3  = fromListPA [arrI1, arrI3, arrI5]
arrN4  = fromListPA [arrI7,  arrI1, arrI3, arrI1]
arrN4' = fromListPA [arrI5', arrI3, arrI7, arrI3]
arrN7  = fromListPA [arrI7, arrI1, arrI3, arrI1, arrI5, arrI1, arrI3]

arrN7' = replicatesPA' [2, 2, 4, 2, 3, 4, 2] arrN7

tagsN7 = [1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1:: Int]

arrM3  = fromListPA [arrN2, arrN1, arrN7]
arrM3' = fromListPA [arrN4, arrN2, arrN3]

arrM5   = fromListPA    [arrN4, arrN2, arrN3, arrN1, arrN4']
arrM5'  = replicatesPA' [3, 1, 2] arrM3

