-- comprehension-based H98 array code

module SMVM_array_compr (smvm_array_compr_bench, toMatVecA) 
where

-- standard libraries
import Array

-- GHC-specific modules
import Control.Exception (evaluate)

-- tools
import BenchUtils (timeIt)


-- The benchmarked code
-- --------------------

-- Dense array-based vectors
--
type Vector = Array Int Double

-- Array-based sparse matrix representation
--
type SparseVector = Array Int (Int, Double)
type SparseMatrix = Array Int SparseVector

-- The actual benchmark code
--
smvm_array_compr :: SparseMatrix -> Vector -> Vector
{-# NOINLINE smvm_array_compr #-}
smvm_array_compr sm vec =
  listArray bnds 
    [sum [x * (vec!col) | (col, x) <- elems row] | row <- elems sm]
  where
    bnds = bounds sm


-- Auxilliary code
-- ---------------

-- Execute and time the benchmark
--
smvm_array_compr_bench :: String -> SparseMatrix -> Vector 
		       -> IO (Integer, String)
smvm_array_compr_bench desc sm vec = 
  timeIt ("H98 Array w/ compr " ++ desc) comp
  where
    comp = do  -- this is what is being timed
	     let vec' = smvm_array_compr sm vec    -- can't fuse due...
		 res  = sum (elems vec')           -- ...to NOINLINE
	     evaluate res
	     return (show res)

-- Dense list-based vectors
--
type VectorL = [Double]

-- List-based sparse matrix representation
--
type SparseVectorL = [(Int, Double)]
type SparseMatrixL = [SparseVectorL]

-- Convert the list representation of a matrix and vector into the `Array'
-- representation
--
toMatVecA :: SparseMatrixL -> VectorL -> IO (SparseMatrix, Vector)
toMatVecA mat vec =
  do
    let segd = map length mat
        mat' = listArray (0, length mat - 1) (map toArr mat)
	vec' = toArr vec
    evaluate $ sum (elems (fmap (sum . elems . fmap fst) mat'))
    evaluate $ sum (elems (fmap (sum . elems . fmap snd) mat'))
    evaluate $ sum (elems vec')
    return (mat', vec')
  where
    toArr l = listArray (0, length l - 1) l
