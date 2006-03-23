-- recursion-based H98 array code

module SMVM_array_rec (smvm_array_rec_bench) 
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
smvm_array_rec :: SparseMatrix -> Vector -> Vector
{-# NOINLINE smvm_array_rec #-}
smvm_array_rec sm vec = fmap (sumRow . elems) sm
  where
    sumRow []             = 0
    sumRow ((i, v) : row) = vec!i * v + sumRow row


-- Auxilliary code
-- ---------------

-- Execute and time the benchmark
--
smvm_array_rec_bench :: String -> SparseMatrix -> Vector 
		     -> IO (Integer, String)
smvm_array_rec_bench desc sm vec = 
  timeIt ("H98 Array w/ recursion " ++ desc) comp
  where
    comp = do  -- this is what is being timed
	     let vec' = smvm_array_rec sm vec    -- can't fuse due...
		 res  = sum (elems vec')         -- ...to NOINLINE
	     evaluate res
	     return (show res)
