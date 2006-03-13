-- flattened code implemented such that the fusion rules can do their job

module SMVM_fusion (smvm_fusion_bench, toMatVecPA) 
where

-- GHC-specific modules
import Control.Exception (evaluate)

-- tools
import BenchUtils (timeIt)

import PArray


-- The benchmarked code
-- --------------------

-- Sparse matrix and vector representation
--
type SparseMatrix = SPArr (PAProd UInt UDouble) (Int, Double)
type Vector       = PArrDouble

-- The actual benchmark code
--
smvm_fusion :: SparseMatrix -> Vector -> Vector
{-# NOINLINE smvm_fusion #-}
smvm_fusion sm vec =
  let (segd, sm') = flattenP sm 
      (ind , val) = unzipP sm'
  in
  sumSP (segd >: mulV (bpermuteP vec ind) val)
  where
    mulV = zipWithP ((*) :: Double -> Double -> Double)


-- Auxilliary code
-- ---------------

-- Execute and time the benchmark
--
smvm_fusion_bench :: String -> SparseMatrix -> Vector -> IO (Integer, String)
smvm_fusion_bench desc sm vec = timeIt ("PArray w/ fusion " ++ desc) comp
  where
    comp = do  -- this is what is being timed
	     let vec' = smvm_fusion sm vec    -- can't fuse due...
		 res  = sumP vec'	      -- ...to NOINLINE
	     evaluate res
	     return (show res)

-- Dense list-based vectors
--
type VectorL = [Double]

-- List-based sparse matrix representation
--
type SparseVectorL = [(Int, Double)]
type SparseMatrixL = [SparseVectorL]

-- Convert the list representation of a matrix and vector into the `PArray'
-- representation
--
toMatVecPA :: SparseMatrixL -> VectorL -> IO (SparseMatrix, Vector)
toMatVecPA mat vec =
  do
    let segd = map length mat
        mat' = toP segd `segmentP` toP (concat mat)
	vec' = toP vec
    evaluate $ (sumP . mapP fst . snd . flattenP) mat'
    evaluate $ (sumP . mapP snd . snd . flattenP) mat'
    evaluate $ sumP vec'
    return (mat', vec')
