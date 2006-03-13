-- Manually optimised loop code using PArrays

module SMVM_optimal (smvm_optimal_bench) 
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
smvm_optimal :: SparseMatrix -> Vector -> Vector
{-# NOINLINE smvm_optimal #-}
smvm_optimal sm vec = 
  loopAccS $ loopSP (foldEFL combine) (keepSFL (const 0)) 0 sm
  where
    combine :: Double -> (Int, Double) -> Double
    combine acc (i, y) = vec!:i * y + acc


-- Auxilliary code
-- ---------------

-- Execute and time the benchmark
--
smvm_optimal_bench :: String -> SparseMatrix -> Vector -> IO (Integer, String)
smvm_optimal_bench desc sm vec = 
  timeIt ("PArray manually optimised " ++ desc) comp
  where
    comp = do  -- this is what is being timed
	     let vec' = smvm_optimal sm vec    -- can't fuse due...
		 res  = sumP vec'	       -- ...to NOINLINE
	     evaluate res
	     return (show res)
