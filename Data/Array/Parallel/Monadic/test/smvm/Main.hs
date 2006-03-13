-- Driver for the benchmarking of sparse matrix vector multiplication
--

module Main (main)
where

-- standard libraries
import List    (nub, sort)
import Random  (randomRs, newStdGen)
import System

-- GHC-specific modules
import Control.Exception (evaluate)

-- tools
import BenchUtils

-- friends
import SMVM_array_compr (smvm_array_compr_bench, toMatVecA)
import SMVM_array_rec   (smvm_array_rec_bench)
import SMVM_fusion      (smvm_fusion_bench, toMatVecPA)
import SMVM_optimal     (smvm_optimal_bench)


-- Benchmark parameters
-- --------------------

-- Non-zero elements
--
size :: Int
size = 640000

-- Benchmarked matrix densities (where 1 ~ dense matrix)
--
densities :: [Float]
densities = [0.001, 0.01, 0.1, 1]


-- Matrix and vector types
-- -----------------------

-- Dense list-based vectors
--
type VectorL = [Double]

-- List-based sparse matrix representation
--
type SparseVectorL = [(Int, Double)]
type SparseMatrixL = [SparseVectorL]


-- Matrix and vector generation
-- ----------------------------

-- Produce a random sparse matrix and vector, which are guaranteed to be
-- already evaluated 
--
-- * The size determines the number of non-zero elements, not the actual
--   matrix dimensions
--
generateMatVecL :: Int -> Float -> IO (SparseMatrixL, VectorL)
generateMatVecL size density =
  do
    let absSize = round (sqrt (fromIntegral size / density))
    mat <- randomSparseMatrixL density absSize
    vec <- randomVectorL               absSize
    evaluate $ sum (map (sum . map fst) mat)
    evaluate $ sum (map (sum . map snd) mat)
    evaluate $ sum vec
    return (mat, vec)

-- given a matrix size and matrix density, generate a random sparse matrix
--
randomSparseMatrixL              :: Float -> Int -> IO SparseMatrixL
randomSparseMatrixL density size  =
  mapM (randomSparseVectorL density) (replicate size size)

-- given a vector size and density, generate a random sparse vector
--
randomSparseVectorL              :: Float -> Int -> IO SparseVectorL
randomSparseVectorL density size  
  | density > 0.1 =		  -- too slow for sparse vectors
  do
    rgen1 <- newStdGen
    rgen2 <- newStdGen
    let randomElems = randomRs (-100.0, 100) rgen1
	oracles     = randomRs (0.0   , 1.0) rgen2
    return $ 
      zip
	[i | (i, oracle) <- zip [0..size - 1] oracles, oracle <= density]
	randomElems
  | otherwise =
  do
    rgen1 <- newStdGen
    rgen2 <- newStdGen
    let randomElems = randomRs (-100.0, 100     ) rgen1
	indicies    = randomRs (0     , size - 1) rgen2
	is          = take (round ((fromIntegral size) * density)) indicies
    return $ zip (nub . sort $ is) randomElems

-- given a size, generate a random vector
--
randomVectorL      :: Int -> IO VectorL
randomVectorL size  =
  do
    rgen <- newStdGen
    return $ take size (randomRs (-100.0, 100) rgen)


-- Toplevel driver
-- ---------------

main :: IO ()
main =
  do
    host <- getEnv "HOSTNAME"
    putStrLn "Sparse Matrix Vector Multiplication Benchmark"
    putStrLn "============================================="

    matVecL  <- mapM (generateMatVecL size) densities
    matVecA  <- mapM (uncurry toMatVecA ) matVecL
    matVecPA <- mapM (uncurry toMatVecPA) matVecL

    doBenchmarks [
		  (indexedBenchmark smvm_array_compr_bench matVecA, 
		   "SMVM: H98 arrays w/ comprehensions",
		   "array_compr"),
		  (indexedBenchmark smvm_array_rec_bench matVecA, 
		   "SMVM: H98 arrays w/ recursion",
		   "array_compr"),
		  (indexedBenchmark smvm_fusion_bench matVecPA, 
		   "SMVM: fusion-enabled PArray version",
		   "fusion"),
		  (indexedBenchmark smvm_optimal_bench matVecPA, 
		   "SMVM: manually optimised PArray version",
		   "optimal")
		 ]
		 [0..length densities - 1]
		 ("SMVM with " ++ show size ++ " non-zero elements and \n\
		  \densities " ++ show densities ++ " on " ++ host)
		 "smvm"

    putStrLn "\nFinished."
  where
    -- convert an benchmark expecting a matrix and vector into an indexed
    -- benchmark 
    --
    indexedBenchmark :: (String -> a -> b -> IO (Integer, String)) 
		     -> [(a, b)]
		     -> Int -> IO (Integer, String)
    indexedBenchmark bench matVecs idx = 
      uncurry (bench desc) (matVecs!!idx)
      where
        desc = "[nonzero = " ++ show size ++ "; density = " ++ 
	       show (densities!!idx) ++ "]"
