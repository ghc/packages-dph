{-# OPTIONS -fglasgow-exts #-}

import qualified Data.Array.Parallel.Unlifted 	as U
import Array					as A
import qualified SolveCArray			as CA
import Data.List				as L
import Data.Maybe
import Bench.Benchmark ( time, showTime )
import Matrix
import System.Environment
import Control.Monad


-- Solvers ----------------------------------------------------------------------------------------
type Solver
	=  Array DIM2 Double		-- ^ First matrix.
	-> Array DIM2 Double		-- ^ Second matrix.
	-> Array DIM2 Double 		-- ^ Product matrix.

algorithms :: [(String, Solver)]
algorithms
  =	[ ("carray-replicate",		  CA.wrapCArraySolver CA.mmMult_replicate) ]


-- Arg Parsing ------------------------------------------------------------------------------------
data Arg
	= ArgSolver       String
	| ArgMatrixRandom Int Int
	| ArgMatrixFile   FilePath
	deriving Show

isArgMatrix arg
 = case arg of
	ArgMatrixRandom{}	-> True
	ArgMatrixFile{}		-> True
	_			-> False

parseArgs []		= []
parseArgs (flag:xx)
	| "-solver"	<- flag
	, s:rest	<- xx
	= ArgSolver s	: parseArgs rest
	
	| "-file"	<- flag
	, f:rest	<- xx
	= ArgMatrixFile f : parseArgs rest
	
	| "-random"	<- flag
	, x:y:rest	<- xx
	= ArgMatrixRandom (read x) (read y) : parseArgs rest
	
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"


-- Get Matrices -----------------------------------------------------------------------------------
getMatrix arg
 = case arg of
	ArgMatrixFile   fileName	-> readMatrixFromTextFile fileName
	ArgMatrixRandom height width	-> genRandomMatrix (() :. height :. width)

			
-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	
	(args :: [Arg])	<- liftM parseArgs $ getArgs

	-- get solver
	let [solverName] = [s | ArgSolver s <- args]
	let (solver :: Solver)	
		= fromMaybe (badSolver solverName)
		$ lookup solverName algorithms
			
	-- get matrices
	let [argMat1, argMat2]	= filter isArgMatrix args
	mat1		<- getMatrix argMat1
	mat2		<- getMatrix argMat2

        mat1
          `deepSeqArray` mat2
          `deepSeqArray` return ()
	
	(matResult, t)	<- time
			$  let matResult = solver mat1 mat2
			   in  matResult `deepSeqArray` return matResult

	putStrLn (showTime t)
			
--	print	$ U.toList $ A.fromArray matResult
--	print 	$ "wibble\n"
	
	
printHelp
 =	putStr 	$ unlines
		[ "Usage: mmult <solver> <matrix1.mat> <matrix2.mat> <output.mat>"
		, ""
		, "  solvers:\n" 
		  ++ (concat $ intersperse "\n" ["     " ++ name | (name, _) <- algorithms])
		, "" 
		, "  Format of matrix file:"
		, "    MATRIX"
		, "    <width> <height>"
		, "    <whitespace separated values..>"
		, "" ]
		
badSolver solverName
	= error 
	$ unlines
	[ "unknown solver: " ++ solverName
	, "choose one of: "  ++ show (L.map fst algorithms) ]






