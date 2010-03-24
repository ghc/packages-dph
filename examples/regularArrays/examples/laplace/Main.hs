
-- | Solver for the Laplace equation
--	Writes a file "out.ppm" of the complete solution.
--	You can use the ImageMagick convert program to make a png
--	with	"convert out.ppm out.png"
--
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Prelude					as P
import Data.List				as L
import Data.Maybe
import System.Environment

import PPM
import Array					as A
import SolveArray				as A
import DArray					as DA
import SolveDArray				as DA


-- Solvers ----------------------------------------------------------------------------------------
type Solver
	= Int 				-- ^ Number of steps to use.
	-> Array DIM2 Double		-- ^ Boundary condition mask
	-> Array DIM2 Double		-- ^ Boundary condition value.
	-> Array DIM2 Double 		-- ^ Initial matrix.
	-> Array DIM2 Double

algorithms
  =	[ ("array-shift",	 A.solve  A.relaxLaplace_shift)
	, ("darray-shift",	DA.solve DA.relaxLaplace_shift) ]


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [solverName, size, steps, fileName]	
	   -> do
		let badSolver
			= error 
			$  "unknown solver: " ++ solverName ++ "\n"
			++ "choose one of: " ++ show (L.map fst algorithms) ++ "\n"

		let solver	
			= fromMaybe badSolver						
			$ lookup solverName algorithms
			
		laplace solver (read size) (read steps) fileName

	  _ -> do
		putStr 	$ unlines
			[ "Usage: laplace <solver name> <matrix dim> <iterations> <output file.ppm>"
			, "  solver names: " ++ (concat $ intersperse ", " $ L.map fst algorithms)
			, "  matrix dim  :: Int     Both the width and height of the matrix"
			, "  iterations  :: Int     Number of iterations to use in the solver"
			, "" ]
			
		return ()


laplace :: Solver -> Int -> Int -> FilePath -> IO ()
laplace solver size steps fileName
 = let
	-- The width and height of the matrix
	shape	= () :*: size :*: size
	
	-- Make matricies for boundary conditions
	arrBoundMask	= createMatrix shape (mkBoundaryMask size)
	arrBoundValue	= createMatrix shape (mkBoundaryValue size)

	-- Use the boundary condition values as the initial matrix
	arrInitial	= arrBoundValue
		
	-- Start with the 
	arrFinal	= solver
				steps
				arrBoundMask 
				arrBoundValue 
				arrInitial
			
	-- Write out the matrix as a colorised PPM image	
   in	writeMatrixAsNormalisedPPM
		fileName
		(rampColorHotToCold 0.0 1.0)
		arrFinal

	
-- Initial Value ----------------------------------------------------------------------------------
-- | Make the initial value for the matrix.
mkInitialValue :: Int -> DIM2 -> Double
mkInitialValue _ _
	= 0


-- Boundary Conditions ----------------------------------------------------------------------------
-- | Make the mask for the boundary conditions.
--	Should return 0 when the point is part of the boundary, and 1 otherwise.
mkBoundaryMask :: Int -> DIM2 -> Double
mkBoundaryMask size (() :*: x :*: y)	
	| x == 0		= 0
	| y == 0		= 0
	| x >= (size - 1)	= 0
	| y >= (size - 1)	= 0
	| otherwise		= 1


-- | Make the values for the boundary conditions.
--	Should return 0 where the point is not part of the boundary.
mkBoundaryValue :: Int -> DIM2 -> Double
mkBoundaryValue size (() :*: x :*: y)
	| x == 0 && y > 0 && y < n 	= 80
	| y == 0 && x > 0 && x < n	= 20
	| x == n && y > 0 && y < n 	= 0
	| y == n && x > 0 && x < n	= 180
	| otherwise			= 0
	where	n	= size - 1



---------------------------------------------------------------------------------------------------
-- | Given a function that produces each element, 
--	create a matrix of a given size.
createMatrix 
	:: DIM2 			-- ^ Size of matrix.
	-> (DIM2 -> Double) 		-- ^ fn to produce each element.
	-> Array DIM2 Double

createMatrix dim mkElem
 = let	() :*: width :*: height	= dim
	arrList	= [mkElem (() :*: x :*: y)
			| y <- [0 .. (width - 1)]
			, x <- [0 .. (height - 1)]]
   in	toArray dim $ U.fromList arrList




