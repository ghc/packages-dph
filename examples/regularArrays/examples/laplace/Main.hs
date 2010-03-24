
-- | Solver for the Laplace equation
--	Writes a file "out.ppm" of the complete solution.
--	You can use the ImageMagick convert program to make a png
--	with	"convert out.ppm out.png"
--
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Array					as Array
import Array					as A
import Prelude					as P
import Data.List				as L
import PPM
import System.Environment


main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [size, steps]	
	   ->	laplace (read size) (read steps)

	  _ -> do
		putStr "Usage: laplace <matrix dim> <iterations>\n"
		return ()


laplace :: Int -> Int -> IO ()
laplace size steps	
 = let
	-- The width and height of the matrix
	shape	= () :*: size :*: size
	
	-- Make matricies for boundary conditions
	arrBoundMask	= createMatrix shape (mkBoundaryMask size)
	arrBoundValue	= createMatrix shape (mkBoundaryValue size)

	-- Use the boundary condition values as the initial matrix
	arrInitial	= arrBoundValue
		
	-- Start with the 
	arrFinal	= solve steps 
				relaxLaplace_shift 
				arrBoundMask 
				arrBoundValue 
				arrInitial
			
	-- Write out the matrix as a colorised PPM image	
   in	writeMatrixAsNormalisedPPM
		"out.ppm"
		(rampColorHotToCold 0.0 1.0)
		arrFinal


-- | Solver loop.
solve 	:: Int 						-- ^ Number of steps to use.
	-> (Array DIM2 Double -> Array DIM2 Double)	-- ^ Relaxation fn to use.
	-> Array DIM2 Double				-- ^ Boundary condition mask
	-> Array DIM2 Double				-- ^ Boundary condition value.
	-> Array DIM2 Double 				-- ^ Initial matrix.
	-> Array DIM2 Double
	
solve steps relax arrBoundMask arrBoundValue arr
	| steps == 0	= arr

	| otherwise
	= solve (steps - 1) relax arrBoundMask arrBoundValue
	$ applyBoundary arrBoundMask arrBoundValue
	$ relax arr
	

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


-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: Array DIM2 Double		-- ^ boundary condition mask
	-> Array DIM2 Double		-- ^ boundary condition values
	-> Array DIM2 Double		-- ^ initial matrix
	-> Array DIM2 Double		-- ^ matrix with boundary conditions applied

applyBoundary arrBoundMask arrBoundValue arr
 	= A.zipWith (+) arrBoundValue
	$ A.zipWith (*) arrBoundMask  arr


-- Relaxation -------------------------------------------------------------------------------------
-- | Perform matrix relaxation for the Laplace equation, using shift.
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace_shift 
	:: Array DIM2 Double 
	-> Array DIM2 Double

{-# INLINE relaxLaplace_shift #-}
relaxLaplace_shift arr
 = let	shiftu = shift arr 0 ((():*: 1   :*:0)	  :: DIM2)
	shiftd = shift arr 0 ((():*:(-1) :*:0)	  :: DIM2)
	shiftl = shift arr 0 ((():*: 0   :*:1)	  :: DIM2)
 	shiftr = shift arr 0 ((():*: 0   :*:(-1)) :: DIM2)

   in	A.map (/ 4)
		(A.zipWith (+)	
			(A.zipWith (+) shiftu shiftl)
			(A.zipWith (+) shiftd shiftr))
