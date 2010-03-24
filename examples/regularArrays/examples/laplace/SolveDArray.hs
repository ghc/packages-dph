
module SolveDArray
	( solve
	, relaxLaplace_shift )
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Prelude					as P
import DArray					as DA
import Array					(Array, DIM2)


-- | Array wrapper for DArray version of solver loop.
solve 	:: (DArray DIM2 Double -> DArray DIM2 Double)
	-> Int
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

solve relaxFn steps arrBoundMask arrBoundValue arr
	= fromDArray
	$ solve' relaxFn steps
		(toDArray arrBoundMask)
		(toDArray arrBoundValue)
		(toDArray arr)
	

-- | Solver loop.
solve' 	:: (DArray DIM2 Double -> DArray DIM2 Double)	-- ^ Relaxation fn to use.
	-> Int 						-- ^ Number of steps to use.
	-> DArray DIM2 Double				-- ^ Boundary condition mask
	-> DArray DIM2 Double				-- ^ Boundary condition value.
	-> DArray DIM2 Double 				-- ^ Initial matrix.
	-> DArray DIM2 Double
	
solve' relaxFn steps arrBoundMask arrBoundValue arr
	| steps == 0	= arr

	| otherwise
	= solve' relaxFn (steps - 1) arrBoundMask arrBoundValue
	$ forceDArray
	$ applyBoundary arrBoundMask arrBoundValue
	$ relaxFn arr


-- | Perform matrix relaxation for the Laplace equation, using shift.
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace_shift 
	:: DArray DIM2 Double 
	-> DArray DIM2 Double

{-# INLINE relaxLaplace_shift #-}
relaxLaplace_shift arr
 = let	shiftu = shift arr 0 ((():*: 1   :*:0)	  :: DIM2)
	shiftd = shift arr 0 ((():*:(-1) :*:0)	  :: DIM2)
	shiftl = shift arr 0 ((():*: 0   :*:1)	  :: DIM2)
 	shiftr = shift arr 0 ((():*: 0   :*:(-1)) :: DIM2)

   in	DA.map (/ 4)
		(DA.zipWith (+)	
			(DA.zipWith (+) shiftu shiftl)
			(DA.zipWith (+) shiftd shiftr))
			

-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: DArray DIM2 Double		-- ^ boundary condition mask
	-> DArray DIM2 Double		-- ^ boundary condition values
	-> DArray DIM2 Double		-- ^ initial matrix
	-> DArray DIM2 Double		-- ^ matrix with boundary conditions applied

{-# INLINE applyBoundary #-}
applyBoundary arrBoundMask arrBoundValue arr
 	= DA.zipWith (+) arrBoundValue
	$ DA.zipWith (*) arrBoundMask  arr
