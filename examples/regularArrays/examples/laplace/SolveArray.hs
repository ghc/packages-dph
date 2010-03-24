
module SolveArray 
	( solve
	, relaxLaplace_shift )
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Array					as A
import Prelude					as P


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
