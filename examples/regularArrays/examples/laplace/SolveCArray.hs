{-# LANGUAGE BangPatterns #-}

module SolveCArray
	( solveLaplace_stencil
	, relaxLaplace_stencil)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Prelude					as P	hiding (zipWith)
import CArray					as CA
import Array					(Array, DIM2, (:.)(..))


-- | Version of the Laplace solver that calls the relaxation and boundary functions
--	directly, instead of them being passed in as parameters.
solveLaplace_stencil
	:: Int
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

solveLaplace_stencil steps arrBoundMask arrBoundValue arr
	= fromCArray
	$ solveLaplace_stencil' 
		steps
		(toCArray arrBoundMask)
		(toCArray arrBoundValue)
		(toCArray arr)


-- | Solver for the Laplace equation.
--
solveLaplace_stencil'
	:: Int
	-> CArray DIM2 Double
	-> CArray DIM2 Double
	-> CArray DIM2 Double
	-> CArray DIM2 Double

solveLaplace_stencil' steps !arrBoundMask !arrBoundValue arr
 = go steps arr
 where	go s !arr
          | s == 0    = arr
          | otherwise = go (s-1)
		$! forceCArray
			(applyBoundary arrBoundMask arrBoundValue
			$  relaxLaplace_stencil  arr)


-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace_stencil
	:: CArray DIM2 Double
	-> CArray DIM2 Double

{-# INLINE relaxLaplace_stencil #-}
relaxLaplace_stencil arr@(CArray shape@(_ :. n :. m) _)
 = CArray shape
 $ Left 
	(\d@(sh :. i :. j)
	  -> if isBorder d
	     	then arr !: d
	     	else (arr !: (sh :. (i-1) :. j)
		   +  arr !: (sh :. i     :. (j-1))
		   +  arr !: (sh :. (i+1) :. j)
		   +  arr !: (sh :. i     :. (j+1))) / 4) 

 where

	-- | Check if this element is on the border of the matrix.
	--	We can't apply the stencil function here because we don't have the right neighbours.
	isBorder :: DIM2 -> Bool
	isBorder  (_ :. i :. j) 
		=  (i == 0) || (i >= n - 1) 
		|| (j == 0) || (j >= m - 1) 
		

-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: CArray DIM2 Double		-- ^ boundary condition mask
	-> CArray DIM2 Double		-- ^ boundary condition values
	-> CArray DIM2 Double		-- ^ initial matrix
	-> CArray DIM2 Double		-- ^ matrix with boundary conditions applied

{-# INLINE applyBoundary #-}
applyBoundary arrBoundMask arrBoundValue arr
 	= CA.zipWith (+) arrBoundValue
	$ CA.zipWith (*) arrBoundMask  arr


