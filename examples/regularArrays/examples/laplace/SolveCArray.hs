

module SolveCArray
	( solveLaplace_stencil
	, relaxLaplace_stencil)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Prelude					as P
import CArray					as CA
import Array					(Array, DIM2)



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

solveLaplace_stencil' steps arrBoundMask arrBoundValue arr
	| steps == 0	
	= arr

	| otherwise
	= solveLaplace_stencil' (steps - 1) arrBoundMask arrBoundValue
	$ forceCArray
--	$ applyBoundary arrBoundMask arrBoundValue
	$ relaxLaplace_stencil arr	


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
relaxLaplace_stencil arr@(CArray shape@(_ :*: n :*: m) _)
 = CArray shape
 $ Left 
	((\d@(sh :*: i :*: j)
	  -> if isBorder d
		then arr !: d
		else (arr !: (sh :*: (i-1) :*: j)
		   +  arr !: (sh :*: i     :*: (j-1))
		   +  arr !: (sh :*: (i+1) :*: j)
		   +  arr !: (sh :*: i     :*: (j+1))) / 4))
		
 where
	isBorder :: DIM2 -> Bool
	isBorder  (_ :*: i :*: j) 
		=  (i == 0) || (i >= n - 1) 
		|| (j == 0) || (j >= m - 1) 
