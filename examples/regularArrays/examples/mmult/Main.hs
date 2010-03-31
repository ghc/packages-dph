

import qualified Data.Array.Parallel.Unlifted 	as U
import Array					as A
import CArray					as CA

	
main 
 = do	
	let result	= mmult
	print	$ U.toList $ A.fromArray $ CA.fromCArray result



mmMult	:: CArray DIM2  Double 
	-> CArray DIM2  Double 
	-> CArray DIM2  Double  

mmMult	arr1@(CArray (sh :. m1 :. n1) fn1) 
	arr2@(CArray (sh' :. m2 :. n2) fn2) 

	| not $ (m1 == n2) && (sh == sh')
	= error "mmMult: invalid matrix sizes"
	
	| otherwise
	= fold (+) 0 (arr1Ext * arr2Ext)
	where
		arr2T   = forceCArray $ transpose arr2
		arr1Ext = replicateSlice arr1  (() :. A.All :. m2 :. A.All)
		arr2Ext = replicateSlice arr2T (() :. n1 :. A.All :. A.All)


mmult 
 = let	mat1_list	= [1, 2, 3, 4, 5, 6, 7, 8, 9]
	mat2_list	= [2, 0, 0, 0, 2, 0, 0, 0, 2]
	
	dim :: DIM2
	dim 	= () :. 3 :. 3
	
	mat1 :: CArray DIM2 Double
	mat1	= CA.toCArray $ A.toArray dim $ U.fromList mat1_list

	
	mat2 :: CArray DIM2 Double
	mat2	= CA.toCArray $ A.toArray dim $ U.fromList mat2_list

	matResult = mmMult mat1 mat2

   in	matResult