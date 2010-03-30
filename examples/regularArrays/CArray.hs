
module CArray
	( CArray(..)
	, (!:)
	, toCArray
	, fromCArray
	, forceCArray 
	, traverseCArray
	, zipWith)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Data.Array.Parallel.Unlifted.Gabi	(mapU, foldU, enumFromToU)

import qualified Array 				as A
import Prelude 					hiding (map, zip, zipWith, replicate, sum)
import Data.Maybe
import Data.Either


-- CArray -----------------------------------------------------------------------------------------
data CArray dim e 
	= CArray
	{ carrayShape	:: dim
	, carrayCache	:: Either (dim -> e) (U.Array e) }
	

-- Primitive functions ----------------------------------------------------------------------------
-- | Lookup the value in an array.
(!:) 	:: (A.Shape dim, U.Elt e)
	=> CArray dim e -> dim -> e

{-# INLINE (!:) #-}
(!:) arr ix
 = case carrayCache arr of
	Right uarr	-> uarr U.!: (A.toIndex (carrayShape arr) ix)
	Left  fn	-> fn ix
	
	
-- Conversions ------------------------------------------------------------------------------------
-- | Convert a strict array into a cached array.
toCArray :: (U.Elt e, A.Shape dim) => A.Array dim e -> CArray dim e
{-# INLINE toCArray #-}
toCArray (A.Array sh dat)
  = sh `A.deepSeq` dat `seq`
    CArray { carrayShape = sh
	   , carrayCache = Right dat }
		

-- | Convert a cache array into a strict array
fromCArray :: (U.Elt e, A.Shape dim) => CArray dim e -> A.Array dim e
{-# INLINE fromCArray #-}
fromCArray (CArray shape cache)
 = shape `A.deepSeq`
   A.Array
	{ A.arrayData
		= case cache of
			Left fn
			 -> U.map (fn . A.fromIndex shape)
			  $ U.enumFromTo 
				(0 :: Int)
				((A.size shape) - 1)
				
			Right uarr -> uarr
			
	, A.arrayShape = shape }


-- Forcing ----------------------------------------------------------------------------------------
forceCArray 
	:: (U.Elt e, A.Shape dim) 
	=> CArray dim e
	-> CArray dim e

{-# INLINE forceCArray #-}
forceCArray arr = toCArray (fromCArray arr)


-- Traversing -------------------------------------------------------------------------------------

-- | Transform and traverse all the elements of an array.
traverseCArray
	:: (U.Elt e, A.Shape dim)
	=> CArray dim e				-- ^ Source array.
	-> (dim  -> dim')			-- ^ Fn to transform the shape of the array.
	-> (dim' -> dim' -> f)			-- ^ Fn to produce elements of the result array.
						--	It is given the new array shape, and the
						--	index of each element.
	-> CArray dim' f
	
{-# INLINE traverseCArray #-}
traverseCArray arr dFn trafoFn
 = case arr of
	CArray d (Left elemFn)
	 -> CArray 
		(dFn d) 
		(Left $ trafoFn (dFn d))
	
	CArray d (Right uarr)
	 -> CArray 
		(dFn d) 
		(Left $ trafoFn (dFn d))



-- Computations -----------------------------------------------------------------------------------
-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zipWith :: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) 
	=> (a -> b -> c) 
	-> CArray dim a
	-> CArray dim b
	-> CArray dim c

{-# INLINE zipWith #-}
zipWith f arr1 arr2
	= CArray (A.intersectDim 
			(carrayShape arr1)
			(carrayShape arr2))
		 (Left (\i -> f (arr1 !: i) (arr2 !: i)))


