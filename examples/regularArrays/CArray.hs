
module CArray
	( CArray(..)
	, (!:)
	, toCArray
	, fromCArray
	, forceCArray 
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

(!:) arr ix
 = case carrayCache arr of
	Right uarr	-> uarr U.!: (A.toIndex (carrayShape arr) ix)
	Left  fn	-> fn ix
	
	
-- Constructors -----------------------------------------------------------------------------------

-- | Convert a strict array into a cached array.
toCArray :: (U.Elt e, A.Shape dim) => A.Array dim e -> CArray dim e
{-# INLINE toCArray #-}
toCArray arr
 	=     A.arrayShape arr 
	`seq` A.arrayData arr 
	`seq` CArray
		{ carrayShape	= A.arrayShape arr
		, carrayCache	= Right (A.arrayData arr) }
		

-- | Convert a cache array into a strict array
fromCArray :: (U.Elt e, A.Shape dim) => CArray dim e -> A.Array dim e
{-# INLINE fromCArray #-}
fromCArray arr	
 = A.Array
	{ A.arrayData
		= case carrayCache arr of
			Left fn
			 -> U.map (fn . A.fromIndex (carrayShape arr))
			  $ U.enumFromTo 
				(0 :: Int)
				((A.size $ carrayShape arr) - 1)
				
			Right uarr -> uarr
			
	, A.arrayShape
		= carrayShape arr }
		

-- Forcing ----------------------------------------------------------------------------------------
forceCArray 
	:: (U.Elt e, A.Shape dim) 
	=> CArray dim e
	-> CArray dim e
{-# INLINE forceCArray #-}
forceCArray arr
 = let	arr'	= fromCArray arr
   in	A.arrayData arr' `seq` (toCArray arr')


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

		

