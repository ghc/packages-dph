{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module MergeSort 
	( sortCorePA
	, sortCore
	, mergeCore
	, flipPairs
	, interleave
	, evens
	, odds)
where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as Int
import qualified Prelude as P

sortCorePA :: PArray Double -> PArray Double
{-# NOINLINE sortCorePA #-}
sortCorePA ps = toPArrayP (sortCore (fromPArrayP ps))


-- | Batcher odd/even merge sort.
--   The length of the list must be a power of two, else loop.
sortCore :: [:Double:] -> [:Double:]
sortCore xx
	| len Int.== 0	= [::]
	| len Int.== 1	= xx
	| otherwise
	= let	half	= len `Int.div` 2
		s1	= sliceP 0    half xx
		s2	= sliceP half len  xx
	  in	mergeCore (sortCore s1 +:+ sortCore s2)

	where len	= lengthP xx


-- | Batcher odd/even merge.
--   The two lists to merge are appended on the input.
--   The length of the lists must be a power of two, else loop.
mergeCore :: [:Double:] -> [:Double:]
mergeCore xx
	| lengthP xx Int.== 2
	= if xx !: 1 < xx !: 0	
		then [: xx !: 1, xx !: 0 :]
		else [: xx !: 0, xx !: 1 :]
	
	| otherwise
	= let	evens'	= mergeCore (evens xx)
		odds'	= mergeCore (odds  xx)
		xx'	= interleave evens' odds'
		ixLast	= lengthP xx' Int.- 1

   	  in	[: xx' !: 0 :]  
	    +:+ (flipPairs (sliceP 1 ixLast xx'))
	    +:+ [: xx' !: ixLast :]


-- | For each consecutive pair of elements, 
--	if they are out of order then flip them so they are.
flipPairs  :: [:Double:] -> [:Double:]
flipPairs xx
 = concatP 
	[: if y < x then [: y, x :] else [: x, y :]
	|  (x, y) 	<- zipP (evens xx) (odds xx) :]


-- | Interleave the elements of two arrays.
interleave :: [:Double:] -> [:Double:] -> [:Double:]
interleave xx yy
 = concatP [: [:x, y:] | (x, y) <- zipP xx yy :]


-- | Take the even indexed elements from an array.
evens :: [:Double:] -> [:Double:]
evens xx
  = [: x | (ix, x)	<- indexedP xx
	 , Int.mod ix 2 Int.== 0 :]


-- | Take the odd indexed elements from an array.
odds  :: [:Double:] -> [:Double:]
odds xx 
	| len Int.== 0	= [::]
	| len Int.== 1	= [::]
	| otherwise	= evens (sliceP 1 len xx)
	where	len	= lengthP xx

