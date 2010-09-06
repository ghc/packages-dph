{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Reverse	
	(treeReversePA)
where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as Int
import qualified Prelude as P


treeReversePA :: PArray Double -> PArray Double
{-# NOINLINE treeReversePA #-}
treeReversePA ps
	= toPArrayP (treeReverse (fromPArrayP ps))

-- | Reverse the elements in an array using a tree.
treeReverse :: [:Double:] -> [:Double:]
{-# NOINLINE treeReverse #-}
treeReverse xx
	| lengthP xx Int.== 1
	= xx
	
	| otherwise
	= let	len	= lengthP xx
		half	= len `Int.div` 2

		-- adding the call to evens here kills it
{-		s1	= [: x 	| (ix, x) 
				<- zipP (Int.enumFromToP 0 (len Int.- 1)) xx
				,  Int.mod ix 2 Int.== 0 :]
-}
--		s1	= [: x 	| (ix, x) 
--				<- zipP (Int.enumFromToP 0 (half Int.- 1)) (sliceP 0 half xx) :]

		s1	= sliceP 0    half xx
		s2	= sliceP half len  xx
	  in	treeReverse s2 +:+ treeReverse s1
