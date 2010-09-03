{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Reverse	
	(treeReverse)
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
treeReverse xx
	| lengthP xx Int.== 1
	= xx
	
	| otherwise
	= let	len	= lengthP xx
		half	= len `Int.div` 2

		-- adding the call to evens here kills it
		s1	= [: x 	| (x, ix) 
				<- zipP xx {- (sliceP 0 half xx) -} (Int.enumFromToP 0 half)
				,  Int.mod ix 2 Int.== 0 :]

--		s1	= sliceP 0    half xx

		s2	= sliceP half len  xx
	  in	treeReverse s2 +:+ treeReverse s1
