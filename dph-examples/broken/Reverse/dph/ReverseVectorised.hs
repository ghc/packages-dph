{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module ReverseVectorised	
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
		s1	= sliceP 0    half xx
		s2	= sliceP half half xx		
	  in	concatP (mapP treeReverse [:s1, s2:])
