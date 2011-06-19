{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module IndicesVectorised        
        (indicesPA, indices)
where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int
import qualified Prelude as P


indicesPA :: PArray Int -> PArray Int -> PArray Int
{-# NOINLINE indicesPA #-}
indicesPA arr ixs
        = toPArrayP (indices (fromPArrayP arr) (fromPArrayP ixs))


indices :: [:Int:] -> [:Int:] -> [:Int:]
indices arr ixs
 = treeLookup arr ixs

{-
 = mapP (thing arr) ixs
 
thing :: [:Int:] -> Int -> Int
thing arr i
 = (sliceP i 1 arr) !: 0 


         
-- arr !: i 

thingo :: [:Int:] -> [:Int:] -> [:Int:]
thingo table is
 = go is
 where  go is'
          = 
-}

treeLookup :: [:Int:] -> [:Int:] -> [:Int:]
{-# NOINLINE treeLookup #-}
treeLookup table xx
 | lengthP xx == 1
 = [: table !: (xx !: 0) :]
        
 | otherwise
 = let   len     = lengthP xx
         half    = len `div` 2
         s1      = sliceP 0    half xx
         s2      = sliceP half half  xx           
   in    concatP (mapP (treeLookup table) [: s1, s2 :])
