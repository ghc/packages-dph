
module Data.Array.Parallel.PArray 
        ( module Data.Array.Parallel.PArray.PData
        , module Data.Array.Parallel.PArray.PDataBase
        , module Data.Array.Parallel.PArray.PDataScalar
        , module Data.Array.Parallel.PArray.PDataTuple
        , module Data.Array.Parallel.PArray.PDataNested
        , module Data.Array.Parallel.PArray.PDataClosure
        , module Data.Array.Parallel.PArray.PRepr
        
        , fromListPA
        , fromUArrayPA
        , lengthPA
        , replicatePA
        , indexPA)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PDataBase
import Data.Array.Parallel.PArray.PDataScalar
import Data.Array.Parallel.PArray.PDataTuple
import Data.Array.Parallel.PArray.PDataNested
import Data.Array.Parallel.PArray.PDataClosure
import Data.Array.Parallel.PArray.PRepr

import qualified Data.Array.Parallel.Unlifted     as U


-- | Convert a list to a PArray.
fromListPA :: PA a => [a] -> PArray a
fromListPA xs
	= PArray (length xs) (fromListPS xs)


-- | Convert an unlifted array to a PArray.
fromUArrayPA :: (U.Elt a, PA a) => U.Array a -> PArray a
fromUArrayPA arr
        = PArray (U.length arr) (fromUArrayPS arr)


-- | Take the length of a PArray.
lengthPA :: PArray a -> Int
lengthPA (PArray n _)
        = n


-- | Replicate a single element a certain number of times, 
--   producing physical copies of the element in the PArray.
replicatePA   :: PA a => Int -> a -> PArray a
replicatePA n x 
        = PArray n (replicatePR n x)

-- | Lookup an indexed element from a PArray.
indexPA :: PA a => PArray a -> Int -> a
indexPA (PArray _ d1) ix
	= indexPJ d1 ix

