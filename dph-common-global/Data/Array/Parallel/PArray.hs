
module Data.Array.Parallel.PArray 
        ( module Data.Array.Parallel.PArray.PData
        , module Data.Array.Parallel.PArray.PRepr
        , fromListPA
        , fromUArrayPA
        , replicatePA
        , indexPA)
where
import Data.Array.Parallel.PArray.PData
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


-- | Replicate a single element a certain number of times, 
--   producing physical copies of the element in the PArray.
replicatePA   :: PA a => Int -> a -> PArray a
replicatePA n x 
        = PArray n (replicatePR n x)


-- | Lookup an indexed element from a PArray.
indexPA :: PA a => PArray a -> Int -> a
indexPA (PArray _ d1) ix
	= indexPJ d1 ix

