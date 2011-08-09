
-- | Functions that work directly on PArrays.

--   * The functions in this module are used by the D.A.P.Lifted.Closure module to
--     define the closures that the vectoriser uses.
--
--   * The functions in this module may also be used directly by user programs.
--
module Data.Array.Parallel.PArray 
        ( -- things re-exported from D.A.P.PArray.PData.Base
          PArray
        , lengthPA
        , unpackPA

          -- things defined in this module.
        , emptyPA
        , constructPA
        , appPA
        , fromListPA
        , fromUArrayPA
        , fromUArray2PA
        , toUArrayPA
        , nfPA
        , replicatePA
        , indexPA
        , nestUSegdPA
        
        -- things re-exported from D.A.P.PArray.PData.Tuple
        , zipPA
        , unzipPA)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PRepr
import qualified Data.Array.Parallel.Unlifted     as U


-- PS Functions ---------------------------------------------------------------
-- | An empty array, with 0 elements.
{-# INLINE_PA emptyPA #-}
emptyPA :: PA a => PArray a
emptyPA = PArray 0 emptyPS


-- | Construct an array by applying a generator function to every integer
--   in an unlifted array.
{-# INLINE_PA constructPA #-}
constructPA :: PA a => (Int -> a) -> U.Array Int -> PArray a
constructPA f xs
        = PArray (U.length xs) (constructPS f xs)


-- | Append two arrays
{-# INLINE_PA appPA #-}
appPA :: PA a => PArray a -> PArray a -> PArray a
appPA (PArray n1 d1) (PArray n2 d2)
        = PArray (n1 + n2) (appPS d1 d2)


-- | Convert a list to a PArray.
{-# INLINE_PA fromListPA #-}
fromListPA :: PA a => [a] -> PArray a
fromListPA xs
	= PArray (length xs) (fromListPS xs)


-- | Convert an unlifted array to a PArray.
{-# INLINE_PA fromUArrayPA #-}
fromUArrayPA :: (U.Elt a, PA a) => U.Array a -> PArray a
fromUArrayPA arr
        = PArray (U.length arr) (fromUArrayPS arr)


-- | Convert an unlifted array of pairs to a PArray of pairs.
{-# INLINE_PA fromUArray2PA #-}
fromUArray2PA   :: (U.Elt a, PA a, U.Elt b, PA b) 
                => U.Array (a, b) -> PArray (a, b)
fromUArray2PA arr
 = let  (xs, ys)        = U.unzip arr
   in   PArray  (U.length arr) 
                (PTuple2 (fromUArrayPS xs) (fromUArrayPS ys))


-- | Convert an unlifted array to a PArray.
{-# INLINE_PA toUArrayPA #-}
toUArrayPA :: (U.Elt a, PA a) => PArray a -> U.Array a
toUArrayPA (PArray _ d1)
        = toUArrayPS d1


-- | Force an array to normal form.
{-# INLINE_PA nfPA #-}
nfPA :: PA a => PArray a -> ()
nfPA (PArray n d)
        = n `seq` nfPS d


-- PJ Functions ---------------------------------------------------------------
-- | O(1). Lookup an indexed element from a PArray.
{-# INLINE_PA indexPA #-}
indexPA :: PA a => PArray a -> Int -> a
indexPA (PArray _ d1) ix
	= indexPJ d1 ix


-- Derived Functions ----------------------------------------------------------
-- | O(n). Replicate a single element a certain number of times, 
--   producing physical copies of the element in the PArray.
{-# INLINE_PA replicatePA #-}
replicatePA :: PA a => Int -> a -> PArray a
replicatePA n x 
        = PArray n (replicatePR n x)


-- | O(1). Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
{-# INLINE_PA nestUSegdPA #-}
nestUSegdPA :: U.Segd -> PArray a -> PArray (PArray a)
nestUSegdPA segd (PArray n d1)
        | U.elementsSegd segd     == n
        = PArray n (PNestedS segd d1)

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestUSegdPA: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ show (U.elementsSegd segd)
                , " length of data array        = " ++ show n ]
                

