{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | Functions that work directly on PArrays.

--   * The functions in this module are used by the D.A.P.Lifted.Closure module to
--     define the closures that the vectoriser uses.
--
--   * The functions in this module may also be used directly by user programs.
--
module Data.Array.Parallel.PArray 
        ( PArray(..)
        , valid
        , nf

        -- * Constructors
        , empty
        , singleton
        , replicate,    replicates
        , append,       appendl
        , concat,       concatl
        , nestUSegd,    unconcat

        -- * Projections
        , length
        , unpack
        , index,        indexl
        , extract,      extracts
        , slice,        slicel

        -- * Pack and Combine
        , packByTag
        , combine2

        -- * Enumerations
        , enumFromTo,   enumFromTol

        -- * Tuples
        , zip,          unzip           -- from D.A.P.PArray.Tuple

        -- * Conversions
        , fromVector,   toVector
        , fromList,     toList
        , fromUArray,   toUArray        -- from D.A.P.PArray.Scalar
	, fromUArray2)                  -- from D.A.P.PArray.Scalar
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import GHC.Exts
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Prelude                        as P
import Prelude hiding 
        ( length, replicate, concat
        , enumFromTo
        , zip, unzip)


instance (Eq a, PA a)  => Eq (PArray a) where
 (==) (PArray _ xs) (PArray _ ys) = toVectorPD xs == toVectorPD ys
 (/=) (PArray _ xs) (PArray _ ys) = toVectorPD xs /= toVectorPD ys


-- Basics ---------------------------------------------------------------------
-- | Check that an array has a valid internal representation.
{-# INLINE_PA valid #-}
valid :: PA a => PArray a -> Bool
valid (PArray n# darr1)
        =  validPD darr1
        && validBool "parray length" (I# n# == lengthPD darr1)


-- | Force an array to normal form.
{-# INLINE_PA nf #-}
nf :: PA a => PArray a -> ()
nf (PArray n# d)
        = nfPD d


-- Constructors ----------------------------------------------------------------
-- | O(1). An empty array.
{-# INLINE_PA empty #-}
empty :: PA a => PArray a
empty   = PArray 0# emptyPD


-- | O(1). Produce an array containing a single element.
{-# INLINE_PA singleton #-}
singleton :: PA a => a -> PArray a
singleton x
        = PArray 1# (replicatePD 1 x)


-- | O(n). Define an array of the given size, that maps all elements to the same value.
--   We require the replication count to be > 0 so that it's easier to maintain
--   the validPR invariants for nested arrays.
{-# INLINE_PA replicate #-}
replicate :: PA a => Int -> a -> PArray a
replicate (I# n#) x
        = PArray n# (replicatePD (I# n#) x)


-- | O(sum lengths). Segmented replicate.
{-# INLINE_PA replicates #-}
replicates :: PA a => U.Array Int -> PArray a -> PArray a
replicates repCounts (PArray _ darr)
 = let  I# n#   = U.sum repCounts
   in   PArray  n#
                (replicatesPD (U.lengthsToSegd repCounts) darr)


-- | Append two arrays.
{-# INLINE_PA append #-}
append :: PA a => PArray a -> PArray a -> PArray a
append (PArray n1# darr1) (PArray n2# darr2)
        = PArray (n1# +# n2#) (appendPD darr1 darr2)


-- | Lifted append.
--   Both arrays must have the same length
{-# INLINE_PA appendl #-}
appendl :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ appendlPD pdata1 pdata2


-- | Concatenate a nested array.
{-# INLINE_PA concat #-}
concat :: PA a => PArray (PArray a) -> PArray a
concat (PArray _ darr)
 = let  darr'   = concatPD darr
        I# n#   = lengthPD darr'
   in   PArray  n# darr'


-- | Lifted concat.
{-# INLINE_PA concatl #-}
concatl :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatl (PArray n# pdata1)
        = PArray n# $ concatlPD pdata1


-- | Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
{-# NOINLINE nestUSegd #-}
nestUSegd :: U.Segd -> PArray a -> PArray (PArray a)
nestUSegd segd (PArray n# pdata)
        | U.elementsSegd segd     == I# n#
        , I# n2#                <- U.lengthSegd segd
        = PArray n2#
	$ PNested (U.promoteSegdToVSegd segd) (V.singleton pdata)	

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestUSegdPA: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ show (U.elementsSegd segd)
                , " length of data array        = " ++ show (I# n#) ]


-- | Impose a nesting structure on a flat array
{-# INLINE_PA unconcat #-}
unconcat :: PA a => PArray (PArray a) -> PArray a -> PArray (PArray a)
unconcat (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ unconcatPD pdata1 pdata2


-- Projections  ---------------------------------------------------------------
-- | O(1). Lookup a single element from the source array.
{-# INLINE_PA index #-}
index    :: PA a => PArray a -> Int -> a
index (PArray _ arr) ix
        = indexPD arr ix


-- | O(len indices). Lookup a several elements from several source arrays
{-# INLINE_PA indexl #-}
indexl    :: PA a => PArray (PArray a) -> PArray Int -> PArray a
indexl (PArray n# darr) (PArray _ ixs)
        = PArray n# (indexlPD (I# n#) darr ixs)


-- | Extract a range of elements from an array.
{-# INLINE_PA extract #-}
extract  :: PA a => PArray a -> Int -> Int -> PArray a
extract (PArray _ arr) start len@(I# len#)
        = PArray len# (extractPD arr start len)


-- | Segmented extract.
{-# INLINE_PA extracts #-}
extracts :: PA a => Vector (PArray a) -> U.SSegd -> PArray a
extracts arrs ssegd
 = let  vecs            = V.map (\(PArray _ vec) -> vec) arrs
        !(I# n#)        = (U.sum $ U.lengthsSSegd ssegd)
   in  PArray   n#
                (extractsPD vecs ssegd)


-- | Extract a range of elements from an arrary.
--   Like `extract` but with the parameters in a different order.
{-# INLINE_PA slice #-}
slice :: PA a => Int -> Int -> PArray a -> PArray a
slice start len@(I# len#) (PArray _ darr)
        = PArray len# (extractPD darr start len)


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
{-# INLINE_PA slicel #-}
slicel :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicel (PArray n# sliceStarts) (PArray _ sliceLens) (PArray _ darr)
        = PArray n# (slicelPR sliceStarts sliceLens darr)


-- Pack and Combine -----------------------------------------------------------
-- | Filter an array based on some tags.
{-# INLINE_PA packByTag #-}
packByTag :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTag (PArray _ darr) tags tag
 = let  darr'           = packByTagPD darr tags tag
        !(I# n#)        = lengthPD darr'
   in   PArray n# darr'


-- | Combine two arrays based on a selector.
{-# INLINE_PA combine2 #-}
combine2  :: PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'           = combine2PD sel darr1 darr2
        !(I# n#)        = lengthPD darr'
   in   PArray n# darr'


-- Conversions ----------------------------------------------------------------
-- | Convert a `Vector` to a `PArray`
{-# INLINE_PA fromVector #-}
fromVector :: PA a => Vector a -> PArray a
fromVector vec
 = let !(I# n#) = V.length vec
   in  PArray n#  (fromVectorPD vec)


-- | Convert a `PArray` to a `Vector`        
{-# INLINE_PA toVector #-}
toVector   :: PA a => PArray a -> Vector a
toVector (PArray _ arr)
        = toVectorPD arr


-- | Convert a list to a `PArray`.
{-# INLINE_PA fromList #-}
fromList :: PA a => [a] -> PArray a
fromList xx
 = let  !(I# n#) = P.length xx
   in   PArray n# (fromVectorPD $ V.fromList xx)


-- | Convert a `PArray` to a list.
{-# INLINE_PA toList #-}
toList   :: PA a => PArray a -> [a]
toList (PArray _ arr)
        = V.toList $ toVectorPD arr

