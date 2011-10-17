{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | Functions that work directly on PArrays.

--   * The functions in this module are used by the D.A.P.Lifted.Closure module to
--     define the closures that the vectoriser uses.
--
--   * The functions in this module may also be used directly by user programs.
--
module Data.Array.Parallel.PArray 
        ( 
        -- Things re-exported from D.A.P.PArray.PData.Base
          PArray(..)
        , lengthPA
        , unpackPA

        -- PA versions of PR functions.
        , validPA
        , emptyPA
        , nfPA
        , replicatePA
        , replicatesPA
        , indexPA
	, indexlPA
        , extractPA
        , extractsPA
        , appendPA
        , packByTagPA
        , combine2PA
        , fromVectorPA
        , toVectorPA

        -- Derived operators.
        , singletonPA
        , fromListPA
        , toListPA
	, fromUArray2PA
	, nestUSegdPA
        , concatPA
        , unconcatPA
        , concatlPA
        , appendlPA
        , slicelPA

        -- Scalar conversion functions, from the D.A.P.PArray.Scalar
        , fromUArrayPA
        , toUArrayPA

        -- Tuple conversion functions
        , fromUArrayPA_2

        -- Wrappers used for testing only.
        , replicatesPA'
        , extractsPA'
        , packByTagPA'
        , combine2PA'
        , slicelPA')
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import GHC.Exts


instance (Eq a, PA a)  => Eq (PArray a) where
 (==) (PArray _ xs) (PArray _ ys) = toVectorPD xs == toVectorPD ys
 (/=) (PArray _ xs) (PArray _ ys) = toVectorPD xs /= toVectorPD ys


-- | Check that an array has a valid internal representation.
{-# INLINE_PA validPA #-}
validPA :: PA a => PArray a -> Bool
validPA (PArray n# darr1)
        =  validPD darr1
        && validBool "parray length" (I# n# == lengthPD darr1)


-- | An empty array.
{-# INLINE_PA emptyPA #-}
emptyPA :: PA a => PArray a
emptyPA
        = PArray 0# emptyPD


-- | Force an array to normal form.
{-# INLINE_PA nfPA #-}
nfPA :: PA a => PArray a -> ()
nfPA (PArray n# d)
        = nfPD d


-- | Define an array of the given size, that maps all elements to the same value.
--   We require the replication count to be > 0 so that it's easier to maintain
--   the validPR invariants for nested arrays.
--   O(n). 
{-# INLINE_PA replicatePA #-}
replicatePA :: PA a => Int -> a -> PArray a
replicatePA (I# n#) x
        = PArray n# (replicatePD (I# n#) x)


-- | Segmented replicate.
--   O(sum lengths). 
{-# INLINE_PA replicatesPA #-}
replicatesPA :: PA a => U.Array Int -> PArray a -> PArray a
replicatesPA repCounts (PArray _ darr)
 = let  I# n#   = U.sum repCounts
   in   PArray  n#
                (replicatesPD (U.lengthsToSegd repCounts) darr)


-- | Convert a `Vector` to a `PArray`
{-# INLINE_PA fromVectorPA #-}
fromVectorPA :: PA a => Vector a -> PArray a
fromVectorPA vec
 = let I# n#      = V.length vec
   in  PArray n#  (fromVectorPD vec)


-- | Convert a `PArray` to a `Vector`        
{-# INLINE_PA toVectorPA #-}
toVectorPA   :: PA a => PArray a -> Vector a
toVectorPA (PArray _ arr)
        = toVectorPD arr


-- | Lookup a single element from the source array.
{-# INLINE_PA indexPA #-}
indexPA    :: PA a => PArray a -> Int -> a
indexPA (PArray _ arr) ix
        = indexPD arr ix


-- | Lookup a several elements from several source arrays
{-# INLINE_PA indexlPA #-}
indexlPA    :: PA a => PArray (PArray a) -> PArray Int -> PArray a
indexlPA (PArray n# darr) (PArray _ ixs)
        = PArray n# (indexlPD (I# n#) darr ixs)


-- | Extract a range of elements from an array.
{-# INLINE_PA extractPA #-}
extractPA  :: PA a => PArray a -> Int -> Int -> PArray a
extractPA (PArray _ arr) start len@(I# len#)
        = PArray len# (extractPD arr start len)


-- | Segmented extract.
{-# INLINE_PA extractsPA #-}
extractsPA 
        :: PA a 
        => Vector (PArray a)    -- ^ source array
        -> U.SSegd
        -> PArray a

extractsPA arrs ssegd
 = let  vecs            = V.map (\(PArray _ vec) -> vec) arrs
        !(I# n#)        = (U.sum $ U.lengthsSSegd ssegd)
   in  PArray   n#
                (extractsPD vecs ssegd)


-- | Append two arrays.
{-# INLINE_PA appendPA #-}
appendPA :: PA a => PArray a -> PArray a -> PArray a
appendPA (PArray n1# darr1) (PArray n2# darr2)
        = PArray (n1# +# n2#) (appendPD darr1 darr2)


-- | Filter an array based on some tags.
{-# INLINE_PA packByTagPA #-}
packByTagPA :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTagPA (PArray _ darr) tags tag
 = let  darr'           = packByTagPD darr tags tag
        !(I# n#)        = lengthPD darr'
   in   PArray n# darr'


-- | Combine two arrays based on a selector.
{-# INLINE_PA combine2PA #-}
combine2PA  :: PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2PA sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'           = combine2PD sel darr1 darr2
        !(I# n#)        = lengthPD darr'
   in   PArray n# darr'


-- Derived combinators --------------------------------------------------------
-- | Produce an array containing a single element.
{-# INLINE_PA singletonPA #-}
singletonPA :: PA a => a -> PArray a
singletonPA x
        = PArray 1# (replicatePD 1 x)


-- | Convert a list to a `PArray`.
{-# INLINE_PA fromListPA #-}
fromListPA :: PA a => [a] -> PArray a
fromListPA xx
 = let  I# n#   = length xx
   in   PArray n# (fromVectorPD $ V.fromList xx)


-- | Convert a `PArray` to a list.
{-# INLINE_PA toListPA #-}
toListPA   :: PA a => PArray a -> [a]
toListPA (PArray _ arr)
        = V.toList $ toVectorPD arr


-- | Convert an unlifted array of pairs to a PArray of pairs.
{-# INLINE_PA fromUArray2PA #-}
fromUArray2PA   :: (Scalar a, PA a, Scalar b, PA b) 
                => U.Array (a, b) -> PArray (a, b)
fromUArray2PA arr
 = let  (xs, ys)        = U.unzip arr
        PArray _ pdata1 = fromUArrayPA xs
        PArray _ pdata2 = fromUArrayPA ys
        I# n#           = U.length arr
   in   PArray  n#
                (PTuple2 pdata1 pdata2)


-- | O(1). Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
{-# NOINLINE nestUSegdPA #-}
nestUSegdPA :: U.Segd -> PArray a -> PArray (PArray a)
nestUSegdPA segd (PArray n# pdata)
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


-- | Concatenate a nested array.
{-# INLINE_PA concatPA #-}
concatPA :: PA a => PArray (PArray a) -> PArray a
concatPA (PArray _ darr)
 = let  darr'   = concatPD darr
        I# n#   = lengthPD darr'
   in   PArray  n# darr'


-- | Impose a nesting structure on a flat array
{-# INLINE_PA unconcatPA #-}
unconcatPA :: PA a => PArray (PArray a) -> PArray a -> PArray (PArray a)
unconcatPA (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ unconcatPD pdata1 pdata2


-- | Lifted concat
{-# INLINE_PA concatlPA #-}
concatlPA :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatlPA (PArray n# pdata1)
        = PArray n# $ concatlPD pdata1


-- | Lifted append
--   Both arrays must have the same length
{-# INLINE_PA appendlPA #-}
appendlPA :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendlPA (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ appendlPD pdata1 pdata2


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
{-# INLINE_PA slicelPA #-}
slicelPA :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicelPA (PArray n# sliceStarts) (PArray _ sliceLens) (PArray _ darr)
        = PArray n# (slicelPR sliceStarts sliceLens darr)


-------------------------------------------------------------------------------
-- These PArray functions are just for testing 
-------------------------------------------------------------------------------

-- | Segmented replicate.
replicatesPA' :: PA a => [Int] -> PArray a -> PArray a
replicatesPA' lens arr
        = replicatesPA (U.fromList lens) arr


-- | Segmented extract.
extractsPA' :: PA a => V.Vector (PArray a) -> [Int] ->  [Int] -> [Int] -> PArray a
extractsPA' arrs srcids startixs seglens
 = let  !(I# n#) = sum seglens
   in   PArray n#
        $ extractsPD
                (V.map unpackPA arrs) 
                (U.mkSSegd (U.fromList srcids) (U.fromList startixs)
                        $ U.lengthsToSegd (U.fromList seglens))

-- | Filter an array based on some tags.
packByTagPA' :: PA a => PArray a -> [Int] -> Int -> PArray a
packByTagPA' (PArray _ arr) tags tag
 = let  arr'    = packByTagPD arr (U.fromList tags) tag
        I# n#   = lengthPD arr'
   in   PArray n#  arr'


combine2PA' :: PA a => [Int] -> PArray a -> PArray a -> PArray a
combine2PA' sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'   = combine2PD (U.tagsToSel2 (U.fromList sel)) darr1 darr2
   in   PArray 0# darr'


-- | Extract some slices from some arrays.
slicelPA' :: PA a => [Int] -> [Int] -> PArray (PArray a) -> PArray (PArray a)
slicelPA' sliceStarts sliceLens arr
        = slicelPA (fromListPA sliceStarts) (fromListPA sliceLens) arr


