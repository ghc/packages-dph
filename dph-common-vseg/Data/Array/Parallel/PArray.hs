{-# LANGUAGE CPP #-}
#include "fusion-phases-vseg.h"

-- | Functions that work directly on PArrays.

--   * The functions in this module are used by the D.A.P.Lifted.Closure module to
--     define the closures that the vectoriser uses.
--
--   * The functions in this module may also be used directly by user programs.
--
module Data.Array.Parallel.PArray 
        ( 
        -- Things re-exported from D.A.P.PArray.PData.Base
          PArray
        , lengthPA
        , unpackPA

        -- PA versions of PR functions.
        , validPA
        , emptyPA
        , nfPA
        , replicatePA
        , replicatesPA
        , unsafeReplicatesPA
        , fromVectorPA
        , toVectorPA
        , indexPA
	, indexlPA
        , extractPA
        , extractsPA
        , appendPA
        , packByTagPA
        , combine2PA
	, fromUArrayPA
	, toUArrayPA
        
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

        -- Wrappers used for testing only.
        , replicatesPA'
        , unsafeReplicatesPA'
        , extractsPA'
        , packByTagPA'
        , combine2PA'
        , slicelPA')
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V

instance (Eq a, PR a)  => Eq (PArray a) where
 (==) xs ys = toVectorPA xs == toVectorPA ys
 (/=) xs ys = toVectorPA xs /= toVectorPA ys


-- | Check that an array has a valid internal representation.
{-# INLINE_PA validPA #-}
validPA :: PA a => PArray a -> Bool
validPA (PArray n darr1)
        =  validPR darr1
        && validBool "parray length" (n == lengthPR darr1)


-- | An empty array.
{-# INLINE_PA emptyPA #-}
emptyPA :: PA a => PArray a
emptyPA
        = PArray 0 emptyPR


-- | Force an array to normal form.
{-# INLINE_PA nfPA #-}
nfPA :: PA a => PArray a -> ()
nfPA (PArray n d)
        = n `seq` nfPR d


-- | Define an array of the given size, that maps all elements to the same value.
--   We require the replication count to be > 0 so that it's easier to maintain
--   the validPR invariants for nested arrays.
--   O(n). 
{-# INLINE_PA replicatePA #-}
replicatePA :: PA a => Int -> a -> PArray a
replicatePA n x
        = PArray n (replicatePR n x)


-- | Segmented replicate.
--   O(sum lengths). 
{-# INLINE_PA replicatesPA #-}
replicatesPA :: PA a => U.Array Int -> PArray a -> PArray a
replicatesPA repCounts (PArray _ darr)
        = PArray (U.sum repCounts) (replicatesPR repCounts darr)


-- | Unsafe segmented replicate.
--   O(sum lengths). 
{-# INLINE_PA unsafeReplicatesPA #-}
unsafeReplicatesPA :: PA a => U.Array Int -> PArray a -> PArray a
unsafeReplicatesPA repCounts (PArray _ darr)
        = PArray (U.sum repCounts) (unsafeReplicatesPR repCounts darr)


-- | Convert a `Vector` to a `PArray`
{-# INLINE_PA fromVectorPA #-}
fromVectorPA :: PA a => Vector a -> PArray a
fromVectorPA vec
        = PArray (V.length vec) (fromVectorPR vec)


-- | Convert a `PArray` to a `Vector`        
{-# INLINE_PA toVectorPA #-}
toVectorPA   :: PA a => PArray a -> Vector a
toVectorPA (PArray _ arr)
        = toVectorPR arr


-- | Lookup a single element from the source array.
{-# INLINE_PA indexPA #-}
indexPA    :: PA a => PArray a -> Int -> a
indexPA (PArray _ arr) ix
        = indexPR arr ix


-- | Lookup a several elements from several source arrays
{-# INLINE_PA indexlPA #-}
indexlPA    :: PA a => PArray (PArray a) -> PArray Int -> PArray a
indexlPA (PArray n darr) (PArray _ ixs)
        = PArray n (indexlPR n darr ixs)


-- | Extract a range of elements from an array.
{-# INLINE_PA extractPA #-}
extractPA  :: PA a => PArray a -> Int -> Int -> PArray a
extractPA (PArray _ arr) start len
        = PArray len (extractPR arr start len)


-- | Segmented extract.
{-# INLINE_PA extractsPA #-}
extractsPA 
        :: PA a 
        => Vector (PArray a)    -- ^ source array
        -> U.Array Int          -- ^ segment source ids
        -> U.Array Int          -- ^ segment base indices
        -> U.Array Int          -- ^ segment lengths
        -> PArray a

extractsPA arrs srcids segIxs segLens
 = let vecs     = V.map (\(PArray _ vec) -> vec) arrs
   in  PArray   (U.sum segLens)
                (extractsPR vecs srcids segIxs segLens)


-- | Append two arrays.
{-# INLINE_PA appendPA #-}
appendPA :: PA a => PArray a -> PArray a -> PArray a
appendPA (PArray n1 darr1) (PArray n2 darr2)
        = PArray (n1 + n2) (darr1 `appendPR` darr2)


-- | Filter an array based on some tags.
{-# INLINE_PA packByTagPA #-}
packByTagPA :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTagPA (PArray _ darr) tags tag
 = let  darr'   = packByTagPR darr tags tag
   in   PArray (lengthPR darr') darr'


-- | Combine two arrays based on a selector.
{-# INLINE_PA combine2PA #-}
combine2PA  :: PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2PA sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'    = combine2PR sel darr1 darr2
   in   PArray (lengthPR darr') darr'


-- | Convert a `UArray` to a `PArray`
{-# INLINE_PA toUArrayPA #-}
toUArrayPA :: (U.Elt a, PA a) => PArray a -> U.Array a
toUArrayPA (PArray n darr)
	= toUArrayPR darr


-- | Convert a `PArray` to a `UArray`
{-# INLINE_PA fromUArrayPA #-}
fromUArrayPA :: (U.Elt a, PA a) => U.Array a -> PArray a
fromUArrayPA uarr
	= PArray (U.length uarr) (fromUArrayPR uarr)


-- Derived combinators --------------------------------------------------------
-- | Produce an array containing a single element.
{-# INLINE_PA singletonPA #-}
singletonPA :: PA a => a -> PArray a
singletonPA x
        = PArray 1 (replicatePR 1 x)


-- | Convert a list to a `PArray`.
{-# INLINE_PA fromListPA #-}
fromListPA :: PA a => [a] -> PArray a
fromListPA xx
        = PArray (length xx) (fromVectorPR $ V.fromList xx)


-- | Convert a `PArray` to a list.
{-# INLINE_PA toListPA #-}
toListPA   :: PA a => PArray a -> [a]
toListPA (PArray _ arr)
        = V.toList $ toVectorPR arr


-- | Convert an unlifted array of pairs to a PArray of pairs.
{-# INLINE_PA fromUArray2PA #-}
fromUArray2PA   :: (U.Elt a, PA a, U.Elt b, PA b) 
                => U.Array (a, b) -> PArray (a, b)
fromUArray2PA arr
 = let  (xs, ys)        = U.unzip arr
   in   PArray  (U.length arr) 
                (PTuple2 (fromUArrayPR xs) (fromUArrayPR ys))


-- | O(1). Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
{-# INLINE_PA nestUSegdPA #-}
nestUSegdPA :: U.Segd -> PArray a -> PArray (PArray a)
nestUSegdPA segd (PArray n darr)
        | U.elementsSegd segd     == n
        = PArray (U.lengthSegd segd)
	$ PNested	
		{ pnested_vsegids	= U.enumFromTo 0 (U.lengthSegd segd - 1)
		, pnested_pseglens	= U.lengthsSegd segd
		, pnested_psegstarts	= U.indicesSegd segd
		, pnested_psegsrcs	= U.replicate (U.lengthSegd segd) 0
		, pnested_psegdata	= V.singleton darr }

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestUSegdPA: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ show (U.elementsSegd segd)
                , " length of data array        = " ++ show n ]


-- | Concatenate a nested array.
{-# INLINE_PA concatPA #-}
concatPA :: PA a => PArray (PArray a) -> PArray a
concatPA (PArray n darr)
 = let  darr'   = concatPR darr
   in   PArray (lengthPR darr') darr'


-- | Impose a nesting structure on a flat array
{-# INLINE_PA unconcatPA #-}
unconcatPA :: PA a => PArray (PArray a) -> PArray a -> PArray (PArray a)
unconcatPA (PArray n darr1) (PArray _ darr2)
        = PArray n (unconcatPR darr1 darr2)


-- | Lifted concat
{-# INLINE_PA concatlPA #-}
concatlPA :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatlPA (PArray n darr1)
        = PArray n (concatlPR darr1)


-- | Lifted append
--   Both arrays must have the same length
{-# INLINE_PA appendlPA #-}
appendlPA :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendlPA (PArray n darr1) (PArray _ darr2)
        = PArray n (appendlPR darr1 darr2)


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
{-# INLINE_PA slicelPA #-}
slicelPA :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicelPA (PArray c sliceStarts) (PArray _ sliceLens) (PArray _ darr)
        = PArray c (slicelPR sliceStarts sliceLens darr)


-------------------------------------------------------------------------------
-- These PArray functions are just for testing 
-------------------------------------------------------------------------------

-- | Segmented replicate.
replicatesPA' :: PA a => [Int] -> PArray a -> PArray a
replicatesPA' lens arr
        = replicatesPA (U.fromList lens) arr

-- | Unsafe segmented replicate.
unsafeReplicatesPA' :: PA a => [Int] -> PArray a -> PArray a
unsafeReplicatesPA' lens arr
        = unsafeReplicatesPA (U.fromList lens) arr


-- | Segmented extract.
extractsPA' :: PA a => V.Vector (PArray a) -> [Int] ->  [Int] -> [Int] -> PArray a
extractsPA' arrs srcids startixs seglens
        = PArray (sum seglens) 
        $ extractsPR (V.map unpackPA arrs) 
                     (U.fromList srcids) (U.fromList startixs) (U.fromList seglens)


-- | Filter an array based on some tags.
packByTagPA' :: PA a => PArray a -> [Int] -> Int -> PArray a
packByTagPA' (PArray n arr) tags tag
 = let  arr'    = packByTagPR arr (U.fromList tags) tag
   in   PArray (lengthPR arr') arr'


combine2PA' :: PA a => [Int] -> PArray a -> PArray a -> PArray a
combine2PA' sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'   = combine2PR (U.tagsToSel2 (U.fromList sel)) darr1 darr2
   in   PArray 0 darr'


-- | Extract some slices from some arrays.
slicelPA' :: PA a => [Int] -> [Int] -> PArray (PArray a) -> PArray (PArray a)
slicelPA' sliceStarts sliceLens arr
        = slicelPA (fromListPA sliceStarts) (fromListPA sliceLens) arr
