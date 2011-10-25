{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | Functions that work directly on PArrays.

--   * The functions in this module are used by the D.A.P.Lifted.Combinator module to
--     define the closures that the vectoriser uses.
--
--   * The functions in this module may also be used directly by user programs.
--
--   * In general, these functions are all unsafe and don't do bounds checks.
--     The lifted versions also don't check that each of the argument arrays
--     have the same length.
--
--     TODO:
--      Export unsafe versions from Data.Array.Parallel.PArray.Unsafe,
--      and make this module export safe wrappers.
--      We want to use the unsafe versions in D.A.P.Lifted.Combinators
--      for performance reasons, but the user facing PArray functions 
--      should all be safe.
-- 
module Data.Array.Parallel.PArray 
        ( PArray(..)
        , valid
        , nf

        -- * Constructors
        , empty
        , singleton,    singletonl
        , replicate,    replicatel,     replicates
        , append,       appendl
        , concat,       concatl
        , unconcat
        , nestUSegd

        -- * Projections
        , length,       lengthl          -- length from D.A.P.PArray.PData.Base
        , index,        indexl
        , extract,      extracts
        , slice,        slicel
        , unpack
        , unsafeTakeSegd

        -- * Pack and Combine
        , pack,         packl
        , packByTag
        , combine2

        -- * Enumerations
        , enumFromTo,   enumFromTol     -- from D.A.P.PArray.Scalar

        -- * Tuples
        , zip,          zipl            -- from D.A.P.PArray.Tuple
        , unzip,        unzipl          -- from D.A.P.PArray.Tuple

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
 (==) (PArray _ xs) (PArray _ ys) = toVectorPA xs == toVectorPA ys
 (/=) (PArray _ xs) (PArray _ ys) = toVectorPA xs /= toVectorPA ys


-- Basics ---------------------------------------------------------------------
-- | Check that an array has a valid internal representation.
valid :: PA a => PArray a -> Bool
valid (PArray n# darr1)
        =  validPA darr1
        && validBool "parray length" (I# n# == lengthPA darr1)
{-# INLINE_PA valid #-}


-- | Force an array to normal form.
nf :: PA a => PArray a -> ()
nf (PArray n# d)
        = nfPA d
{-# INLINE_PA nf #-}


-- Constructors ----------------------------------------------------------------
-- | O(1). An empty array.
empty :: PA a => PArray a
empty   = PArray 0# emptyPA
{-# INLINE_PA empty #-}


-- | O(1). Produce an array containing a single element.
singleton :: PA a => a -> PArray a
singleton x
        = PArray 1# (replicatePA 1 x)
{-# INLINE_PA singleton #-}


-- | O(n). Produce an array of singleton arrays.
singletonl :: PA a => PArray a -> PArray (PArray a)
singletonl arr
        = replicatel (replicate (length arr) 1) arr
{-# INLINE_PA singletonl #-}


-- | O(n). Define an array of the given size, that maps all elements to the same value.
--   We require the replication count to be > 0 so that it's easier to maintain
--   the validPR invariants for nested arrays.
replicate :: PA a => Int -> a -> PArray a
replicate (I# n#) x
        = PArray n# (replicatePA (I# n#) x)
{-# INLINE_PA replicate #-}


-- | O(sum lengths). Lifted replicate.
replicatel :: PA a => PArray Int -> PArray a -> PArray (PArray a)
replicatel (PArray 0# _) _       = empty
replicatel (PArray n# (PInt lens)) (PArray _ pdata)
 = let  segd    = U.lengthsToSegd lens
        c       = I# n#
   in   PArray n# 
         $ mkPNested
                (U.replicate_s segd (U.enumFromTo 0 (c - 1)))
                lens
                (U.indicesSegd segd)
                (U.replicate c 0)
                (singletondPA pdata)
{-# INLINE_PA replicatel #-}


-- | O(sum lengths). Segmented replicate.
replicates :: PA a => U.Segd -> PArray a -> PArray a
replicates segd (PArray _ pdata)
 = let  !(I# n#) = U.elementsSegd segd
   in   PArray n# $ replicatesPA segd pdata
{-# INLINE_PA replicates #-}


-- | Append two arrays.
append :: PA a => PArray a -> PArray a -> PArray a
append (PArray n1# darr1) (PArray n2# darr2)
        = PArray (n1# +# n2#) (appendPA darr1 darr2)
{-# INLINE_PA append #-}


-- | Lifted append.
--   Both arrays must have the same length
appendl :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ appendlPA pdata1 pdata2
{-# INLINE_PA appendl #-}


-- | Concatenate a nested array.
concat :: PA a => PArray (PArray a) -> PArray a
concat (PArray _ darr)
 = let  darr'    = concatPA darr
        !(I# n#) = lengthPA darr'
   in   PArray  n# darr'
{-# INLINE_PA concat #-}


-- | Lifted concat.
concatl :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatl (PArray n# pdata1)
        = PArray n# $ concatlPA pdata1
{-# INLINE_PA concatl #-}


-- | Impose a nesting structure on a flat array
unconcat :: (PA a, PA b) => PArray (PArray a) -> PArray b -> PArray (PArray b)
unconcat (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ unconcatPA pdata1 pdata2
{-# INLINE_PA unconcat #-}


-- | Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
nestUSegd :: PA a => U.Segd -> PArray a -> PArray (PArray a)
nestUSegd segd (PArray n# pdata)
        | U.elementsSegd segd     == I# n#
        , I# n2#                <- U.lengthSegd segd
        = PArray n2#
	$ PNested (U.promoteSegdToVSegd segd) (singletondPA pdata)	

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestUSegdPA: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ show (U.elementsSegd segd)
                , " length of data array        = " ++ show (I# n#) ]
{-# NOINLINE nestUSegd #-}


-- Projections  ---------------------------------------------------------------
-- | Take the length of some arrays.
lengthl :: PA a => PArray (PArray a) -> PArray Int
lengthl (PArray n# (PNested vsegd _))
        = PArray n# $ PInt $ U.takeLengthsOfVSegd vsegd


-- | O(1). Lookup a single element from the source array.
index    :: PA a => PArray a -> Int -> a
index (PArray _ arr) ix
        = indexPA arr ix
{-# INLINE_PA index #-}


-- | O(len indices). Lookup a several elements from several source arrays
indexl    :: PA a => PArray (PArray a) -> PArray Int -> PArray a
indexl (PArray n# darr) (PArray _ ixs)
        = PArray n# (indexlPA darr ixs)
{-# INLINE_PA indexl #-}


-- | Extract a range of elements from an array.
extract  :: PA a => PArray a -> Int -> Int -> PArray a
extract (PArray _ arr) start len@(I# len#)
        = PArray len# (extractPA arr start len)
{-# INLINE_PA extract #-}


-- | Segmented extract.
extracts :: PA a => Vector (PArray a) -> U.SSegd -> PArray a
extracts 
        = error "PArray extracts fixme"
{- arrs ssegd
 = let  vecs            = V.map (\(PArray _ vec) -> vec) arrs
        !(I# n#)        = (U.sum $ U.lengthsSSegd ssegd)
   in  PArray   n#
                (extractsPA vecs ssegd) -}
{-# INLINE_PA extracts #-}


-- | Extract a range of elements from an arrary.
--   Like `extract` but with the parameters in a different order.
slice :: PA a => Int -> Int -> PArray a -> PArray a
slice start len@(I# len#) (PArray _ darr)
        = PArray len# (extractPA darr start len)
{-# INLINE_PA slice #-}


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
slicel :: PA a => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicel (PArray n# sliceStarts) (PArray _ sliceLens) (PArray _ darr)
        = PArray n# (slicelPD sliceStarts sliceLens darr)
{-# INLINE_PA slicel #-}


-- | Take the segment descriptor from a nested array and demote it to a
--   plain Segd. This is unsafe because it can cause index space overflow.
unsafeTakeSegd :: PArray (PArray a) -> U.Segd
unsafeTakeSegd (PArray _ pdata)
        = unsafeTakeSegdPD pdata
{-# INLINE unsafeTakeSegd #-}


-- Pack and Combine -----------------------------------------------------------
-- | Select the elements of an array that have their tag set to True.
pack :: PA a => PArray a -> PArray Bool -> PArray a
pack (PArray _ xs) (PArray _ (PBool sel2))
 = let  darr'           = packByTagPA xs (U.tagsSel2 sel2) 1

        -- The selector knows how many elements are set to '1',
        -- so we can use this for the length of the resulting array.
        !(I# m#)        = U.elementsSel2_1 sel2

    in  PArray m# darr'


-- | Lifted pack.
--   Both data and tag arrays must have the same segmentation structure, 
--   but this is not checked.
packl :: forall a. PA a => PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
packl xss bss
 | PArray n# (PNested vsegd xdatas)     <- xss
 , PArray _  (PNested _     bdatas)     <- bss
 = let  
        -- Split up the vsegd into its parts.
        vsegids         = U.takeVSegidsOfVSegd  vsegd
        ssegd           = U.takeSSegdOfVSegd    vsegd

        -- Gather the scattered data together into contiguous arrays, 
        -- which is the form packByTag needs.
        xdata_contig            = extractsPA xdatas ssegd
        bdata'@(PBool sel2)     = extractsPA bdatas ssegd
        tags                    = U.tagsSel2 sel2
         
        -- Pack all the psegs.
        xdata'          = packByTagPA xdata_contig tags 1

        -- Rebuild the segd to account for the possibly smaller segments.
        segd            = U.lengthsToSegd $ U.lengthsSSegd ssegd
        segd'           = U.lengthsToSegd $ U.count_s segd tags 1

        -- Reattach the vsegids, because the top level sharing structure
        -- of the array is unchanged under pack.
        vsegd'          = U.mkVSegd vsegids (U.promoteSegdToSSegd segd')

   in   PArray n# (PNested vsegd' (singletondPA xdata'))
      


-- | Filter an array based on some tags.
packByTag :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTag (PArray _ darr) tags tag
 = let  darr'           = packByTagPA darr tags tag
        !(I# n#)        = lengthPA darr'
   in   PArray n# darr'
{-# INLINE_PA packByTag #-}


-- | Combine two arrays based on a selector.
combine2  :: PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'           = combine2PA sel darr1 darr2
        !(I# n#)        = lengthPA darr'
   in   PArray n# darr'
{-# INLINE_PA combine2 #-}


-- Conversions ----------------------------------------------------------------
-- | Convert a `Vector` to a `PArray`
fromVector :: PA a => Vector a -> PArray a
fromVector vec
 = let !(I# n#) = V.length vec
   in  PArray n#  (fromVectorPA vec)
{-# INLINE_PA fromVector #-}


-- | Convert a `PArray` to a `Vector`        
toVector   :: PA a => PArray a -> Vector a
toVector (PArray _ arr)
        = toVectorPA arr
{-# INLINE_PA toVector #-}


-- | Convert a list to a `PArray`.
fromList :: PA a => [a] -> PArray a
fromList xx
 = let  !(I# n#) = P.length xx
   in   PArray n# (fromVectorPA $ V.fromList xx)
{-# INLINE_PA fromList #-}


-- | Convert a `PArray` to a list.
toList   :: PA a => PArray a -> [a]
toList (PArray _ arr)
        = V.toList $ toVectorPA arr
{-# INLINE_PA toList #-}

