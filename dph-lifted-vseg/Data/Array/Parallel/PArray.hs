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
        ( PArray(..), PA
        , valid
        , nf

        -- * Constructors
        , empty
        , singleton,    singletonl
        , replicate,    replicatel,     replicates,     replicates'
        , append,       appendl
        , concat,       concatl
        , unconcat
        , nestUSegd

        -- * Projections
        , length,       lengthl         -- length from D.A.P.PArray.PData.Base
        , index,        indexl
        , extract,      extracts,       extracts'
        , slice,        slicel
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
import qualified Data.Array.Parallel.Pretty     as T
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import GHC.Exts
import Data.Maybe
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified "dph-lifted-reference" Data.Array.Parallel.PArray as R
import qualified Data.Array.Parallel.Array      as A
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Prelude                        as P
import Prelude hiding 
        ( length, replicate, concat
        , enumFromTo
        , zip, unzip)
        
import Debug.Trace

-- Config ---------------------------------------------------------------------
debugLiftedTrace        = False
debugLiftedCompare      = False


-- Tracing --------------------------------------------------------------------
-- TODO: we could use this to trace the lengths of the vectors being used, 
--       as well as the types that each opeartor is being called at.

instance PA e => A.Array PArray e where
 length arr     = length arr

 index (PArray _ pdata) ix
        = indexPA pdata ix

 append         = append

 -- The toVector conversion used for testing is built by looking up every index
 --  instead of using the bulk fromVectorPA function.
 --  We need to do this to convert arrays of type (PArray Void) properly, as 
 --  although a (PArray Void) has an intrinsic length, a (PData Void) does not.
 --  Arrays of type PArray Void aren't visible in the user API, but during debugging
 --  we need to be able to print them out with the correct length.
 toVector arr
        = V.map (A.index arr) $ V.enumFromTo 0 (A.length arr - 1)

 fromVector
        = fromVector

instance PA a => PprPhysical (PArray a) where
 pprp (PArray n# pdata)
        =     ( T.text "PArray " T.<+> T.int (I# n#))
        T.$+$ ( T.nest 4 
              $ pprpDataPA pdata)

instance PA a => PprPhysical (Vector a) where
 pprp vec
        = T.brackets 
        $ T.hcat
        $ T.punctuate (T.text ", ") 
        $ V.toList $ V.map pprpPA vec

-- TODO: shift this stuff to the reference implementation module.
--       make the PArray constructor polymorphic
-- | Compare a flat array against a reference
withRef1 :: PA a 
         => String
         -> R.PArray a
         -> PArray a
         -> PArray a

withRef1 name arrRef arrImpl
 = let  trace'
         = if debugLiftedTrace  
            then trace (T.render $ T.text " " 
                        T.$$ T.text name 
                        T.$$ (T.nest 8 $ pprpPA arrImpl))
            else id    

        resultOk
         = valid arrImpl
             && A.length arrRef == A.length arrImpl
             && (V.and $ V.zipWith
                  similarPA
                  (A.toVectors1 arrRef) (A.toVectors1 arrImpl))
              
        resultFail
         = error $ T.render $ T.vcat
                [ T.text "withRef1: failure " T.<> T.text name
                , T.nest 4 $ pprp  $ A.toVectors1 arrRef
                , T.nest 4 $ pprpPA arrImpl ]

   in   trace' (if debugLiftedCompare
                 then (if resultOk then arrImpl else resultFail)
                 else arrImpl)
{-# INLINE withRef1 #-}


withRef2 :: PA a 
         => String 
         -> R.PArray (R.PArray a)
         -> PArray (PArray a)
         -> PArray (PArray a)

withRef2 name arrRef arrImpl
 = let  trace'
         = if debugLiftedTrace  
            then trace (T.render $ T.text " " 
                        T.$$ T.text name 
                        T.$$ (T.nest 8 $ pprpPA arrImpl))
            else id

        resultOK
         = valid arrImpl
           && A.length arrRef == A.length arrImpl
           && (V.and $ V.zipWith 
                (\xs ys -> V.and $ V.zipWith similarPA xs ys)
                (A.toVectors2 arrRef) (A.toVectors2 arrImpl))
        
        resultFail
         = error $ T.render $ T.vcat
                [ T.text "withRef2: failure " T.<> T.text name
                , T.nest 4 $ pprpPA arrImpl ]

   in   trace' (if debugLiftedCompare
                 then (if resultOK then arrImpl else resultFail)
                 else arrImpl)
{-# INLINE withRef2 #-}


-- TODO: shift this stuff to the reference implementation module.
--       make the parray constructor polymorphic.
toRef1 :: PA a => PArray a -> R.PArray a
toRef1  = A.fromVectors1 . A.toVectors1

toRef2 :: PA a => PArray (PArray a) -> R.PArray (R.PArray a)
toRef2  = A.fromVectors2 . A.toVectors2

toRef3 :: PA a => PArray (PArray (PArray a)) -> R.PArray (R.PArray (R.PArray a))
toRef3  = A.fromVectors3 . A.toVectors3


-- Basics ---------------------------------------------------------------------
instance (Eq a, PA a)  => Eq (PArray a) where
 (==) (PArray _ xs) (PArray _ ys) = toVectorPA xs == toVectorPA ys
 (/=) (PArray _ xs) (PArray _ ys) = toVectorPA xs /= toVectorPA ys


-- | Check that an array has a valid internal representation.
valid :: PA a => PArray a -> Bool
valid (PArray n# darr1)
        =  validPA  darr1
        && coversPA True darr1 (I# n#)
{-# INLINE_PA valid #-}


-- | Force an array to normal form.
nf :: PA a => PArray a -> ()
nf (PArray n# d)
        = nfPA d
{-# INLINE_PA nf #-}


-- Constructors ----------------------------------------------------------------
-- | O(1). An empty array.
empty :: PA a => PArray a
empty
 = withRef1 "empty" R.empty
 $ PArray 0# emptyPA

{-# INLINE_PA empty #-}


-- | O(1). Produce an array containing a single element.
singleton :: PA a => a -> PArray a
singleton x
 = withRef1 "singleton" (R.singleton x)
 $ PArray 1# (replicatePA 1 x)
{-# INLINE_PA singleton #-}


-- | O(n). Produce an array of singleton arrays.
singletonl :: PA a => PArray a -> PArray (PArray a)
singletonl arr
 = withRef2 "singletonl" (R.singletonl (toRef1 arr))
 $ replicatel (replicate (length arr) 1) arr
{-# INLINE_PA singletonl #-}


-- | O(n). Define an array of the given size, that maps all elements to the same value.
--   We require the replication count to be > 0 so that it's easier to maintain
--   the validPR invariants for nested arrays.
replicate :: PA a => Int -> a -> PArray a
replicate n@(I# n#) x
 = withRef1 "replicate" (R.replicate n x)
 $ PArray n# (replicatePA (I# n#) x)
{-# INLINE_PA replicate #-}


-- | O(sum lengths). Lifted replicate.
replicatel :: PA a => PArray Int -> PArray a -> PArray (PArray a)
replicatel reps@(PArray n# (PInt lens)) arr@(PArray _ pdata)
 = withRef2 "replicatel" (R.replicatel (toRef1 reps) (toRef1 arr))
 $ if n# ==# 0# then empty else 
    let segd    = U.lengthsToSegd lens
        pdata'  = replicatesPA segd pdata
        c       = I# n#
        
     in PArray n# 
         $ mkPNested
                (U.enumFromTo 0 (c - 1))
                lens
                (U.indicesSegd segd)
                (U.replicate c 0)
                (singletondPA pdata')

{-# INLINE_PA replicatel #-}


-- | O(sum lengths). Segmented replicate.
replicates :: PA a => U.Segd -> PArray a -> PArray a
replicates segd arr@(PArray _ pdata)
 = withRef1 "replicates" (R.replicates segd (toRef1 arr))
 $ let  !(I# n#) = U.elementsSegd segd
   in   PArray n# $ replicatesPA segd pdata
{-# INLINE_PA replicates #-}


-- | O(sum lengths). Wrapper for segmented replicate that takes replication counts
--  and uses them to build the `U.Segd`.
replicates' :: PA a => PArray Int -> PArray a -> PArray a
replicates' (PArray _ (PInt reps)) arr
 = replicates (U.lengthsToSegd reps) arr
{-# INLINE_PA replicates' #-}
 
 
-- | Append two arrays.
append :: PA a => PArray a -> PArray a -> PArray a
append arr1@(PArray n1# darr1) arr2@(PArray n2# darr2)
 = withRef1 "append" (R.append (toRef1 arr1) (toRef1 arr2))
 $ PArray (n1# +# n2#) (appendPA darr1 darr2)
{-# INLINE_PA append #-}


-- | Lifted append.
--   Both arrays must have the same length
appendl :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl arr1@(PArray n# pdata1) arr2@(PArray _ pdata2)
 = withRef2 "appendl" (R.appendl (toRef2 arr1) (toRef2 arr2))
 $ PArray n# $ appendlPA pdata1 pdata2
{-# INLINE_PA appendl #-}


-- | Concatenate a nested array.
concat :: PA a => PArray (PArray a) -> PArray a
concat arr@(PArray _ darr)
 = withRef1 "concat" (R.concat (toRef2 arr))
 $ let  darr'    = concatPA darr
        !(I# n#)        = lengthPA darr'
   in   PArray  n# darr'
{-# INLINE_PA concat #-}


-- | Lifted concat.
concatl :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatl arr@(PArray n# pdata1)
 = withRef2 "concatl" (R.concatl (toRef3 arr))
 $ PArray n# $ concatlPA pdata1
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
lengthl arr@(PArray n# (PNested vsegd _))
 = withRef1 "lengthl" (R.lengthl (toRef2 arr))
 $ PArray n# $ PInt $ U.takeLengthsOfVSegd vsegd


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
extracts arrs ssegd
 = let  pdatas          = fromVectordPA $ V.map (\(PArray _ vec) -> vec) arrs
        !(I# n#)        = (U.sum $ U.lengthsSSegd ssegd)
   in   PArray   n#
                (extractsPA pdatas ssegd)
{-# INLINE_PA extracts #-}


-- | Wrapper for `extracts` that takes arrays of sources, starts and lengths of
--   the segments, and uses these to build the `U.SSegd`.
--   TODO: The lengths of the sources, starts and lengths arrays must be the same, 
--         but this is not checked.
--         All sourceids must point to valid data arrays.
--         Segments must be within their corresponding source array.
extracts' 
        :: PA a 
        => Vector (PArray a) 
        -> PArray Int           -- ^ id of source array for each segment.
        -> PArray Int           -- ^ starting index of each segment in its source array.
        -> PArray Int           -- ^ length of each segment.
        -> PArray a
extracts' arrs (PArray _ (PInt sources)) (PArray _ (PInt starts)) (PArray _ (PInt lengths))
 = let  segd    = U.lengthsToSegd lengths
        ssegd   = U.mkSSegd starts sources segd
   in   extracts arrs ssegd
{-# INLINE_PA extracts' #-}
        

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
{-# INLINE_PA unsafeTakeSegd #-}


-- Pack and Combine -----------------------------------------------------------
-- | Select the elements of an array that have their tag set to True.
pack :: PA a => PArray a -> PArray Bool -> PArray a
pack arr@(PArray _ xs) flags@(PArray _ (PBool sel2))
 = withRef1 "pack" (R.pack (toRef1 arr) (toRef1 flags))
 $ let  darr'           = packByTagPA xs (U.tagsSel2 sel2) 1

        -- The selector knows how many elements are set to '1',
        -- so we can use this for the length of the resulting array.
        !(I# m#)        = U.elementsSel2_1 sel2

    in  PArray m# darr'
{-# INLINE_PA pack #-}


-- | Lifted pack.
packl :: PA a => PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
packl xss@(PArray n# xdata@(PNested vsegd _))
      fss@(PArray _  fdata)
 = withRef2 "packl" (R.packl (toRef2 xss) (toRef2 fss))
 $ let  
        -- Demote the vsegd to get the virtual segmentation of the two arrays.
        -- The virtual segmentation of both must be the same, but this is not checked.
        segd            = U.demoteToSegdOfVSegd vsegd
        
        -- Concatenate both arrays to get the flat data.
        --   Although the virtual segmentation should be the same,
        --   the physical segmentation of both arrays may be different.
        xdata_flat              = concatPA xdata
        fdata_flat@(PBool sel)  = concatPA fdata
        tags                    = U.tagsSel2 sel
        
        -- Count how many elements go into each segment.        
        segd'           = U.lengthsToSegd $ U.count_s segd tags 1

        -- Build the result array
        vsegd'          = U.promoteSegdToVSegd segd'
        xdata'          = packByTagPA xdata_flat tags 1
        
   in   PArray n# (PNested vsegd' $ singletondPA xdata')
{-# INLINE_PA packl #-}


-- | Filter an array based on some tags.
packByTag :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTag arr@(PArray _ darr) tags tag
 = withRef1 "packByTag" (R.packByTag (toRef1 arr) tags tag)
 $ let  darr'           = packByTagPA darr tags tag
        !(I# n#)        = lengthPA darr'
   in   PArray  n# darr'

{-# INLINE_PA packByTag #-}


-- | Combine two arrays based on a selector.
combine2  :: forall a. PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 sel arr1@(PArray _ darr1) arr2@(PArray _ darr2)
 = withRef1 "combine2" (R.combine2 sel (toRef1 arr1) (toRef1 arr2))
 $ let  darr'           = combine2PA sel darr1 darr2
        !(I# n#)        = lengthPA darr'
   in   PArray  n# darr'
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

