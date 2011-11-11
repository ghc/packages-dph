
-- | Unvectorised parallel arrays.
--
--   * These operators may be used directly by unvectorised client programs.
--
--   * They are also used by the "Data.Array.Parallel.Lifted.Combinators"
--     module to define the closure converted versions that vectorised code
--     uses.
--
--   * In general, the operators here are all unsafe and don't do bounds checks.
--     The lifted versions also don't check that each of the argument arrays
--     have the same length.
--
--   TODO: check lengths properly in functions like zip, extracts
--
module Data.Array.Parallel.PArray
        ( PArray(..)
        , valid
        , nf
        
        -- * Constructors
        , empty
        , singleton,    singletonl
        , replicate,    replicatel,     replicates,     replicates'
        , append,       appendl
        , concat,       concatl
        , unconcat
        , nestSegd
        
        -- * Projections
        , length,       lengthl
        , index,        indexl
        , extract,      extracts,       extracts'
        , slice,        slicel
        , takeSegd
        
        -- * Pack and Combine
        , pack,         packl
        , packByTag
        , combine2
        
        -- * Enumerations
        , enumFromTo,   enumFromTol
        
        -- * Tuples
        , zip,          zipl
        , unzip,        unzipl
        
        -- * Conversions
        , fromVector,   toVector
        , fromList,     toList
        , fromUArray,   toUArray
        , fromUArray2)
where
import Data.Array.Parallel.Base                 (Tag)
import Data.Vector                              (Vector)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.Array      as A
import qualified Data.Vector                    as V
import Control.Monad
import GHC.Exts
import qualified Prelude                        as P
import Prelude hiding
        ( replicate, length, concat
        , enumFromTo
        , zip, unzip)

die fn str = error $ "Data.Array.Parallel.PArray: " ++ fn ++ " " ++ str

-- | Parallel Ararys.
data PArray a
        = PArray Int# (Vector a)
        deriving (Eq, Show)


-- Array Instances ------------------------------------------------------------
instance A.Array PArray a where
 valid     = const True
 singleton = A.singleton

 length  (PArray _ vec)
        = V.length vec

 index (PArray _ vec) ix
        = vec V.! ix

 append (PArray n1# xs) (PArray n2# ys)
        = PArray (n1# +# n2#) (xs V.++ ys)

 toVector (PArray _ vec)
        = vec

 fromVector vec
  = case V.length vec of
        I# n# -> PArray n# vec


-- | Lift a unary array operator.
lift1 :: (a -> b) -> PArray a -> PArray b
lift1 f (PArray n# vec)
        = PArray n# $ V.map f vec


-- | Lift a binary array operator.
lift2 :: (a -> b -> c) -> PArray a -> PArray b -> PArray c
lift2 f (PArray n1# vec1) (PArray n2# vec2)
 | I# n1# /= I# n2# 
 = die "lift2" "length mismatch"
 
 | otherwise
 = PArray n1# $ V.zipWith f vec1 vec2


-- | Lift a trinary array operator
lift3 :: (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d
lift3 f (PArray n1# vec1) (PArray n2# vec2) (PArray n3# vec3)
 |   I# n1# /= I# n2# 
  || I# n1# /= I# n3#
 = die "lift3" "length mismatch"
 
 | otherwise
 = PArray n1# $ V.zipWith3 f vec1 vec2 vec3


-- Basics ---------------------------------------------------------------------
-- | Check that an array has a valid internal representation.
valid :: PArray a -> Bool
valid _ = True

-- | Force an array to normal form.
nf :: PArray a -> ()
nf _    = ()


-- Constructors ----------------------------------------------------------------
-- | O(1). An empty array.
empty :: PArray a
empty           = PArray 0# V.empty


-- | O(1). Produce an array containing a single element.
singleton :: a -> PArray a
singleton x     = PArray 1# (V.singleton x)


-- | O(n). Produce an array of singleton arrays.
singletonl :: PArray a -> PArray (PArray a)
singletonl = lift1 singleton


-- | O(n). Define an array of the given size, that maps all elements to the same value.
replicate :: Int -> a -> PArray a
replicate n@(I# n#) x
        = PArray n# (V.replicate n x)


-- | O(sum lengths). Lifted replicate.
replicatel :: PArray Int -> PArray a -> PArray (PArray a)
replicatel = lift2 replicate


-- | O(sum lengths). Segmented replicate.
replicates :: U.Segd -> PArray a -> PArray a
replicates segd (PArray n# vec)
 | I# n# /= U.lengthSegd segd
 = die "replicates" $ unlines
        [ "segd length mismatch"
        , "  segd length  = " ++ show (U.lengthSegd segd)
        , "  array length = " ++ show (I# n#) ]

 | otherwise
 = let  !(I# n2#) = U.elementsSegd segd
   in   PArray n2# 
         $ join $ V.zipWith V.replicate
                        (V.convert $ U.lengthsSegd segd)
                        vec


-- | O(sum lengths). Wrapper for segmented replicate that takes replication counts
--  and uses them to build the `U.Segd`.
replicates' :: PArray Int -> PArray a -> PArray a
replicates' (PArray _ reps) arr
 = replicates (U.lengthsToSegd $ V.convert reps) arr


-- | Append two arrays.
append :: PArray a -> PArray a -> PArray a
append (PArray n1# xs) (PArray n2# ys)
        = PArray (n1# +# n2#) (xs V.++ ys)


-- | Lifted append.
appendl :: PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl = lift2 append


-- | Concatenation
concat :: PArray (PArray a) -> PArray a
concat (PArray _ xss)
 = let  xs       = join $ V.map A.toVector xss
        !(I# n') = V.length xs
   in   PArray n' xs


-- | Lifted concatenation
concatl :: PArray (PArray (PArray a)) -> PArray (PArray a)
concatl = lift1 concat


-- | Impose a nesting structure on a flat array
unconcat ::  PArray (PArray a) -> PArray b -> PArray (PArray b)
unconcat arr1 arr2
        = nestSegd (takeSegd arr1) arr2


-- | Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
nestSegd :: U.Segd -> PArray a -> PArray (PArray a)
nestSegd segd (PArray n# vec)
        | U.elementsSegd segd     == I# n#
        , I# n2#                <- U.lengthSegd segd
        = PArray n2#
        $ V.zipWith
                (\start len@(I# len#) -> PArray len# $ V.slice start len vec)
                (V.convert $ U.indicesSegd segd)
                (V.convert $ U.lengthsSegd segd)

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestSegd: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ show (U.elementsSegd segd)
                , " length of data array        = " ++ show (I# n#) ]
{-# NOINLINE nestSegd #-}


-- Projections ----------------------------------------------------------------
-- | Take the length of an array
length :: PArray a -> Int
length (PArray n# _)    = I# n#


-- | Take the length of some arrays.
lengthl :: PArray (PArray a) -> PArray Int
lengthl = lift1 length


-- | Lookup a single element from the source array.
index :: PArray a -> Int -> a
index (PArray _ arr) ix
        = arr V.! ix


-- | Lookup a several elements from several source arrays.
indexl :: PArray (PArray a) -> PArray Int -> PArray a
indexl  = lift2 index


-- | Extract a range of elements from an array.
extract :: PArray a -> Int -> Int -> PArray a
extract (PArray _ vec) start len@(I# len#)
        = PArray len# $ V.slice start len vec


-- | Segmented extract.
extracts :: Vector (PArray a) -> U.SSegd -> PArray a
extracts arrs ssegd
        = concat
        $ fromVector
        $ V.zipWith3
                (\src start len -> extract (arrs V.! src) start len)
                (V.convert $ U.sourcesSSegd ssegd)
                (V.convert $ U.startsSSegd  ssegd)
                (V.convert $ U.lengthsSSegd ssegd)


-- | Wrapper for `extracts` that takes arrays of sources, starts and lengths of
--   the segments, and uses these to build the `U.SSegd`.
extracts' 
        :: Vector (PArray a) 
        -> PArray Int           -- ^ id of source array for each segment.
        -> PArray Int           -- ^ starting index of each segment in its source array.
        -> PArray Int           -- ^ length of each segment.
        -> PArray a
extracts' arrs (PArray _ sources) (PArray _ starts) (PArray _ lengths)
 = let  segd    = U.lengthsToSegd $ V.convert lengths
        ssegd   = U.mkSSegd 
                        (V.convert starts)
                        (V.convert sources)
                        segd
   in   extracts arrs ssegd


-- | Extract a range of elements from an arrary.
--   Like `extract` but with the parameters in a different order.
slice :: Int -> Int -> PArray a -> PArray a
slice start len arr
 = extract arr start len


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
slicel :: PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicel  = lift3 slice


-- | Take the segment descriptor from a nested array. This can cause index space
--   overflow if the number of elements in the result does not can not be
--   represented by a single machine word.
takeSegd :: (PArray (PArray a)) -> U.Segd
takeSegd (PArray _ vec)
        = U.lengthsToSegd 
        $ V.convert
        $ V.map length vec
        

-- Pack and Combine -----------------------------------------------------------
-- | Select the elements of an array that have their tag set to True.
pack    :: PArray a -> PArray Bool -> PArray a
pack (PArray n1# xs) (PArray n2# bs)
 | I# n1# /= I# n2#
 = die "pack" $ unlines
        [ "array length mismatch"
        , "  data  length = " ++ show (I# n1#)
        , "  flags length = " ++ show (I# n2#) ]

 | otherwise
 = let  xs'      = V.ifilter (\i _ -> bs V.! i) xs
        !(I# n') = V.length xs'
   in   PArray n' xs'

-- | Lifted pack.
packl :: PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
packl   = lift2 pack


-- | Filter an array based on some tags.
packByTag :: PArray a -> U.Array Tag -> Tag -> PArray a
packByTag (PArray n1# xs) tags tag
 | I# n1# /= U.length tags
 = die "packByTag" $ unlines
        [ "array length mismatch"
        , "  data  length = " ++ show (I# n1#)
        , "  flags length = " ++ (show $ U.length tags) ]

 | otherwise
 = let  xs'      = V.ifilter (\i _ -> tags U.!: i == tag) xs
        !(I# n') = V.length xs'
   in   PArray n' xs'


-- | Combine two arrays based on a selector.
combine2 :: U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 tags (PArray _ vec1) (PArray _ vec2)
 = let  
        go [] [] []                     = []
        go (0 : bs) (x : xs) ys         = x : go bs xs ys
        go (1 : bs) xs       (y : ys)   = y : go bs xs ys
        go _ _ _ = error "Data.Array.Parallel.PArray.combine: length mismatch"
 
        vec3    = V.fromList
                $ go    (V.toList $ V.convert $ U.tagsSel2 tags)
                        (V.toList vec1)
                        (V.toList vec2)
        !(I# n') = V.length vec3
   
    in  PArray n' vec3


-- Enumerations ---------------------------------------------------------------
-- | Construct a range of integers
enumFromTo :: Int -> Int -> PArray Int
enumFromTo m n 
        = fromList [m..n]


-- | Lifted enumeration
enumFromTol :: PArray Int -> PArray Int -> PArray (PArray Int)
enumFromTol = lift2 enumFromTo


-- Tuples ---------------------------------------------------------------------
-- | O(n). Zip a pair of arrays into an array of pairs.
zip :: PArray a -> PArray b -> PArray (a, b)
zip (PArray n1# vec1) (PArray _ vec2)
        = PArray n1# (V.zip vec1 vec2)


-- | Lifted zip
zipl    :: PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
zipl    = lift2 zip


-- | O(n). Unzip an array of pairs into a pair of arrays.
unzip   :: PArray (a, b) -> (PArray a, PArray b)
unzip (PArray n# vec)
 = let  (xs, ys)        = V.unzip vec
   in   (PArray n# xs, PArray n# ys)


-- | Lifted unzip
unzipl  :: PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
unzipl  = lift1 unzip


-- Conversions ----------------------------------------------------------------
-- | Convert a `Vector` to a `PArray`
fromVector :: Vector a -> PArray a
fromVector vec
 = let  !(I# n#) = V.length vec
   in   PArray n# vec


-- | Convert a `PArray` to a `Vector`        
toVector   :: PArray a -> Vector a
toVector (PArray _ vec)
        = vec


-- | Convert a list to a `PArray`.
fromList :: [a] -> PArray a
fromList xx
 = let  !(I# n#) = P.length xx
   in   PArray n# (V.fromList xx)


-- | Convert a `PArray` to a list.
toList     :: PArray a -> [a]
toList (PArray _ vec)
        = V.toList vec


-- | Convert a `U.Array` to a `PArray`
fromUArray :: U.Elt a => U.Array a -> PArray a
fromUArray uarr
 = let  !(I# n#) = U.length uarr
   in   PArray n# (V.convert uarr)


-- | Convert a `PArray` to a `U.Array`
toUArray :: U.Elt a => PArray a -> U.Array a
toUArray (PArray _ vec)
        = V.convert vec


-- | Convert a `U.Array` of tuples to a `PArray`
fromUArray2
        :: (U.Elt a, U.Elt b)
        => U.Array (a, b) -> PArray (a, b)
        
fromUArray2 uarr
 = let  !(I# n#) = U.length uarr
   in   PArray n# $ V.convert uarr
