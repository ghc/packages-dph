
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
        
        -- * Projections
        , length,       lengthl
        , index,        indexl
        , extract
        
        -- * Pack and Combine
        , pack,         packl
        , packByTag
        , combine2)
where
import Data.Array.Parallel.Base                 (Tag)
import Data.Vector                              (Vector)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.Array      as A
import qualified Data.Vector                    as V
import Control.Monad
import GHC.Exts
import Prelude
        hiding (replicate, length, concat)

die fn str = error $ "Data.Array.Parallel.PArray: " ++ fn ++ " " ++ str

-- | Parallel Ararys.
data PArray a
        = PArray Int# (Vector a)
        deriving (Eq, Show)


-- Array Instances ------------------------------------------------------------
instance A.Array PArray a where
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


-- | Lift a unary array operator
lift1 :: (a -> b) -> PArray a -> PArray b
lift1 f (PArray n# vec)
        = PArray n# $ V.map f vec


-- | Lift a unary array operator
lift2 :: (a -> b -> c) -> PArray a -> PArray b -> PArray c
lift2 f (PArray n1# vec1) (PArray n2# vec2)
 | I# n1# /= I# n2# 
 = die "lift2" "length mismatch"
 
 | otherwise
 = PArray n1# $ V.zipWith f vec1 vec2


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

-----------------------------------------------------------
-- TODO: unconcat
-----------------------------------------------------------

-----------------------------------------------------------
-- TODO: nestUSegd
-----------------------------------------------------------


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
combine2 tags (PArray n1# vec1) (PArray n2# vec2)
 = let  
        go [] [] [] = []
        go (0 : bs) (x : xs) ys       = x : go bs xs ys
        go (1 : bs) xs       (y : ys) = y : go bs xs ys
 
        vec3    = V.fromList
                $ go    (V.toList $ V.convert $ U.tagsSel2 tags)
                        (V.toList vec1)
                        (V.toList vec2)
        !(I# n') = V.length vec3
   
    in  PArray n' vec3
