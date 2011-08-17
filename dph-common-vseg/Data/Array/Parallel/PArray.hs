
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
        , emptyPA
        , nfPA
        , replicatePA
        , fromVectorPA
        , toVectorPA
        , indexPA
        , extractPA
        , extractsPA
        , appPA
        , packByTagPA
        , combine2PA
        
        -- Derived operators.
        , fromListPA
        , toListPA
        , concatPA

        -- Wrappers used for testing only.
        , replicatesPA'
        , extractsPA'
        , packByTagPA'
        , combine2PA')
where
import Data.Array.Parallel.PArray.PData
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V

instance (Eq a, PR a)  => Eq (PArray a) where
 (==) xs ys = toVectorPA xs == toVectorPA ys
 (/=) xs ys = toVectorPA xs /= toVectorPA ys


-- | An empty array.
{-# INLINE_PA emptyPA #-}
emptyPA :: PR a => PArray a
emptyPA
        = PArray 0 emptyPR


-- | Force an array to normal form.
{-# INLINE_PA nfPA #-}
nfPA :: PR a => PArray a -> ()
nfPA (PArray n d)
        = n `seq` nfPR d


-- | Create an array of the given size that maps all elements to the same value.
{-# INLINE_PA replicatePA #-}
replicatePA :: PR a => Int -> a -> PArray a
replicatePA n x
        = PArray n (replicatePR n x)


-- | Convert a `Vector` to a `PArray`
{-# INLINE_PA fromVectorPA #-}
fromVectorPA :: PR a => Vector a -> PArray a
fromVectorPA vec
        = PArray (V.length vec) (fromVectorPR vec)


-- | Convert a `PArray` to a `Vector`        
{-# INLINE_PA toVectorPA #-}
toVectorPA   :: PR a => PArray a -> Vector a
toVectorPA (PArray _ arr)
        = toVectorPR arr


-- | Lookup a single element from the source array.
{-# INLINE_PA indexPA #-}
indexPA    :: PR a => PArray a -> Int -> a
indexPA (PArray _ arr) ix
        = indexPR arr ix


-- | Extract a range of elements from an array.
{-# INLINE_PA extractPA #-}
extractPA  :: PR a => PArray a -> Int -> Int -> PArray a
extractPA (PArray _ arr) start len
        = PArray len (extractPR arr start len)


-- | Segmented extract.
{-# INLINE_PA extractsPA #-}
extractsPA 
        :: PR a 
        => Vector (PArray a)    -- ^ source array
        -> U.Array Int          -- ^ segment source ids
        -> U.Array Int          -- ^ segment base indices
        -> U.Array Int          -- ^ segment lengths
        -> PArray a

extractsPA arrs srcids segIxs segLens
 = let vecs     = V.map (\(PArray _ vec) -> vec) arrs
   in  PArray   (U.length srcids)
                (extractsPR vecs srcids segIxs segLens)


-- | Append two arrays.
{-# INLINE_PA appPA #-}
appPA      :: PR a => PArray a -> PArray a -> PArray a
appPA (PArray n1 darr1) (PArray n2 darr2)
        = PArray (n1 + n2) (darr1 `appPR` darr2)


-- | Filter an array based on some tags.
{-# INLINE_PA packByTagPA #-}
packByTagPA :: PR a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTagPA (PArray _ darr) tags tag
 = let  darr'   = packByTagPR darr tags tag
   in   PArray (lengthPR darr') darr'


-- | Combine two arrays based on a selector.
{-# INLINE_PA combine2PA #-}
combine2PA  :: PR a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2PA sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'    = combine2PR sel darr1 darr2
   in   PArray (lengthPR darr') darr'


-- | Convert a list to a `PArray`.
{-# INLINE_PA fromListPA #-}
fromListPA :: PR a => [a] -> PArray a
fromListPA xx
        = PArray (length xx) (fromVectorPR $ V.fromList xx)


-- | Convert a `PArray` to a list.
{-# INLINE_PA toListPA #-}
toListPA   :: PR a => PArray a -> [a]
toListPA (PArray _ arr)
        = V.toList $ toVectorPR arr


-- | Concatenate a nested array.
concatPA :: PR a => PArray (PArray a) -> PArray a
concatPA (PArray n darr)
 = let  darr'   = concatPR darr
   in   PArray (lengthPR darr') darr'


-------------------------------------------------------------------------------
-- These PArray functions are just for testing 
-------------------------------------------------------------------------------

-- | Create an array of the given size that maps all elements to the same value.
replicatesPA' :: PR a => [Int] -> PArray a -> PArray a
replicatesPA' lens (PArray _ darr)
        = PArray (sum lens) (replicatesPR (U.fromList lens) darr)


-- | Segmented extract.
extractsPA' :: PR a => V.Vector (PArray a) -> [Int] ->  [Int] -> [Int] -> PArray a
extractsPA' arrs srcids startixs seglens
        = PArray (sum seglens) 
        $ extractsPR (V.map unpackPA arrs) 
                     (U.fromList srcids) (U.fromList startixs) (U.fromList seglens)


-- | NOTE: Returns an array with a fake size for testing purposes.
packByTagPA' :: PR a => PArray a -> [Int] -> Int -> PArray a
packByTagPA' (PArray n arr) tags tag
 = let  arr'    = packByTagPR arr (U.fromList tags) tag
   in   PArray 0 arr'


combine2PA' :: PR a => [Int] -> PArray a -> PArray a -> PArray a
combine2PA' sel (PArray _ darr1) (PArray _ darr2)
 = let  darr'   = combine2PR (U.tagsToSel2 (U.fromList sel)) darr1 darr2
   in   PArray 0 darr'




-- Finish me ------------------------------------------------------------------
{-
-- | Convert an unlifted array of pairs to a PArray of pairs.
{-# INLINE_PA fromUArray2PA #-}
fromUArray2PA   :: (U.Elt a, PA a, U.Elt b, PA b) 
                => U.Array (a, b) -> PArray (a, b)
fromUArray2PA arr
 = let  (xs, ys)        = U.unzip arr
   in   PArray  (U.length arr) 
                (PTuple2 (fromUArrayPS xs) (fromUArrayPS ys))


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
-}                

