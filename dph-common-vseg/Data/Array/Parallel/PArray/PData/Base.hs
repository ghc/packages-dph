{-# LANGUAGE 
        TypeFamilies, MultiParamTypeClasses,
        FlexibleContexts, ExplicitForAll,
        StandaloneDeriving,
        UndecidableInstances #-}
        -- Undeciable instances only need for derived Show instance

module Data.Array.Parallel.PArray.PData.Base 
        ( -- * Parallel Array types.
          PArray(..)
        , emptyPA
        , lengthPA,     unpackPA
        , fromVectorPA, toVectorPA
        , indexPA
        , extractPA,    extractsPA
        , appPA
        , fromListPA,   toListPA
        , PprPhysical (..), PprVirtual (..)
        , PData (..)
        , PR(..)
        
        , uextracts
        , replicatePA, replicatesPA'
        , extractsPA'
        , packByTagPA'
        , combine2PA')

where
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import Text.PrettyPrint

-- PArray ---------------------------------------------------------------------
-- | A parallel array. 
--   PArrays always contain a finite (sized) number of elements, which means
--   they have a length.
data PArray a
	= PArray Int (PData  a)

deriving instance (Show (PData a), Show a)
	=> Show (PArray a)


-- | Take the length of an array
{-# INLINE_PA lengthPA #-}
lengthPA :: forall a. PArray a -> Int
lengthPA (PArray n _)   = n


-- | Take the data from an array.
{-# INLINE_PA unpackPA #-}
unpackPA :: PArray a -> PData a
unpackPA (PArray _ d)   = d


-- Pretty Printer classes -----------------------------------------------------
-- | Pretty print physical structure of data.
class PprPhysical a where
 pprp :: a -> Doc
 
instance PprPhysical (PData a) => PprPhysical (PArray a) where
 pprp (PArray n dat)
  =   (text "PArray " <+> int n)
  $+$ (nest 4 
      $ pprp dat)
 
-- | Pretty print virtual / logical structure of data.
class PprVirtual a where
 pprv :: a -> Doc

instance PprVirtual (PData a) => PprVirtual (PArray a) where
 pprv (PArray _ dat)
  =   pprv dat
 
 
-- PData ----------------------------------------------------------------------
-- | Parallel array data.
--   As opposed to finite PArrays, a PData can represent a finite or infinite
--   number of array elements, depending on the mode. The infinite case simply
--   means that all possible array indices map to some element value.
--   
data family PData a


-- PR Dictionary (Representation) ---------------------------------------------
class PR a where
  -- | Produce an empty array with size zero.
  emptyPR	:: PData a

  -- | Ensure there are no thunks in the representation of a manifest array.
  nfPR          :: PData a -> ()

  -- | Define an array of the given size, that maps all elements to the same value.
  --   O(n). 
  replicatePR   :: Int    -> a           -> PData a

  -- | Segmented replicate.
  --   O(sum lengths). 
  replicatesPR  :: U.Array Int -> PData a -> PData a

  -- | Lookup a single element from the source array.
  --   O(1). 
  indexPR       :: PData a    -> Int -> a

  -- | Extract a range of elements from an array.
  --   O(n). 
  extractPR     :: PData a -> Int -> Int -> PData a

  -- | Segmented extract.
  --   O(sum seglens).  
  extractsPR    :: Vector (PData a)
                -> U.Array Int  -- ^ segment source ids
                -> U.Array Int  -- ^ segment base indices
                -> U.Array Int  -- ^ segment lengths
                -> PData a

  -- | Append two sized arrays.
  appPR		:: PData a -> PData a -> PData a

  -- | Filter an array based on some tags.
  packByTagPR   :: PData a      -- ^ source array
                -> U.Array Tag  -- ^ array of tags
                -> Tag          -- ^ tag of elements to select
                -> PData a

  -- | Combine two arrays based on a selector.
  combine2PR    :: U.Sel2       -- ^ selector
                -> PData a      -- ^ first source array
                -> PData a      -- ^ second source array
                -> PData a

  -- Conversions ---------------------
  -- | Convert a list to an array
  fromVectorPR	:: Vector a -> PData a

  -- | Convert an array to a list
  toVectorPR    :: PData a -> Vector a

  -- TODO: Shift these into the Scalar class like existing library.  
  -- | Convert an unlifted array to a PData. 
  fromUArrayPR  :: U.Elt a => U.Array a -> PData a
  
  -- | Convert a PData into an unlifted array.
  toUArrayPR    :: U.Elt a => PData a -> U.Array a




----------------------------------------------------------------------------------
-- These PArray functions should be moved into D.A.P.PArray when closures work ---
----------------------------------------------------------------------------------

instance (Eq a, PR a)  => Eq (PArray a) where
 (==) xs ys = toVectorPA xs == toVectorPA ys
 (/=) xs ys = toVectorPA xs /= toVectorPA ys

-- | An empty array.
{-# INLINE_PA emptyPA #-}
emptyPA :: PR a => PArray a
emptyPA
        = PArray 0 emptyPR


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


-------------------------------------------------------------------------------
-- These PArray functions are just for testing 
-------------------------------------------------------------------------------

replicatesPA' :: PR a => [Int] -> PArray a -> PArray a
replicatesPA' lens (PArray _ darr)
        = PArray (sum lens) (replicatesPR (U.fromList lens) darr)


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


-------------------------------------------------------------------------------
-- extra unlifted primitives should be moved into unlifted library ------------
-------------------------------------------------------------------------------

{-# INLINE uextracts #-}
uextracts 
        :: U.Elt a 
        => V.Vector (U.Array a) 
        -> U.Array Int  -- source ids
        -> U.Array Int  -- base indices
        -> U.Array Int  -- segment lengths
        -> U.Array a

uextracts arrs srcids ixBase lens 
 = let -- total length of the result
        dstLen    = U.sum lens
        segd      = U.lengthsToSegd lens
    
        -- source array ids to load from
        srcids'   = U.replicate_s segd srcids

        -- base indices in the source array to load from
        baseixs   = U.replicate_s segd ixBase
        
        -- starting indices for each of the segments
        startixs  = U.scan (+) 0 lens
          
        -- starting indices for each of the segments in the result
        startixs' = U.replicate_s segd startixs

        result    = U.zipWith3
                        (\ixDst ixSegDst (ixSegSrcBase, srcid)
                                -> (arrs V.!  srcid) U.!: (ixDst - ixSegDst + ixSegSrcBase))
                        (U.enumFromTo 0 (dstLen - 1))
                        startixs'
                        (U.zip baseixs srcids')
   in result

