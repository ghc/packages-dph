{-# LANGUAGE 
        TypeFamilies, MultiParamTypeClasses,
        FlexibleContexts, ExplicitForAll,
        StandaloneDeriving,
        UndecidableInstances #-}
        -- Undeciable instances only need for derived Show instance

module Data.Array.Parallel.PArray.PData.Base 
        ( -- * Parallel Array types.
          PArray(..)
        , lengthPA, unpackPA
        , replicatePA, replicatesPA', extractsPA'
        , packByTagPA'
        , fromListPA, toListPA
        , PprPhysical (..), PprVirtual (..)
        , PData (..)
        , PR(..)
        
        , uextracts1, uextracts)
where
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
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


-- | Replicate an array
{-# INLINE_PA replicatePA #-}
replicatePA :: PR a => Int -> a -> PArray a
replicatePA n x
        = PArray n (replicatePR n x)


replicatesPA' :: PR a => [Int] -> PArray a -> PArray a
replicatesPA' lens (PArray _ darr)
        = PArray (sum lens) (replicatesPR (U.fromList lens) darr)


extractsPA' :: PR a => (Int -> PArray a) -> [Int] ->  [Int] -> [Int] -> PArray a
extractsPA' getArr srcids startixs seglens
        = PArray (sum seglens) 
        $ extractsPR (unpackPA . getArr) 
                     (U.fromList srcids) (U.fromList startixs) (U.fromList seglens)


-- | NOTE: Returns an array with a fake size for testing purposes.
packByTagPA' :: PR a => PArray a -> [Int] -> Int -> PArray a
packByTagPA' (PArray n arr) tags tag
 = let  arr'    = packByTagPR arr (U.fromList tags) tag
   in   PArray 0 arr'


{-# INLINE_PA fromListPA #-}
fromListPA :: forall a. PR a => [a] -> PArray a
fromListPA xx
        = PArray (length xx) (fromListPR xx)

{-# INLINE_PA toListPA #-}
toListPA   :: forall a. PR a => PArray a -> [a]
toListPA (PArray _ arr)
        = toListPR arr


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

  -- | O(n). Define an array of the given size, that maps all elements to the same value.
  replicatePR   :: Int    -> a           -> PData a

  -- | O(sum lens). Segmented replicate.
  replicatesPR  :: U.Array Int -> PData a -> PData a

  -- | O(1). Lookup a single element from the source array.
  indexPR       :: PData a    -> Int -> a

  -- | O(n). Extract a range of elements from an array.
  extractPR     :: PData a    -> Int -> Int -> PData a

  -- | Segmented extract.
  extractsPR    :: (Int -> PData a) -> U.Array Int -> U.Array Int -> U.Array Int -> PData a

  -- | Append two sized arrays.
  appPR		:: PData a -> PData a -> PData a

  -- | Filter an array based on some tags.
  packByTagPR   :: PData a -> U.Array Tag -> Tag -> PData a

  -- Conversions ---------------------
  -- | Convert a list to an array
  fromListPR	:: [a] -> PData a

  -- | Convert an array to a list
  toListPR      :: PData a -> [a]

  -- TODO: Shift these into the Scalar class like existing library.  
  -- | Convert an unlifted array to a PData. 
  fromUArrayPR  :: U.Elt a => U.Array a -> PData a
  
  -- | Convert a PData into an unlifted array.
  toUArrayPR    :: U.Elt a => PData a -> U.Array a

{-# INLINE uextracts1 #-}
uextracts1 :: U.Elt a => U.Array a -> U.Array Int -> U.Array Int -> U.Array a
uextracts1 arr ixBase lens
 = let  lenTotal        = U.sum lens
   in   uextracts (const arr) (U.replicate lenTotal 0) ixBase lens


{-# INLINE uextracts #-}
uextracts :: U.Elt a => (Int -> U.Array a) -> U.Array Int -> U.Array Int -> U.Array Int -> U.Array a
uextracts getArr srcids ixBase lens 
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
                                -> getArr srcid U.!: (ixDst - ixSegDst + ixSegSrcBase))
                        (U.enumFromTo 0 (dstLen - 1))
                        startixs'
                        (U.zip baseixs srcids')
   in result
   
 
    