{-# LANGUAGE 
        CPP,
        TypeFamilies, MultiParamTypeClasses,
        FlexibleContexts, ExplicitForAll,
        StandaloneDeriving,
        UndecidableInstances #-}
        -- Undeciable instances only need for derived Show instance

#include "fusion-phases-vseg.h"

module Data.Array.Parallel.PArray.PData.Base 
        ( -- * Parallel Array types.
          PArray(..)
        , lengthPA,     unpackPA
        , PprPhysical (..), PprVirtual (..)
        , PData (..)
        , PR(..)
        
        , uextracts)
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
 
instance (PprPhysical (PData a)) => PprPhysical (PArray a) where
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
data family PData a


-- PR Dictionary (Representation) ---------------------------------------------
class PR a where
  -- | Check that an array has a well formed representation.
  --   This should only return False where there is a bug in the library.
  validPR       :: PData a -> Bool

  -- | Produce an empty array with size zero.
  emptyPR       :: PData a

  -- | Ensure there are no thunks in the representation of a manifest array.
  nfPR          :: PData a -> ()

  -- | Get the number of elements in an array.
  --   For nested arrays this is just the length of the top level of nesting,
  --   not the total number of elements in the array.

  --   TODO: We want a length function so we can use it in validPR,
  --         but it should return  Nothing when the array is 'defined everywhere', 
  --         like with arrays of ().
  lengthPR      :: PData a -> Int

  -- | Define an array of the given size, that maps all elements to the same value.
  --   We require the replication count to be > 0 so that it's easier to maintain
  --   the validPR invariants for nested arrays.
  --   O(n). 
  replicatePR   :: Int          -- ^ length of result array. Must be > 0.
                -> a            -- ^ element to replicate.
                -> PData a

  -- | Segmented replicate.
  --   O(sum lengths). 
  replicatesPR       :: U.Array Int -> PData a -> PData a

  -- | Unsafe segmented replicate is not guaranteed to preserve the validPR
  --   condition that all psegs are referenced by some vseg. This can happen
  --   when some of the rep counts are zero. 
  -- 
  --   unsafeReplicatesPR can be used when the result is passed to a function
  --   that doesn't require this condition, such as concatPA. Doing this
  --   saves us from needing to explicitly filter out the unused psegs.
  unsafeReplicatesPR :: U.Array Int -> PData a -> PData a
  unsafeReplicatesPR = replicatesPR

  -- | Lookup a single element from the source array.
  --   O(1). 
  indexPR       :: PData a    -> Int -> a

  -- | Lookup several elements from several source arrays
  indexlPR      :: Int -> PData (PArray a) -> PData Int -> PData a

  -- | Extract a range of elements from an array.
  --   O(n). 
  extractPR     :: PData a 
                -> Int                  -- ^ starting index
                -> Int                  -- ^ length of slice
                -> PData a

  -- | Segmented extract.
  --   O(sum seglens).  
  extractsPR    :: Vector (PData a)
                -> U.Array Int          -- ^ segment source ids
                -> U.Array Int          -- ^ segment starting indices
                -> U.Array Int          -- ^ segment lengths
                -> PData a

  -- | Append two sized arrays.
  appendPR      :: PData a -> PData a -> PData a

  -- | Segmented append
  appendsPR     :: U.Segd               -- ^ segd of result
                -> U.Segd -> PData a    -- ^ segd/data of first  arrays
                -> U.Segd -> PData a    -- ^ segd/data of second arrays
                -> PData a

  -- | Filter an array based on some tags.
  packByTagPR   :: PData a              -- ^ source array
                -> U.Array Tag          -- ^ array of tags
                -> Tag                  -- ^ tag of elements to select
                -> PData a

  -- | Combine two arrays based on a selector.
  combine2PR    :: U.Sel2               -- ^ selector
                -> PData a              -- ^ first source array
                -> PData a              -- ^ second source array
                -> PData a

  -- Conversions ---------------------
  -- | Convert a list to an array
  fromVectorPR  :: Vector a -> PData a

  -- | Convert an array to a list
  toVectorPR    :: PData a -> Vector a

  -- TODO: Shift these into the Scalar class like existing library.  
  -- | Convert an unlifted array to a PData. 
  fromUArrayPR  :: U.Elt a => U.Array a -> PData a
  
  -- | Convert a PData into an unlifted array.
  toUArrayPR    :: U.Elt a => PData a -> U.Array a




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

