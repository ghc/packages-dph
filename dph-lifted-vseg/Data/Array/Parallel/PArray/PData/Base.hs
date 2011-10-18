{-# LANGUAGE UndecidableInstances #-}
-- Undeciable instances only need for derived Show instance

#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Base 
        ( -- * Parallel Array types.
          PArray(..)
        , length, unpack
        , PprPhysical (..), PprVirtual (..)
        , PData
        , PR(..)
        
        , uextracts)
where
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Pretty
import GHC.Exts
import SpecConstr
import Prelude hiding (length)

-- PArray ---------------------------------------------------------------------
-- | A parallel array. 
--   PArrays always contain a finite (sized) number of elements, which means
--   they have a length.
--
--   IMPORTANT: 
--    The vectoriser requires the data constructor to have this specific form,
--    because it builds them explicitly.
--    In particular, the array length must be unboxed.
--
--   TODO: Why do we need the NoSpecConstr annotation?
-- 
{-# ANN type PArray NoSpecConstr #-}
data PArray a
        = PArray Int# (PData  a)

deriving instance (Show (PData a), Show a)
        => Show (PArray a)

instance (PprPhysical (PData a)) => PprPhysical (PArray a) where
 pprp (PArray n# dat)
  =   (text "PArray " <+> int (I# n#))
  $+$ (nest 4 
      $ pprp dat)

instance PprVirtual (PData a) => PprVirtual (PArray a) where
 pprv (PArray _ dat)
  =   pprv dat


-- | Take the length of an array
{-# INLINE_PA length #-}
length :: PArray a -> Int
length (PArray n# _)   = (I# n#)


-- | Take the data from an array.
{-# INLINE_PA unpack #-}
unpack :: PArray a -> PData a
unpack (PArray _ d)   = d


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

  -- | O(sum lengths). Segmented replicate.
  --   NOTE: This takes a whole Segd instead of just the lengths, because we can
  --   do it more efficiently if we know there are no zero lengths.
  --   TODO: the Segd should actually keep track of whether there are zero lengths.
  replicatesPR  :: U.Segd               -- ^ segment descriptor defining the lengths of the segments.
                -> PData a              -- ^ data elements to replicate
                -> PData a

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
                -> U.SSegd              -- ^ segment descriptor describing scattering of data.
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
  -- | Convert a boxed vector to an array.
  fromVectorPR  :: Vector a -> PData a

  -- | Convert an array to a boxed vector.
  toVectorPR    :: PData a -> Vector a


-------------------------------------------------------------------------------
-- extra unlifted primitives should be moved into unlifted library ------------
-------------------------------------------------------------------------------

-- TODO: zip srcids ixBase and startsix before calling replicate_s
--       don't want to replicate_s multiple times on same segd.
--
-- TODO: pass in a projection function to get the correct array from the vector, 
--       to avoid unpackig all the arrays from PDatas with a big map traversal.

{-# NOINLINE uextracts #-}
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

        {-# INLINE get #-}
        get ixDst ixSegDst (ixSegSrcBase, srcid)
         = let  !arr    = arrs V.! srcid                         -- TODO: use unsafeIndex
                !ix     = ixDst - ixSegDst + ixSegSrcBase
           in   arr U.!: ix                         -- TODO unsafe unsafeIndex
         
        result    = U.zipWith3 get
                        (U.enumFromTo 0 (dstLen - 1))
                        startixs'
                        (U.zip baseixs srcids')

   in result
