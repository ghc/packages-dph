{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPSSegd (
  -- * Types
  UPSSegd, valid,

  -- * Constructors
  mkUPSSegd, empty, singleton,
  fromUPSegd,
  
  -- * Projections
  length,
  takeLengths,
  takeIndices,
  takeElements,
  takeStarts,
  takeSources,
  takeUSSegd,
  takeDistributed,
  getSeg,
  
  -- * Append
  appendWith,
  
  -- * Segmented Folds
  foldWith,
  fold1With,
  foldSegsWith
) where
import Data.Array.Parallel.Pretty                               hiding (empty)
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Sequential.USegd
import Data.Array.Parallel.Unlifted.Sequential.USSegd
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector)
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector, MVector, Unbox)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd             (UPSegd)

import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector         as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Combinators    as Seq

import qualified Data.Vector                                            as V
import Control.Monad.ST
import Prelude hiding (length)


-- | A Parallel segment descriptor holds the original descriptor,
--   and a distributed one that describes how to distribute the work
--   on such a segmented array.
data UPSSegd 
        = UPSSegd 
        { upssegd_ussegd :: !USSegd
          -- ^ Segment descriptor that describes the whole array.

        , upssegd_dssegd :: Dist ((USSegd,Int),Int)
          -- ^ Segment descriptor for each chunk, 
          --   along with segment id of first slice in the chunk,
          --   and the offset of that slice in its segment.
          --   See docs of `splitSegdOfElemsD` for an example.
        }


instance PprPhysical UPSSegd where
 pprp (UPSSegd ussegd dssegd)
  =  text "UPSSegd"
  $$ (nest 7 $ vcat
        [ text "ussegd:  " <+> pprp ussegd
        , text "dssegd:  " <+> pprp dssegd])


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
--   TODO: doesn't do any checks yet
valid :: UPSSegd -> Bool
{-# INLINE valid #-}
valid _ = True


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUPSSegd 
        :: Vector Int   -- ^ starting index of each segment in its flat array
        -> Vector Int   -- ^ which array to take each segment from
        -> USegd        -- ^ contiguous segment descriptor
        -> UPSSegd

{-# INLINE mkUPSSegd #-}
mkUPSSegd starts sources usegd
        = toUPSSegd (mkUSSegd starts sources usegd)


-- | O(1).
--  Convert a global `USegd` to a distributed `UPSegd` by splitting
--  it across the gang.
toUPSSegd :: USSegd -> UPSSegd
{-# INLINE toUPSSegd #-}
toUPSSegd ssegd 
        = UPSSegd ssegd (splitSSegdOnElemsD theGang ssegd)


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: UPSSegd
{-# INLINE empty #-}
empty   = toUPSSegd emptyUSSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> UPSSegd
{-# INLINE singleton #-}
singleton n  = toUPSSegd $ singletonUSSegd n


-- | O(segs). 
--   Promote a plain USSegd to a UPSSegd
--   All segments are assumed to come from a flat array with sourceid 0.

--   TODO: Sequential construction of the indices and source field.
--         We throw out the existing distributed usegd here,
--          maybe we can do the promotion while keeping some of the existing fields.
fromUPSegd :: UPSegd -> UPSSegd
{-# INLINE fromUPSegd #-}
fromUPSegd upsegd
 = toUPSSegd $ promoteUSegdToUSSegd $ UPSegd.takeUSegd upsegd


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
length :: UPSSegd -> Int
{-# INLINE length #-}
length = lengthUSSegd . upssegd_ussegd


-- | O(1). Yield the global `USegd` of a `UPSegd`
takeUSSegd :: UPSSegd -> USSegd
{-# INLINE takeUSSegd #-}
takeUSSegd = upssegd_ussegd


-- | O(1). Yield the distributed `USegd` of a `UPSegd`
takeDistributed :: UPSSegd -> Dist ((USSegd, Int), Int)
{-# INLINE takeDistributed #-}
takeDistributed = upssegd_dssegd


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: UPSSegd -> Vector Int
{-# INLINE takeLengths #-}
takeLengths = lengthsUSSegd . upssegd_ussegd


-- | O(1). Yield the segment indices of a segment descriptor.
takeIndices :: UPSSegd -> Vector Int
{-# INLINE takeIndices #-}
takeIndices = indicesUSSegd . upssegd_ussegd


-- | O(1). Yield the number of data elements.
takeElements :: UPSSegd -> Int
{-# INLINE takeElements #-}
takeElements = elementsUSSegd . upssegd_ussegd


-- | O(1). Yield the starting indices of a `UPSSegd`
takeStarts :: UPSSegd -> Vector Int
{-# INLINE takeStarts #-}
takeStarts = startsUSSegd . upssegd_ussegd


-- | O(1). Yield the source ids of a `UPSSegd`
takeSources :: UPSSegd -> Vector Int
{-# INLINE takeSources #-}
takeSources = sourcesUSSegd . upssegd_ussegd 


-- | O(1).
--   Get the length, segment index, starting index, and source id of a segment.
getSeg :: UPSSegd -> Int -> (Int, Int, Int, Int)
{-# INLINE getSeg #-}
getSeg upssegd ix
        = getSegOfUSSegd (upssegd_ussegd upssegd) ix


-- Append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor that describes the result of appending.
--   
--   TODO: This calls out to the sequential version.
--
appendWith
        :: UPSSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> UPSSegd -> Int        -- ^ ussegd of array, and number of physical data arrays
        -> UPSSegd
{-# INLINE appendWith #-}
appendWith
        upssegd1 pdatas1
        upssegd2 pdatas2
 = toUPSSegd 
 $ appendUSSegd (upssegd_ussegd upssegd1) pdatas1
                (upssegd_ussegd upssegd2) pdatas2


-- Fold -----------------------------------------------------------------------
-- | Fold segments specified by a UPSegd.
foldWith :: Unbox a
         => (a -> a -> a) -> a -> UPSSegd -> V.Vector (Vector a) -> Vector a
{-# INLINE foldWith #-}
foldWith f !z
        = foldSegsWith f (Seq.foldlSSU f z)


-- | Fold segments specified by a UPSegd, with a non-empty vector.
fold1With :: Unbox a
         => (a -> a -> a) -> UPSSegd -> V.Vector (Vector a) -> Vector a
{-# INLINE fold1With #-}
fold1With f
        = foldSegsWith f (Seq.fold1SSU f)


-- | Sum up segments specified by a UPSegd.
sumWith :: (Num a, Unbox a)
        => UPSSegd -> V.Vector (Vector a) -> Vector a
{-# INLINE sumWith #-}
sumWith = foldWith (+) 0


-- | Fold the segments specified by a UPSSegd
--
--   Low level function takes a per-element worker and a per-segment worker.
--   It folds all the segments with the per-segment worker, then uses the
--   per-element worker to fixup the partial results when a segment 
--   is split across multiple threads.
--   
foldSegsWith
        :: Unbox a
        => (a -> a -> a)
        -> (USSegd -> V.Vector (Vector a) -> Vector a)
        -> UPSSegd -> V.Vector (Vector a) -> Vector a

{-# INLINE foldSegsWith #-}
foldSegsWith fElem fSeg segd xss 
 = dcarry `seq` drs `seq` 
   runST (do
        mrs <- joinDM theGang drs
        fixupFold fElem mrs dcarry
        Seq.unsafeFreeze mrs)

 where  (dcarry,drs)
          = unzipD
          $ mapD theGang partial (takeDistributed segd)

        partial ((ssegd, k), off)
         = let rs = fSeg ssegd xss
               {-# INLINE [0] n #-}
               n | off == 0  = 0
                 | otherwise = 1

           in  ((k, Seq.take n rs), Seq.drop n rs)


fixupFold
        :: Unbox a
        => (a -> a -> a)
        -> MVector s a
        -> Dist (Int,Vector a)
        -> ST s ()

{-# NOINLINE fixupFold #-}
fixupFold f !mrs !dcarry = go 1
  where
    !p = gangSize theGang

    go i | i >= p = return ()
         | Seq.null c = go (i+1)
         | otherwise   = do
                           x <- Seq.read mrs k
                           Seq.write mrs k (f x (c Seq.! 0))
                           go (i + 1)
      where
        (k,c) = indexD dcarry i
