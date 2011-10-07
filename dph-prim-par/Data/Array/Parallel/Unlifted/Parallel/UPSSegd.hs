{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel scattered segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPSSegd (
  -- * Types
  UPSSegd, valid,

  -- * Constructors
  mkUPSSegd, fromUSSegd, fromUPSegd,
  empty, singleton,
  
  -- * Projections
  length,
  takeUSSegd,
  takeDistributed,
  takeLengths,
  takeIndices,
  takeElements,
  takeStarts,
  takeSources,
  getSeg,
  
  -- * Append
  appendWith,
  
  -- * Segmented Folds
  foldWithP,
  fold1WithP,
  foldSegsWithP
) where
import Data.Array.Parallel.Pretty                                       hiding (empty)
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.UPSegd                     (UPSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.USSegd                   (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector, MVector, Unbox)

import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.USSegd        as DUSSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd         as USSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector         as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Combinators    as Seq

import qualified Data.Vector                                            as V
import Control.Monad.ST
import Prelude hiding (length)


-- | A parallel scattered segment descriptor is an extension of `UPSegd` 
--   that allows the segments to be scattered through multiple flat arrays.
--
--   Each segment is associated with a source id that indicates what 
--   flat array it is in, along with the starting index in that flat array.
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
        deriving Show

instance PprPhysical UPSSegd where
 pprp (UPSSegd ussegd dssegd)
  =  text "UPSSegd"
  $$ (nest 7 $ vcat
        [ text "ussegd:  " <+> pprp ussegd
        , text "dssegd:  " <+> pprp dssegd])


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
-- 
--   * TODO: this doesn't do any checks yet
valid :: UPSSegd -> Bool
{-# INLINE_UP valid #-}
valid _ = True


-- Constructors ---------------------------------------------------------------
-- | Construct a new segment descriptor.
mkUPSSegd 
        :: Vector Int   -- ^ Starting index of each segment in its flat array.
        -> Vector Int   -- ^ Source id of the flat array to tach each segment from.
        -> UPSegd       -- ^ Contiguous (unscattered) segment descriptor.
        -> UPSSegd

{-# INLINE_UP mkUPSSegd #-}
mkUPSSegd starts sources upsegd
        = fromUSSegd (USSegd.mkUSSegd starts sources (UPSegd.takeUSegd upsegd))


-- | Promote a global `USSegd` to a parallel `UPSSegd` by distributing
--   it across the gang.
fromUSSegd :: USSegd -> UPSSegd
{-# INLINE_UP fromUSSegd #-}
fromUSSegd ssegd 
        = UPSSegd ssegd (DUSSegd.splitSSegdOnElemsD theGang ssegd)


-- | Promote a plain `UPSegd` to a `UPSSegd`, by assuming that all segments
--   come from a single flat array with source id 0.
--
--   * TODO:
--     This sequentially constructs the indices and source fields, and we
--     throw out the existing distributed `USegd`. We could probably keep
--     some of the existing fields and save reconstructing them.
--
fromUPSegd :: UPSegd -> UPSSegd
{-# INLINE_UP fromUPSegd #-}
fromUPSegd upsegd
        = fromUSSegd $ USSegd.fromUSegd $ UPSegd.takeUSegd upsegd


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: UPSSegd
{-# INLINE_UP empty #-}
empty   = fromUSSegd USSegd.empty


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> UPSSegd
{-# INLINE_UP singleton #-}
singleton n = fromUSSegd $ USSegd.singleton n


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
length :: UPSSegd -> Int
{-# INLINE_UP length #-}
length          = USSegd.length . upssegd_ussegd


-- | O(1). Yield the global `USegd` of a `UPSegd`
takeUSSegd :: UPSSegd -> USSegd
{-# INLINE_UP takeUSSegd #-}
takeUSSegd      = upssegd_ussegd


-- | O(1). Yield the distributed `USegd` of a `UPSegd`
takeDistributed :: UPSSegd -> Dist ((USSegd, Int), Int)
{-# INLINE_UP takeDistributed #-}
takeDistributed = upssegd_dssegd


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: UPSSegd -> Vector Int
{-# INLINE_UP takeLengths #-}
takeLengths     = USSegd.takeLengths . upssegd_ussegd


-- | O(1). Yield the segment indices.
takeIndices :: UPSSegd -> Vector Int
{-# INLINE_UP takeIndices #-}
takeIndices     = USSegd.takeIndices . upssegd_ussegd


-- | O(1). Yield the total number of data elements.
--
--  @takeElements upssegd = sum (takeLengths upssegd)@
--
takeElements :: UPSSegd -> Int
{-# INLINE_UP takeElements #-}
takeElements    = USSegd.takeElements . upssegd_ussegd


-- | O(1). Yield the starting indices.
takeStarts :: UPSSegd -> Vector Int
{-# INLINE_UP takeStarts #-}
takeStarts      = USSegd.takeStarts . upssegd_ussegd


-- | O(1). Yield the source ids.
takeSources :: UPSSegd -> Vector Int
{-# INLINE_UP takeSources #-}
takeSources     = USSegd.takeSources . upssegd_ussegd 


-- | O(1).
--   Get the length, segment index, starting index, and source id of a segment.
getSeg :: UPSSegd -> Int -> (Int, Int, Int, Int)
{-# INLINE_UP getSeg #-}
getSeg upssegd ix
        = USSegd.getSeg (upssegd_ussegd upssegd) ix


-- Append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor that describes the result of appending two
--   segmented arrays.
--   
--   * TODO: This calls out to the sequential version.
--
--   * Appending two nested arrays is an index space transformation. Because
--     a `UPSSegd` can contain segments from multiple flat data arrays, we can
--     represent the result of the append without copying elements from the
--     underlying flat data arrays.
--
appendWith
        :: UPSSegd              -- ^ Segment descriptor of first nested array.
        -> Int                  -- ^ Number of flat data arrays used to represent first nested array.
        -> UPSSegd              -- ^ Segment descriptor of second nested array. 
        -> Int                  -- ^ Number of flat data arrays used to represent second nested array.
        -> UPSSegd
{-# INLINE_UP appendWith #-}
appendWith upssegd1 pdatas1
           upssegd2 pdatas2
 = fromUSSegd 
 $ USSegd.append (upssegd_ussegd upssegd1) pdatas1
                 (upssegd_ussegd upssegd2) pdatas2


-- Fold -----------------------------------------------------------------------
-- | Fold segments specified by a `UPSSegd`.
foldWithP :: Unbox a
         => (a -> a -> a) -> a -> UPSSegd -> V.Vector (Vector a) -> Vector a
{-# INLINE_UP foldWithP #-}
foldWithP f !z
        = foldSegsWithP f (Seq.foldlSSU f z)


-- | Fold segments specified by a `UPSSegd`, with a non-empty vector.
fold1WithP :: Unbox a
         => (a -> a -> a) -> UPSSegd -> V.Vector (Vector a) -> Vector a
{-# INLINE_UP fold1WithP #-}
fold1WithP f
        = foldSegsWithP f (Seq.fold1SSU f)


-- | Sum up segments specified by a `UPSSegd`.
sumWithP :: (Num a, Unbox a)
        => UPSSegd -> V.Vector (Vector a) -> Vector a
{-# INLINE_UP sumWithP #-}
sumWithP = foldWithP (+) 0


-- | Fold the segments specified by a `UPSSegd`.
--
--   Low level function takes a per-element worker and a per-segment worker.
--   It folds all the segments with the per-segment worker, then uses the
--   per-element worker to fixup the partial results when a segment 
--   is split across multiple threads.
--   
foldSegsWithP
        :: Unbox a
        => (a -> a -> a)
        -> (USSegd -> V.Vector (Vector a) -> Vector a)
        -> UPSSegd -> V.Vector (Vector a) -> Vector a

{-# INLINE_UP foldSegsWithP #-}
foldSegsWithP fElem fSeg segd xss 
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
