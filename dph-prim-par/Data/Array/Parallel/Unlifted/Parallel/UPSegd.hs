{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel segment descriptors.
module Data.Array.Parallel.Unlifted.Parallel.UPSegd (
  -- * Types
  UPSegd, valid,

  -- * Constructors
  mkUPSegd, empty, singleton,
  fromLengths,
  
  -- * Projections
  length,
  takeLengths,
  takeIndices,
  takeElements,
  takeUSegd,
  takeDistributed,
  
  -- * Replicate
  replicateWith,
    
  -- * Segmented Folds
  foldSegsWith,
  foldWith,
  fold1With,
  sumWith,
  indicesWith
) where
import Data.Array.Parallel.Unlifted.Distributed

import Data.Array.Parallel.Unlifted.Sequential.USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Basics         as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Combinators    as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector         as Seq
import Data.Array.Parallel.Unlifted.Sequential.Vector                   (Vector, MVector, Unbox)

import Control.Monad.ST
import Prelude  hiding (length)


-- | A Parallel segment descriptor holds the original descriptor,
--   and a distributed one that describes how to distribute the work
--   on such a segmented array.
data UPSegd 
        = UPSegd 
        { upsegd_usegd :: !USegd
          -- ^ Segment descriptor that describes the whole array.

        , upsegd_dsegd :: Dist ((USegd,Int),Int)
          -- ^ Segment descriptor for each chunk, 
          --   along with segment id of first slice in the chunk,
          --   and the offset of that slice in its segment.
          --   See docs of `splitSegdOfElemsD` for an example.
        }


-- | O(1).
--   Check the internal consistency of a scattered segment descriptor.
--   TODO: doesn't do any checks yet
valid :: UPSegd -> Bool
{-# INLINE valid #-}
valid _ = True


-- Constructors ---------------------------------------------------------------
-- | O(1). Construct a new segment descriptor.
mkUPSegd 
        :: Vector Int   -- ^ length of each segment
        -> Vector Int   -- ^ starting index of each segment
        -> Int          -- ^ total number of elements in the flat array
        -> UPSegd

{-# INLINE mkUPSegd #-}
mkUPSegd lens idxs n
        = toUPSegd (mkUSegd lens idxs n)


-- | O(1).
--  Convert a global `USegd` to a distributed `UPSegd` by splitting
--  it across the gang.
toUPSegd :: USegd -> UPSegd
{-# INLINE toUPSegd #-}
toUPSegd segd   = UPSegd segd (splitSegdOnElemsD theGang segd)


-- | O(1). Yield an empty segment descriptor, with no elements or segments.
empty :: UPSegd
{-# INLINE empty #-}
empty           = toUPSegd emptyUSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements.
singleton :: Int -> UPSegd
{-# INLINE singleton #-}
singleton n     = toUPSegd $ singletonUSegd n


-- | O(n). Convert a length array into a segment descriptor.
-- 
--   The array contains the length of each segment, and we compute the 
--   indices from that. Runtime is O(n) in the number of segments.
--
fromLengths :: Vector Int -> UPSegd
{-# INLINE fromLengths #-}
fromLengths     = toUPSegd . lengthsToUSegd


-- Projections ----------------------------------------------------------------
-- | O(1). Yield the overall number of segments.
length :: UPSegd -> Int
{-# INLINE length #-}
length          = lengthUSegd . upsegd_usegd


-- | O(1). Yield the lengths of the individual segments.
takeLengths :: UPSegd -> Vector Int
{-# INLINE takeLengths #-}
takeLengths     = lengthsUSegd . upsegd_usegd


-- | O(1). Yield the segment indices of a segment descriptor.
takeIndices :: UPSegd -> Vector Int
{-# INLINE takeIndices #-}
takeIndices     = indicesUSegd . upsegd_usegd


-- | O(1). Yield the number of data elements.
takeElements :: UPSegd -> Int
{-# INLINE takeElements #-}
takeElements    = elementsUSegd . upsegd_usegd


-- | O(1). Yield the global `USegd` of a `UPSegd`
takeUSegd :: UPSegd -> USegd
{-# INLINE takeUSegd #-}
takeUSegd       = upsegd_usegd


-- | O(1). Yield the distributed `USegd` of a `UPSegd`
takeDistributed :: UPSegd -> Dist ((USegd,Int),Int)
{-# INLINE takeDistributed #-}
takeDistributed = upsegd_dsegd


-- Indices --------------------------------------------------------------------
indicesWith :: UPSegd -> Vector Int
{-# INLINE_UP indicesWith #-}
indicesWith
        = joinD theGang balanced
        . mapD theGang indices
        . takeDistributed
  where
    indices ((segd,k),off) = Seq.indicesSU' off segd


-- Replicate ------------------------------------------------------------------
-- | Segmented replication.
replicateWith :: Unbox a => UPSegd -> Vector a -> Vector a
{-# INLINE_UP replicateWith #-}
replicateWith segd !xs 
  = joinD theGang balanced
  . mapD theGang rep
  $ takeDistributed segd
  where
    rep ((dsegd,di),_)
      = Seq.replicateSU dsegd (Seq.slice xs di (lengthUSegd dsegd))


-- Fold -----------------------------------------------------------------------
-- | Fold segments specified by a UPSegd.
foldWith :: Unbox a
         => (a -> a -> a) -> a -> UPSegd -> Vector a -> Vector a
{-# INLINE foldWith #-}
foldWith f !z
        = foldSegsWith f (Seq.foldlSU f z)


-- | Fold segments specified by a UPSegd, with a non-empty vector.
fold1With :: Unbox a
         => (a -> a -> a) -> UPSegd -> Vector a -> Vector a
{-# INLINE fold1With #-}
fold1With f
        = foldSegsWith f (Seq.fold1SU f)


-- | Sum up segments specified by a UPSegd.
sumWith :: (Num e, Unbox e) => UPSegd -> Vector e -> Vector e
{-# INLINE sumWith #-}
sumWith = foldWith (+) 0


-- | Fold the segments specified by a UPSegd.
--
--   Low level function takes a per-element worker and a per-segment worker.
--   It folds all the segments with the per-segment worker, then uses the
--   per-element worker to fixup the partial results when a segment 
--   is split across multiple threads.
--   
foldSegsWith
        :: Unbox a
        => (a -> a -> a)
        -> (USegd -> Vector a -> Vector a)
        -> UPSegd -> Vector a -> Vector a

{-# INLINE foldSegsWith #-}
foldSegsWith fElem fSeg segd xs 
 = dcarry `seq` drs `seq` 
   runST (do
        mrs <- joinDM theGang drs
        fixupFold fElem mrs dcarry
        Seq.unsafeFreeze mrs)

 where  (dcarry,drs)
          = unzipD
          $ mapD theGang partial
          $ zipD (takeDistributed segd)
                 (splitD theGang balanced xs)

        partial (((segd, k), off), as)
         = let rs = fSeg segd as
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
