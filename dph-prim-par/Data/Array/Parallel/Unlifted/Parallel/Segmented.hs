{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Parallel combinators for segmented unboxed arrays
module Data.Array.Parallel.Unlifted.Parallel.Segmented 
        ( replicateRSUP
        , appendSUP
        , foldRUP
        , sumRUP

        -- * Scattered indexing.
        , unsafeIndexsFromVectorsWithUPVSegd

        -- * Scattered extracts.
        , unsafeExtractsFromNestedWithUPSSegd
        , unsafeExtractsFromVectorsWithUPSSegd
        , unsafeExtractsFromVectorsWithUPVSegd)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.UPSegd                     (UPSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPSSegd                    (UPSSegd)
import Data.Array.Parallel.Unlifted.Parallel.UPVSegd                    (UPVSegd)
import Data.Array.Parallel.Unlifted.Sequential.USegd                    (USegd)
import Data.Array.Parallel.Unlifted.Sequential.USSegd                   (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector                   as Seq
import Data.Array.Parallel.Unlifted.Vectors                             (Vectors)
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd           as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd          as UPSSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPVSegd          as UPVSegd
import qualified Data.Array.Parallel.Unlifted.Vectors                   as US
import qualified Data.Array.Parallel.Unlifted.Stream                    as US
import qualified Data.Array.Parallel.Unlifted.Sequential                as Seq
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd          as USegd
import qualified Data.Array.Parallel.Unlifted.Sequential.USSegd         as USSegd

import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Stream.Size    ( Size(..) )
import qualified Data.Vector.Fusion.Stream                              as S
import qualified Data.Vector                                            as V

-- replicate ------------------------------------------------------------------

-- | Segmented replication.
--   Each element in the vector is replicated the given number of times.
--   
--   @replicateRSUP 2 [1, 2, 3, 4, 5] = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]@
--

--   TODO: make this efficient
replicateRSUP :: Unbox a => Int -> Vector a -> Vector a
{-# INLINE_UP replicateRSUP #-}
replicateRSUP n xs
        = UPSegd.replicateWithP (UPSegd.fromLengths (replicateUP (Seq.length xs) n)) xs


-- Append ---------------------------------------------------------------------
-- | Segmented append.
appendSUP
        :: Unbox a
        => UPSegd
        -> UPSegd -> Vector a
        -> UPSegd -> Vector a
        -> Vector a

{-# INLINE_UP appendSUP #-}
appendSUP segd !xd !xs !yd !ys
  = joinD theGang balanced
  . mapD  theGang append
  $ UPSegd.takeDistributed segd
  where append ((segd,seg_off),el_off)
         = Seq.unstream
         $ appendSegS (UPSegd.takeUSegd xd) xs
                      (UPSegd.takeUSegd yd) ys
                      (USegd.takeElements segd)
                      seg_off el_off

-- append ---------------------------------------------------------------------
appendSegS
        :: Unbox a      
        => USegd        -- ^ Segment descriptor of first array.
        -> Vector a     -- ^ Data of first array
        -> USegd        -- ^ Segment descriptor of second array.
        -> Vector a     -- ^ Data of second array.
        -> Int
        -> Int
        -> Int
        -> S.Stream a

{-# INLINE_STREAM appendSegS #-}
appendSegS !xd !xs !yd !ys !n seg_off el_off
  = Stream next state (Exact n)
  where
    !xlens = USegd.takeLengths xd
    !ylens = USegd.takeLengths yd

    state
      | n == 0 = Nothing
      | el_off < xlens ! seg_off
      = let i = (USegd.takeIndices xd ! seg_off) + el_off
            j =  USegd.takeIndices yd ! seg_off
            k = (USegd.takeLengths xd ! seg_off) - el_off
        in  Just (False, seg_off, i, j, k, n)

      | otherwise
      = let -- NOTE: *not* indicesUSegd xd ! (seg_off+1) since seg_off+1
            -- might be out of bounds
            i       = (USegd.takeIndices xd ! seg_off) + (USegd.takeLengths xd ! seg_off)
            el_off' = el_off - USegd.takeLengths xd ! seg_off
            j       = (USegd.takeIndices yd ! seg_off) + el_off'
            k       = (USegd.takeLengths yd ! seg_off) - el_off'
        in  Just (True, seg_off, i, j, k, n)

    {-# INLINE next #-}
    next Nothing = return Done

    next (Just (False, seg, i, j, k, n))
      | n == 0    = return Done
      | k == 0    = return $ Skip (Just (True, seg, i, j, ylens ! seg, n))
      | otherwise = return $ Yield (xs!i) (Just (False, seg, i+1, j, k-1, n-1))

    next (Just (True, seg, i, j, k, n))
      | n == 0    = return Done
      | k == 0
      = let !seg' = seg+1
        in  return $ Skip (Just (False, seg', i, j, xlens ! seg', n))

      | otherwise = return $ Yield (ys!j) (Just (True, seg, i, j+1, k-1, n-1))


-- foldR ----------------------------------------------------------------------
-- | Regular segmented fold.
foldRUP :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Int -> Vector a -> Vector b
{-# INLINE_UP foldRUP #-}
foldRUP f z !segSize xs = 
   joinD theGang unbalanced
    (mapD theGang 
      (Seq.foldlRU f z segSize)
      (splitAsD theGang (mapD theGang (*segSize) dlen) xs))
  where
    noOfSegs = Seq.length xs `div` segSize
    dlen = splitLenD theGang noOfSegs


-- sumR -----------------------------------------------------------------------
-- | Regular segmented sum.
sumRUP :: (Num e, Unbox e) => Int -> Vector e -> Vector e
{-# INLINE_UP sumRUP #-}
sumRUP = foldRUP (+) 0


-- Indexvs --------------------------------------------------------------------
-- | Lookup elements from some `Vectors` through a `UPVSegd`.
--
--   TODO: make this parallel.
--
unsafeIndexsFromVectorsWithUPVSegd 
        :: (Unbox a, US.Unboxes a)
        => Vectors a -> UPVSegd -> Vector (Int, Int) -> Vector a

unsafeIndexsFromVectorsWithUPVSegd vectors upvsegd vsrcixs
 = let  -- Because we're just doing indexing here, we don't need the culled
        -- vsegids or ussegd, and can just use the redundant version.
        !vsegids  = UPVSegd.takeVSegidsRedundant upvsegd
        !upssegd  = UPVSegd.takeUPSSegdRedundant upvsegd
        !ussegd   = UPSSegd.takeUSSegd upssegd
   in   Seq.unstream
         $ US.unsafeStreamElemsFromVectors        vectors
         $ US.unsafeStreamSrcIxsThroughUSSegd  ussegd
         $ US.unsafeStreamSrcIxsThroughVSegids vsegids
         $ Seq.stream vsrcixs
{-# INLINE_U unsafeIndexsFromVectorsWithUPVSegd #-}


-- Extracts -------------------------------------------------------------------
-- | Copy segments from a nested vectors and concatenate them into a new array.
unsafeExtractsFromNestedWithUPSSegd
        :: Unbox a
        => UPSSegd -> V.Vector (Vector a) -> Vector a

unsafeExtractsFromNestedWithUPSSegd upssegd vectors
        = Seq.unstream 
        $ US.unsafeStreamSegsFromNestedUSSegd
                vectors
                (UPSSegd.takeUSSegd upssegd)
{-# INLINE_U unsafeExtractsFromNestedWithUPSSegd #-}


-- | TODO: make this parallel.
unsafeExtractsFromVectorsWithUPSSegd
        :: (Unbox a, US.Unboxes a)
        => UPSSegd
        -> Vectors a
        -> Vector a

unsafeExtractsFromVectorsWithUPSSegd upssegd vectors
        = Seq.unsafeExtractsFromVectorsUSSegd
                (UPSSegd.takeUSSegd upssegd) 
                vectors
{-# INLINE_UP unsafeExtractsFromVectorsWithUPSSegd #-}


-- | TODO: make this parallel.
unsafeExtractsFromVectorsWithUPVSegd
        :: (Unbox a, US.Unboxes a)
        => UPVSegd
        -> Vectors a
        -> Vector a

unsafeExtractsFromVectorsWithUPVSegd upvsegd vectors
        = Seq.unstream 
        $ US.unsafeStreamSegsFromVectorsUSSegd
                vectors         -- TODD: avoid demote, use the vsegd directly.
                (UPSSegd.takeUSSegd $ UPVSegd.demoteToUPSSegd upvsegd)
{-# INLINE_UP unsafeExtractsFromVectorsWithUPVSegd #-}

