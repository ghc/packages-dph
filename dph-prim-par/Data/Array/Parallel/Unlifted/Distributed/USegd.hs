{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Operations on Distributed Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.USegd (
        splitSegdOnSegsD,
        splitSegdOnElemsD,
        splitSD,
        joinSegdD
)
where
import Data.Array.Parallel.Unlifted.Distributed.Arrays
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Unlifted.Sequential.Vector           (Vector, Unbox, (!))
import Data.Array.Parallel.Base
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as Seq
import Data.Bits     ( shiftR )
import Control.Monad ( when )


-------------------------------------------------------------------------------
-- | Split a segment descriptor across the gang, segment wise.
--   Whole segments are placed on each thread, and we try to balance out
--   the segments so each thread has the same number of array elements.
--
--   We don't split segments across threads, as this would limit our ability
--   to perform intra-thread fusion of lifted operations. The down side
--   of this is that if we have few segments with an un-even size distribution
--   then large segments can cause the gang to become unbalanced.
--
--   In the following example the segment with size 100 dominates and
--   unbalances the gang. There is no reason to put any segments on the
--   the last thread because we need to wait for the first to finish anyway.
--
--   @ > pprp $ splitSegdOnSegsD theGang
--            $ lengthsToUSegd $ fromList [100, 10, 20, 40, 50  :: Int]
-- 
--     DUSegd lengths:   DVector lengths:  [ 1,    3,         1,  0]
--                                chunks:  [[100],[10,20,40],[50],[]]
-- 
--            indices:   DVector lengths:  [1,3,1,0]
--                                chunks:  [[0],  [0,10,30], [0], []]
--
--            elements:  DInt [100,70,50,0]
--   @
--
--  NOTE: This splitSegdOnSegsD function isn't currently used.
--
splitSegdOnSegsD :: Gang -> USegd -> Dist USegd
{-# NOINLINE splitSegdOnSegsD #-}
splitSegdOnSegsD g !segd 
  = mapD g lengthsToUSegd
  $ splitAsD g d lens
  where
    !d   = snd
         . mapAccumLD g chunks 0
         . splitLenD g
         $ elementsUSegd segd

    n    = lengthUSegd segd
    lens = lengthsUSegd segd

    chunks !i !k 
      = let !j = go i k
        in  (j,j-i)

    go !i !k | i >= n    = i
             | m == 0    = go (i+1) k
             | k <= 0    = i
             | otherwise = go (i+1) (k-m)
      where
        m = lens ! i


-------------------------------------------------------------------------------
-- | Split a segment descriptor across the gang, element wise.
--   We try to put the same number of elements on each thread, which means
--   that segments are sometimes split across threads.
--
--   Each thread gets a slice of segment descriptor, the segid of the first 
--   slice, and the offset of the first slice in its segment.
--   
--   Example:
--    In this picture each X represents 5 elements, and we have 5 segements in total.
--
-- @
--    segs:    ----------------------- --- ------- --------------- -------------------
--    elems:  |X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|X X X X X X X X X|
--            |     thread1     |     thread2     |     thread3     |     thread4     |
--    segid:  0                 0                 3                 4
--    offset: 0                 45                0                 5
--
--   > pprp $ splitSegdOnElemsD theGang 
--          $ lengthsToUSegd $ fromList [60, 10, 20, 40, 50 :: Int]
--
--     segd:    DUSegd lengths:  DVector lengths: [1,3,2,1]
--                                        chunks:  [[45],[15,10,20],[40,5],[45]]
--                     indices:  DVector lengths: [1,3,2,1]
--                                        chunks:  [[0], [0,15,25], [0,40],[0]]
--                    elements:  DInt [45,45,45,45]
--
--     segids: DInt [0,0,3,4]     (segment id of first slice on thread)
--    offsets: DInt [0,45,0,5]    (offset of that slice in its segment)
-- @
--
splitSegdOnElemsD :: Gang -> USegd -> Dist ((USegd,Int),Int)
{-# INLINE splitSegdOnElemsD #-}
splitSegdOnElemsD g !segd 
  = imapD g mk (splitLenIdxD g (elementsUSegd segd))
  where 
        -- Number of threads in gang.
        !nThreads = gangSize g

        -- Determine what elements go on a thread
        mk :: Int                  -- Thread index.
           -> (Int, Int)           -- Number of elements on this thread,
                                   --   and starting offset into the flat array.
           -> ((USegd, Int), Int)  -- Segd for this thread, segid of first slice,
                                   --   and offset of first slice.

        mk i (nElems, ixStart) 
         = case chunk segd ixStart nElems (i == nThreads - 1) of
            (# lens, l, o #) -> ((lengthsToUSegd lens, l), o)


-- | Determine what elements go on a thread.
--   The 'chunk' refers to the a chunk of the flat array, and is defined
--   by a set of segment slices. 
--
chunk   :: USegd          -- ^ Segment descriptor of entire array.
        -> Int            -- ^ Starting offset into the flat array for the first
                          --   slice on this thread.
        -> Int            -- ^ Number of elements in this thread.
        -> Bool           -- ^ Whether this is the last thread in the gang.
        -> (# Vector Int  --   Lengths of segment slices, 
            , Int         --     segid of first slice,
            , Int #)      --     offset of first slice.

chunk !segd !nStart !nElems is_last
  = (# lens', k-left_len, left_off #)
  where
    -- Lengths of all segments.
    -- eg: [60, 10, 20, 40, 50]
    lens = lengthsUSegd segd

    -- Starting indices of all segments.
    -- eg: [0, 60, 70, 90, 130]
    idxs = indicesUSegd segd
    
    -- Total number of segments defined by segment descriptor.
    -- eg: 5
    n    = Seq.length lens

    -- The segid of the first segment on the thread.
    -- eg: for nStart = 75, 
    --              k = 2   (the third seg)
    --
    k    = search nStart idxs

    -- The segid of the first segment on the next thread.
    k'       | is_last     = n
             | otherwise   = search (nStart + nElems) idxs

    left     | k == n      = nElems
             | otherwise   = min ((idxs ! k) - nStart) nElems

    right    | k' == k     = 0
             | otherwise   = nStart + nElems - (idxs ! (k'-1))

    left_len | left == 0   = 0
             | otherwise   = 1

    left_off | left == 0   = 0
             | otherwise   = nStart - idxs ! (k-1)

    n' = left_len + (k'-k)

    !lens' 
     = runST (do
            mlens' <- Seq.newM n'

            when (left /= 0) 
             $ Seq.write mlens' 0 left

            Seq.copy (Seq.mdrop left_len mlens')
                     (Seq.slice lens k (k'-k))

            when (right /= 0)
             $ Seq.write mlens' (n' - 1) right

            Seq.unsafeFreeze mlens')


search :: Int -> Vector Int -> Int
search !x ys = go 0 (Seq.length ys)
  where
    go i n | n <= 0        = i
           | ys ! mid < x  = go (mid + 1) (n - half - 1)
           | otherwise     = go i half
      where
        half = n `shiftR` 1
        mid  = i + half



-------------------------------------------------------------------------------
joinSegdD :: Gang -> Dist USegd -> USegd
{-# INLINE_DIST joinSegdD #-}
joinSegdD g = lengthsToUSegd
           . joinD g unbalanced
           . mapD g lengthsUSegd


splitSD :: Unbox a => Gang -> Dist USegd -> Vector a -> Dist (Vector a)
{-# INLINE_DIST splitSD #-}
splitSD g dsegd xs = splitAsD g (elementsUSegdD dsegd) xs

{-# RULES

"splitSD/splitJoinD" forall g d f xs.
  splitSD g d (splitJoinD g f xs) = f (splitSD g d xs)

"splitSD/Seq.zip" forall g d xs ys.
  splitSD g d (Seq.zip xs ys) = zipWithD g Seq.zip (splitSD g d xs)
                                             (splitSD g d ys)

  #-}
