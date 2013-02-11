{-# LANGUAGE MagicHash, BangPatterns, ParallelArrays, TypeOperators #-}

module Handvec ( hsplit_v, Point, Line )
where

import Points2D.Types

import Data.Array.Parallel as PA
import Data.Array.Parallel.Base as B
import Data.Array.Parallel.PArray ()
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PDataInstances
import Data.Array.Parallel.PArray.ScalarInstances
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Prelude as P'
import Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Unlifted as U
import GHC.Exts

import Debug.Trace as Debug

import Prelude as P


hsplit_v :: PArray Point :-> Line :-> PArray Point
hsplit_v = closure2 hsplit_s hsplit_l
{-# NOINLINE hsplit_v #-}


-- | Scalar version of hsplit (unused).
hsplit_s :: PArray Point -> Line -> PArray Point
hsplit_s ps _ = ps
{-# NOINLINE hsplit_s #-}


-- | The heart of QuickHull.
--
-- Wrapper for lifted hsplit with the type expected by the vectoriser
-- (which we still go through to avoid hand vectorising 'HandvecWrp.quickHull'.
hsplit_l :: PArray (PArray Point) -> PArray Line -> PArray (PArray Point)
hsplit_l (PArray _ points_s) (PArray nlines# lines)
  = let result@(PNested segd _) = hsplit_l' nlines# points_s lines
        !(I# nsegs#) = U.lengthSegd segd                        -- should be 2
    in  PArray nsegs# result
{-# INLINE hsplit_l #-}


-- | Actually the heart of QuickHull.
hsplit_l' :: Int# -> PData (PArray Point) -> PData Line -> PData (PArray Point)
hsplit_l' nlines# (PNested segd _) _
  | Debug.traceEvent ("GANG[1/1] Issuing hsplit_l' " ++
                      "with " ++ show (I# nlines#) ++ " lines " ++
                      "and " ++ show (U.elementsSegd segd) ++ " points, " ++
                      "distributed as: " ++ show (U.toList $ U.lengthsSegd segd)) False = undefined
hsplit_l' 0# _ _ = PNested U.emptySegd (P_2 (PDouble U.empty) (PDouble U.empty))
hsplit_l' nlines# points_s                                     -- E.g.: nlines#: 6, npts >= 13
          lines@(P_2 (P_2 (PDouble line_x1s) (PDouble line_y1s))
                     (P_2 (PDouble line_x2s) (PDouble line_y2s)))
  = let -- Find distance-like measures between each point and its respective line.
        cross_s     = calc_cross_s  points_s lines

        -- Throw out all the points which are below the line (i.e. distance <= 0).
        above_s@(PNested aboveSegd (P_2 (PDouble xos) (PDouble yos)))
                    = calc_above_s points_s cross_s

        -- For some of the lines there are no more points left (segment length 0).
        -- The recursion for these lines has finished and we'll treat them separately.
        (aboveSegdLens,_,_) = unpackSegd aboveSegd          -- Segd ( [4, 5, 0, 2, 2,  0]
                                                            --      , [0, 4, 9, 9, 11, 13]
                                                            --      , 13 )

        -- Empty segments selector (empty -> 1; otherwise -> 0)
        selE        = U.tagsToSel2                          -- Sel2 ( [0, 0, 1, 0, 0, 1]
                    $ U.map B.fromBool                      --      , [0, 1, 0, 2, 3, 1]
                    $ U.zipWith (P.==) aboveSegdLens        --      , 4, 2 )
                                       (U.replicate (I# nlines#) 0)
        tagsE       = U.tagsSel2 selE                       -- [0, 0, 1, 0, 0, 1]

        --- *** First take care of the lines for which the recursion hasn't finished *** ---
        --- ***                                                                      *** ---

        -- Create segd for the points above those lines, which still have points above them.
        -- This is equivalent to removing all empty segments from 'aboveSegd'.
        segdNE      = U.lengthsToSegd                       -- Segd ( [4, 5, 2, 2]
                    $ U.packByTag aboveSegdLens tagsE 0     --      , [0, 4, 9, 11]
                                                            --      , 13 )
        (lensNE, idsNE, _) = unpackSegd segdNE

        tagsE_r     = U.replicate_s aboveSegd tagsE         -- [0, 0, 0, 0,     -- 4
                                                            --  0, 0, 0, 0, 0,  -- 5
                                                            --                  -- 0
                                                            --  0, 0,           -- 2
                                                            --  0, 0]           -- 2
                                                            --                  -- 0

        -- WATCH OUT: Redundant packByTag. Basicaly remove all empty segments from 'aboveSegd',
        -- without touching the data arrays
        aboveNE@(PNested _ (P_2 (PDouble xos') (PDouble yos')))
                    = PNested segdNE (P_2 (PDouble $ U.packByTag xos tagsE_r 0)
                                          (PDouble $ U.packByTag yos tagsE_r 0))

        --- *** Prepare segd to call hsplit anew *** ---
        --- ***                                  *** ---

        -- Number of non-empty segments
        !nNE@(I# nNE#) = U.elementsSel2_0 selE              -- 4

        -- New segd with one element per non-empty segment of 'aboveSegd' (farthest point)
        segdNEx1    = U.mkSegd (U.replicate nNE 1)          -- Segd ( [1, 1, 1, 1]
                               (U.enumFromStepLen 0 1 nNE)  --      , [0, 1, 3, 4]
                               nNE                          --      , 4 )
        -- New segd with two elements per non-empty segment of 'aboveSegd'.
        -- It's used indirectly in multiple places to denote the fact that we get two lines
        -- for each line for which the recursion hasn't finished. They share the point farthest
        -- from the old line.
        segdNEx2    = segdNEx1 `U.plusSegd` segdNEx1        -- Segd ( [2, 2, 2, 2]
                                                            --      , [0, 2, 4, 8]
        (_,_,eltsNEx2) = unpackSegd segdNEx2                --      , 8 )


        -- Replicate each non-empty segment twice
        lens_dbl      = U.replicate_s segdNEx2 lensNE       -- Segd ( [4, 4, 5, 5, 2, 2, 2, 2]
        segd_dbl      = U.lengthsToSegd lens_dbl            --      , [0, 4, 8,13,18,20,22,24]
        (_,_,elts_dbl)= unpackSegd segd_dbl                 --      ,  26 )

        -- Find indices to take from the points array to make it adhere to the doubled segd_dbl
        -- (vsegs to the rescue?)
        ids_to_take   = U.enumFromStepLenEach elts_dbl                       -- 26 (length hint)
                                              (U.replicate_s segdNEx2 idsNE) -- [0, 0, 4, 4, 9, 9,11,11]
                                              (U.replicate   eltsNEx2 1)     -- [1, 1, 1, 1, 1, 1, 1, 1]
                                              lens_dbl                       -- [4, 4, 5, 5, 2, 2, 2, 2]
        -- ids_to_take: [0,1,2,3, 0,1,2,3, 4,5,6,7,8, 4,5,6,7,8, 9,10, 9,10, 11,12, 11,12]

        -- Actually perform doubling of all segments.
        -- Each line now has two identical segments for the points above it.
        above_dbl   = PNested segd_dbl (P_2 (PDouble $ U.bpermute xos' ids_to_take)
                                            (PDouble $ U.bpermute yos' ids_to_take))

        -- Take a moment to find points farthest from each line
        fars@(P_2 (PDouble far_xs) (PDouble far_ys))
                    = calc_farthest_s points_s cross_s tagsE

        -- Find lines to and from the farthest points (i.e. [:(p1, pm), (pm, p2):] in the original code).
        -- Remember we are only dealing with lines for which there are still points above them.
        -- The use of segments here is a convenient way to interleave the old line starts, with the
        -- farthest points (also used as line starts in the next iteration).
        -- segdNEx2 is not used directly and is merely a hint to segmented append.
        line_1s'    = P_2 (PDouble $ U.append_s segdNEx2
                                                segdNEx1 (U.packByTag line_x1s tagsE 0)
                                                segdNEx1 far_xs)

                          (PDouble $ U.append_s segdNEx2
                                                segdNEx1 (U.packByTag line_y1s tagsE 0)
                                                segdNEx1 far_ys)

        -- Line ends are interleaved in a similar manner.
        line_2s'    = P_2 (PDouble $ U.append_s segdNEx2
                                                segdNEx1 far_xs
                                                segdNEx1 (U.packByTag line_x2s tagsE 0))

                          (PDouble $ U.append_s segdNEx2
                                                segdNEx1 far_ys
                                                segdNEx1 (U.packByTag line_y2s tagsE 0))
        lines'      = P_2 line_1s' line_2s'                 -- 8 lines in total

        -- Finally make recursive call to compute convex hull from (when the recursion hasn't finished)
        !(I# eltsNEx2#) = eltsNEx2
        hullNE@(PNested hullNE_segd (P_2 (PDouble hullNE_xs) (PDouble hullNE_ys)))
                    = hsplit_l' eltsNEx2# above_dbl lines'

        -- hsplit returns *only* line starts. In the example above we had 4 lines with points above them
        (hullNE_lens,_,_) = unpackSegd hullNE_segd          -- Segd: [ 2, 1, 2, 2, 1, 1, 2, 1 ]
                                                            --                    ^          ^
                                                            -- Lines with finished recursion each contribute
                                                            -- one pt to hull. They go where ^ points.

        --- *** Now deal with the empty segnments, i.e. lines which have no points above them *** ---
        --- *** and will thus form part of the convex hull                                    *** ---
        !nE@(I# nE#)= U.elementsSel2_1 selE                -- 2 lines have no more points above them

        -- Prepare the final segd to hold both the lines that have no above points as well as the remainder
        -- of the convex hull returned by the recursive call.
        result_segd = U.lengthsToSegd                       -- Segd: [3, 4, 1, 2, 3, 1]
                    $ U.combine2 (U.tagsSel2 selE)          -- [0, 0, 1, 0, 0, 1] (actually tagsE)
                                 (U.repSel2  selE)          -- Distribution across PEs
                                 (U.sum_s segdNEx2 hullNE_lens)   -- [3, 4, 2, 3]
                                 (U.replicate nE 1)               -- [1, 1]

        result_sel  = U.tagsToSel2                          -- [0,0,0, 0,0,0,0, 1, 0,0, 0,0,0, 1]
                    $ (U.replicate_s result_segd tagsE)
        result_tags = U.tagsSel2  result_sel                -- As above
        result_repsel = U.repSel2 result_sel                -- Distribution across PEs

        -- Combine data returned from 'hsplit' and the starts of the lines we left alone.
        result_xs   = U.combine2 result_tags
                                 result_repsel
                                 hullNE_xs
                                 (U.packByTag line_x1s tagsE 1)
        result_ys   = U.combine2 result_tags
                                 result_repsel
                                 hullNE_ys
                                 (U.packByTag line_y1s tagsE 1)

    in  (PNested result_segd (P_2 (PDouble result_xs) (PDouble result_ys)))
{-# INLINE hsplit_l' #-}

unpackSegd :: Segd -> (Array Int, Array Int, Int)
unpackSegd segd = ( U.lengthsSegd  segd
                  , U.indicesSegd  segd
                  , U.elementsSegd segd )
{-# INLINE unpackSegd #-}


-- | Find (relative) distances of points from line.
--
-- Each point can be above (positive distance) or below (negative distance)
-- a line as looking from the center of the convex hull.
--
-- Corresponds to 'cross' in the original program
-- > cross  = [: distance p line | p <- points :]
calc_cross_s :: PData (PArray Point) -> PData Line -> PData (PArray Double)
calc_cross_s (PNested segd (P_2 (PDouble xos) (PDouble yos)))
             (P_2 (P_2 (PDouble x1s) (PDouble y1s))
                  (P_2 (PDouble x2s) (PDouble y2s)))
  = let crosses = U.zipWith6 distance xos
                                       yos
                                       (U.replicate_s segd x1s)
                                       (U.replicate_s segd y1s)
                                       (U.replicate_s segd x2s)
                                       (U.replicate_s segd y2s)
    in PNested segd (PDouble crosses)
{-# INLINE calc_cross_s #-}


-- | Calculate cross product between vectors formed between a point and
--   each of the two line ends.
--
-- I.e. |x1-xo|   |x2-xo|
--      |     | x |     | = (x1-xo)(y2-yo)-(y1-yo)(x2-xo)
--      |y1-yo|   |y2-yo|
--
-- Not changed from the original source version thanks to vectavoid
-- (except tuples are dissolved).
distance :: Double -> Double -- Point
         -> Double -> Double -- Line start
         -> Double -> Double -- Line end
         -> Double          -- Distance
distance xo yo x1 y1 x2 y2
  = (x1 P.- xo) P.* (y2 P.- yo) P.- (y1 P.- yo) P.* (x2 P.- xo)
{-# INLINE distance #-}


-- | Find points above the lines given distance-like measures
--
-- Corresponds to 'packed' in the original program
-- > packed = [: p | (p,c) <- zipP points cross, c D.> 0.0 :]
calc_above_s :: PData (PArray Point) -> PData (PArray Double) -> PData (PArray Point)
calc_above_s (PNested segd (P_2 (PDouble xos) (PDouble yos))) -- points_s
             (PNested _    (PDouble distances))                -- cross_s
  = let length    = U.elementsSegd segd

        -- Compute selector for positive elements ((>0) -> 1; otherwise -> 0)
        selAbove  = U.tagsToSel2
                  $ U.map B.fromBool
                  $ U.zipWith (P.>) distances (U.replicate length 0.0)

        -- Compute segd for just the positive elements
        segdAbove = U.lengthsToSegd (U.count_s segd (tagsSel2 selAbove) 1)

        -- Get the actual points corresponding to positive elements
        tagsAbove = U.tagsSel2 selAbove
        xosAbove  = U.packByTag xos tagsAbove 1
        yosAbove  = U.packByTag yos tagsAbove 1

    in  (PNested segdAbove (P_2 (PDouble xosAbove) (PDouble yosAbove)))
{-# INLINE calc_above_s #-}


-- | For each line find a point fartherst from that line.
--
-- Each segment is a collection of points above a certain line.
-- The array of Doubles gives (relative) distances of points from the line.
--
-- Corresponds to 'pm' in the original program:
-- > pm = points !: maxIndexP cross
calc_farthest_s :: PData (PArray Point) -> PData (PArray Double) -> Array Tag -> PData Point
calc_farthest_s (PNested ptsSegd   (P_2 (PDouble x1s) (PDouble y1s))) -- points_s
                (PNested distSegd (PDouble distances))                -- cross_s
                tags
  = let -- Index the distances array, and find the indices corresponding to the
        -- largest distance in each segment
        indexed  = U.zip (U.indices_s distSegd) distances
        max_ids  = U.fsts (U.fold1_s maxSnd distSegd indexed)

        -- Find indices to take from the points array by offsetting from segment starts
        ids      = U.zipWith (P.+) (U.indicesSegd ptsSegd)
                                   max_ids
        max_xs   = U.bpermute x1s ids
        max_ys   = U.bpermute y1s ids
        -- however we are only interested in the ones which are from the non-empty segments
        -- (TODO revise comment)
    in  P_2 (PDouble $ U.packByTag max_xs tags 0)
            (PDouble $ U.packByTag max_ys tags 0)
{-# INLINE calc_farthest_s #-}


-- | Find pair with the bigest second element.
-- from dph-lifted-copy:D.A.P.Prelude.Int
maxSnd :: P.Ord b => (a, b) -> (a, b) -> (a, b)
maxSnd (i,x) (j,y) | x P.>= y    = (i,x)
                   | P.otherwise = (j,y)
{-# INLINE maxSnd #-}
