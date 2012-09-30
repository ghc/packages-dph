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

--type Tag = Bool -- from dph-prim-seq:D.A.P.Base

-- not changed thanks to vectavoid (except tuples are dissolved)
distance :: Double -> Double -- Point
         -> Double -> Double -- Line start
         -> Double -> Double -- Line end
         -> Double          -- Distance
distance xo yo x1 y1 x2 y2
  = (x1 P.- xo) P.* (y2 P.- yo) P.- (y1 P.- yo) P.* (x2 P.- xo)
{-# INLINE distance #-}

hsplit_v :: PArray Point :-> Line :-> PArray Point
hsplit_v = closure2 hsplit_s hsplit_l_wrp
{-# NOINLINE hsplit_v #-}

-- scalar version can be generalised to lifted hsplit:
-- 1. construct a nested array out of $points$ array (single segment)
-- 2. construct a singleton array containing $line$
hsplit_s :: PArray Point -> Line -> PArray Point
hsplit_s ps@(PArray n# pts) line
  = let segd     = U.singletonSegd (I# n#)
        pts_s    = PArray 1# (PNested segd pts)
        lines    = PArray 1# (toPData line)
        hull@(PArray _ (PNested segd' hull_pts))
                 = hsplit_l_wrp pts_s lines
        (I# n'#) = U.elementsSegd segd'
    in Debug.trace "hsplit_s" $ PArray n'# hull_pts
  where toPData ((x1,y1), (x2,y2)) = P_2 (P_2 (r x1) (r y1))
                                         (P_2 (r x2) (r y2))
        r x = PDouble (U.replicate 1 x)
{-# NOINLINE hsplit_s #-}

hsplit_l_wrp :: PArray (PArray Point) -> PArray Line -> PArray (PArray Point)
hsplit_l_wrp (PArray n# pts) (PArray _ lns)
  = let result@(PNested segd _) = hsplit_l_wrk n# pts lns
        !(I# n'#) = U.elementsSegd segd
    in  PArray n'# result
{-# INLINE hsplit_l_wrp #-}

hsplit_l_wrk :: Int# -> PData (PArray Point) -> PData Line -> PData (PArray Point)
hsplit_l_wrk n# (PNested segd (P_2 (PDouble xos) _)) (P_2 (P_2 (PDouble x1s) _) _)
  | Debug.trace ("hsplit_l_wrk: "++
                 "nlines#: "     ++ show (I# n#) ++
                 ", eltsSegd: "  ++ show (U.elementsSegd segd) ++
                 ", lensSegd: "  ++ show (U.toList (U.lengthsSegd segd)) ++
                 ", npts: "      ++ show (U.length xos) ++
                 ", nlines arr: "++ show (U.length x1s)) False = undefined
hsplit_l_wrk 0# _ _ = PNested U.emptySegd (P_2 (PDouble U.empty) (PDouble U.empty))
hsplit_l_wrk nlines# points_s                                     -- nlines#: 6, npts >= 13
         lines@(P_2 (P_2 (PDouble line_x1s) (PDouble line_y1s))
                    (P_2 (PDouble line_x2s) (PDouble line_y2s)))
  = let -- find distance-like measures between each point and its respective line
        cross_s     = calc_cross_s  points_s lines
        -- throw out all the points which are below the line (i.e. distance <= 0)
        packed_s@(PNested segd (P_2 (PDouble xos) (PDouble yos)))
                    = calc_packed_s points_s cross_s
        -- for some of the lines there are no more points left (segment length 0)
        -- the recursion for these lines has finished and we'll treat them separately
        ( segd_lens, _, _) = getSegdParams segd             -- Segd ( [4, 5, 0, 2, 2,  0]
                                                            --      , [0, 4, 9, 9, 11, 13]
                                                            --      , 13 )
        -- empty segments selector ((==0) -> 1; otherwise -> 0)
        selE        = U.tagsToSel2                          -- Sel2 ( [0, 0, 1, 0, 0, 1]
                    $ U.map B.fromBool                      --      , [0, 1, 0, 2, 3, 1]
                    $ U.zipWith (P.==) segd_lens            --      , 4, 2 )
                                    (U.replicate (I# nlines#) 0)
        tagsE       = U.tagsSel2 selE                       -- [0, 0, 1, 0, 0, 1]
        --- *** First take care of the lines for which the recursion hasn't finished *** ---
        -- number of non-empty segments
        !nNE@(I# nNE#) = U.elementsSel2_0 selE              -- 4
        -- new segd with one element per non empty segments of original segd
        segdNEx1    = U.mkSegd (U.replicate nNE 1)          -- Segd ( [1, 1, 1, 1]
                               (U.enumFromStepLen 0 1 nNE)  --      , [0, 1, 3, 4]
                               nNE                          --      , 4 )
        segdNEx2    = segdNEx1 `U.plusSegd` segdNEx1        -- Segd ( [2, 2, 2, 2]
                                                            --      , [0, 2, 4, 8]
                                                            --      , 8 )
        (lensx2,idsx2,neltsx2)
                    = getSegdParams segdNEx2
        -- recreate segdNE for some reason
        segdNE      = U.lengthsToSegd                       -- Segd ( [4, 5, 2, 2]
                    $ U.packByTag segd_lens tagsE 0         --      , [0, 4, 9, 11]
                                                            --      , 13)
        (lensNE,idsNE,_)
                    = getSegdParams segdNE
        tagsE_r     = U.replicate_s segd tagsE              -- [0, 0, 0, 0,     -- 4
                                                            --  0, 0, 0, 0, 0,  -- 5
                                                            --                  -- 0
                                                            --  0, 0,           -- 2
                                                            --  0, 0]           -- 2
                                                            --                  -- 0
        -- WATCH OUT: redundant packByTag. Basicaly remove all empty segments from segd
        packedNE@(PNested _ (P_2 (PDouble xos') (PDouble yos')))
                    = PNested segdNE (P_2 (PDouble $ U.packByTag xos tagsE_r 0)
                                          (PDouble $ U.packByTag yos tagsE_r 0))
        -- prepare segd to call hsplit anew
        lens_r      = U.replicate_s segdNEx2 lensNE        -- Segd ( [4, 4, 5, 5, 2, 2, 2, 2]
        segd_r      = U.lengthsToSegd lens_r               --      , [0, 4, 8,13,18,20,22,24]
        (_,ixs_r,elts_r) = getSegdParams segd_r            --      ,  26
        ids_to_take = U.enumFromStepLenEach elts_r                         -- 26
                                            (U.replicate_s segdNEx2 idsNE) -- [0, 0, 4, 4, 9, 9,11,11]
                                            (U.replicate   neltsx2  1)     -- [1, 1, 1, 1, 1, 1, 1, 1]
                                            lens_r                         -- [4, 4, 5, 5, 2, 2, 2, 2]
        -- ids_to_take: [0,1,2,3, 0,1,2,3, 4,5,6,7,8, 4,5,6,7,8, 9,10, 9,10, 11,12, 11,12]
        packed_r@(PNested _ (P_2 (PDouble xos_r) (PDouble yos_r)))
                    = PNested segd_r (P_2 (PDouble $ U.bpermute xos' ids_to_take)
                                          (PDouble $ U.bpermute yos' ids_to_take))
        pm_s@(P_2 (PDouble pm_xs) (PDouble pm_ys))
                    = calc_pm_s points_s cross_s tagsE
        line_1s'    = P_2 (PDouble $ U.append_s segdNEx2
                                                segdNEx1
                                                (U.packByTag line_x1s tagsE 0)
                                                segdNEx1
                                                pm_xs)
                          (PDouble $ U.append_s segdNEx2
                                                segdNEx1
                                                (U.packByTag line_y1s tagsE 0)
                                                segdNEx1
                                                pm_ys)
        line_2s'    = P_2 (PDouble $ U.append_s segdNEx2
                                                segdNEx1
                                                pm_xs
                                                segdNEx1
                                                (U.packByTag line_x2s tagsE 0))
                          (PDouble $ U.append_s segdNEx2
                                                segdNEx1
                                                pm_ys
                                                segdNEx1
                                                (U.packByTag line_y2s tagsE 0))
        lines'      = P_2 line_1s' line_2s'
        -- finally make recursive call to compute convect hull from (when the recursion hasn't finished)
        hullNE@(PNested hullNE_segd (P_2 (PDouble hullNE_xs) (PDouble hullNE_ys)))
                    = hsplit_l_wrk nNE# packed_r lines'
        (hullNE_lens, hullNE_ids, hullNE_nelts)
                    = getSegdParams hullNE_segd
        --- *** now deal with the empty segnments, i.e. lines which have no points above them *** ---
        --- *** and will thus form part of the convex hull                                    *** ---
        !nE@(I# nE#) = U.elementsSel2_1 selE                -- 2
        result_segd = U.lengthsToSegd
                    $ U.combine2 (U.tagsSel2 selE)
                                 (U.repSel2  selE)
                                 (U.sum_s segdNEx2 hullNE_lens) --
                                 (U.replicate nE 1)        -- [1, 1]
        result_sel  = U.tagsToSel2 (U.replicate_s result_segd tagsE)
        result_tags = U.tagsSel2  result_sel
        result_repsel = U.repSel2 result_sel
        result_xs   = U.combine2 result_tags
                                 result_repsel
                                 hullNE_xs
                                 (U.packByTag line_x1s tagsE 1)
        result_ys   = U.combine2 result_tags
                                 result_repsel
                                 hullNE_ys
                                 (U.packByTag line_y1s tagsE 1)
    in  (PNested result_segd (P_2 (PDouble result_xs) (PDouble result_ys)))
{-# INLINE hsplit_l_wrk #-}

getSegdParams :: Segd -> (Array Int, Array Int, Int)
getSegdParams segd = ( U.lengthsSegd  segd
                     , U.indicesSegd segd
                     , U.elementsSegd segd )
{-# INLINE getSegdParams #-}

calc_cross_s :: PData (PArray Point) -> PData Line -> PData (PArray Double)
calc_cross_s (PNested segd (P_2 (PDouble xos) (PDouble yos)))
             (P_2 (P_2 (PDouble x1s) (PDouble y1s))
                  (P_2 (PDouble x2s) (PDouble y2s)))
  = let distances = U.zipWith6 distance xos
                                        yos
                                        (U.replicate_s segd x1s)
                                        (U.replicate_s segd y1s)
                                        (U.replicate_s segd x2s)
                                        (U.replicate_s segd y2s)
    in PNested segd (PDouble distances)
{-# INLINE calc_cross_s #-}

calc_packed_s :: PData (PArray Point) -> PData (PArray Double) -> PData (PArray Point)
calc_packed_s (PNested segd (P_2 (PDouble xos) (PDouble yos)))
              (PNested _    (PDouble cross))
  = let nelts  = U.elementsSegd segd
        -- compute selector for positive elements ((>0) -> 1; otherwise -> 0)
        selGZ  = U.tagsToSel2
               $ U.map B.fromBool
               $ U.zipWith (P.>) cross (U.replicate nelts 0.0)
        -- compute segd for just the positive elements
        segdGZ = U.lengthsToSegd (U.count_s segd (tagsSel2 selGZ) 1)
        -- get the actual points corresponding to positive elements
        tagsGZ = U.tagsSel2 selGZ
        xosGZ  = U.packByTag xos tagsGZ 1
        yosGZ  = U.packByTag yos tagsGZ 1
    in  (PNested segdGZ (P_2 (PDouble xosGZ) (PDouble yosGZ)))
{-# INLINE calc_packed_s #-}

calc_pm_s :: PData (PArray Point) -> PData (PArray Double) -> Array Tag -> PData Point
calc_pm_s (PNested segd_pts (P_2 (PDouble x1s) (PDouble y1s))) -- points_s
          (PNested segd_crs (PDouble cross))                   -- cross_s
          tags
  = let idexed = U.zip (U.indices_s segd_crs) cross
        max_ids = U.fsts (U.fold1_s max' segd_crs idexed) 
        -- find indices to take from the points array
        ids     = U.zipWith (P.+) (U.indicesSegd segd_pts)
                                max_ids
        max_xs  = U.bpermute x1s ids
        max_ys  = U.bpermute y1s ids
        -- however we are only interested in the ones which are from the non-empty segments
        -- (TODO revise comment)
    in  P_2 (PDouble $ U.packByTag max_xs tags 0)
            (PDouble $ U.packByTag max_ys tags 0)
{-# INLINE calc_pm_s #-}

-- from dph-lifted-copy:D.A.P.Prelude.Int
max' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# INLINE max' #-}

min' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# INLINE min' #-}


