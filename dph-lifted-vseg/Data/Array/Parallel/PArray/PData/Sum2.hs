#include "fusion-phases.h"
module Data.Array.Parallel.PArray.PData.Sum2 where

import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Int
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Unlifted             as U
import qualified Data.Vector                    as V
import Text.PrettyPrint
import Prelude                                  as P
import Debug.Trace


-------------------------------------------------------------------------------
data instance PData (Sum2 a b)
        = PSum2  U.Sel2
                 (PData a)
                 (PData b)

data instance PDatas (Sum2 a b)
        = PSum2s (V.Vector U.Sel2)
                 (PDatas Tag)
                 (PDatas a)
                 (PDatas b)


-- PR -------------------------------------------------------------------------
-- This stuff isn't implemented yet.
nope str   = error $ "Data.Array.Parallel.PData.Sum2: no PR method for Sum2 " ++ str


instance (PR a, PR b) => PR (Sum2 a b)  where

  {-# INLINE_PDATA validPR #-}
  validPR _
        = True


  {-# INLINE_PDATA nfPR #-}
  nfPR (PSum2 sel xs ys)
        = sel `seq` nfPR xs `seq` nfPR ys `seq` ()

  {-# INLINE_PDATA coversPR #-}
  coversPR weak (PSum2 sel _ _) ix
   | weak       = ix <= U.length (U.tagsSel2 sel)
   | otherwise  = ix <  U.length (U.tagsSel2 sel)

  
  {-# NOINLINE pprpPR #-}
  pprpPR xx
   = case xx of
        Alt2_1 x -> text "Alt2_1" <+> parens (pprpPR x)
        Alt2_2 y -> text "Alt2_2" <+> parens (pprpPR y)


  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PSum2 sel pdatas1 pdatas2)
   =   text "PSum2"
   $+$ (nest 4 $ vcat
        [ pprp sel
        , text "ALTS0: " <+> pprp pdatas1
        , text "ALTS1: " <+> pprp pdatas2])


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PSum2 (U.mkSel2 U.empty U.empty 0 0 (U.mkSelRep2 U.empty)) emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR n aa
   = case aa of
      Alt2_1 x  
       -> PSum2 (U.mkSel2 (U.replicate n 0)
                          (U.enumFromStepLen 0 1 n)
                          n 0
                         (U.mkSelRep2 (U.replicate n 0)))
                (replicatePR n x)
                emptyPR
        
      Alt2_2 x
       -> PSum2 (U.mkSel2 (U.replicate n 1)
                          (U.enumFromStepLen 0 1 n)
                          0 n
                          (U.mkSelRep2 (U.replicate n 1)))
                emptyPR
                (replicatePR n x)    

                
  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR
        = nope "replicates"


  {-# INLINE_PDATA appendPR #-}
  appendPR (PSum2 sel1 as1 bs1)
           (PSum2 sel2 as2 bs2)
    = let !sel  = U.tagsToSel2
                $ U.tagsSel2 sel1 U.+:+ U.tagsSel2 sel2
      in  PSum2 sel (appendPR as1 as2)
                    (appendPR bs1 bs2)
        
        
  {-# INLINE_PDATA appendsPR #-}
  appendsPR
        = nope "appends"


  -- Projections --------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PSum2 sel _ _)
        = U.length $ tagsSel2 sel
  
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PSum2 sel as bs) i
   = let !k = U.indicesSel2 sel U.!: i
     in  case U.tagsSel2 sel U.!: i of
             0 -> Alt2_1 (indexPR as k)
             _ -> Alt2_2 (indexPR bs k)
           
  {-# INLINE_PDATA indexlPR #-}
  indexlPR 
        = nope "indexl"


  -- extract / extracts 
  -- Extract a range of elements from an array of Sum2s.
  -- Example:
  --  arr:         [L 20, R 30, L 40, R 50, R 60, R 70, L 80, R 90, L 100]
  --                            -----------------------------
  --  Sel2:
  --   TAGS:       [0     1     0     1     1     1     0     1     0]
  --   INDICES:    [0     0     1     1     2     3     2     4     3]
  --   ALTS0: PInt [20 40 80 100]
  --   ALTS1: PInt [30 50 60 70 90]
  --
  -- Goal: extract arr 2 5
  --            =  [L 40, R 50, R 60, R 70, L 80]
  --  Sel2: 
  --   TAGS:       [0     1     1     1     0]
  --   INDICES:    [0     0     1     2     1]
  --   ALTS0: PInt [40 80]
  --   ALTS1: PInt [50 60 70]
  --  
  {-# INLINE_PDATA extractPR #-}
  extractPR  (PSum2 sel as bs) start len
   = let 
         -- Extract the tags of the result elements,
         --  and rebuild the result selector indices based on these tags.
         -- This is the selelector for the result array.
         --  TAGS:    [0     1     1     1     0]
         --  INDICES: [0     0     1     2     1]
         tags'      = U.extract (U.tagsSel2 sel) start len
         sel'       = U.tagsToSel2 tags'
         
         -- Extract the indices of the data elements that we want.
         -- These are the indices of the elements in their source arrays.
         --  INDICES:  [1     1     2     3     2]
         indices'   = U.extract (U.indicesSel2 sel) start len

         -- Build maps of which source index to get the data for each ALT array.
         --  indices0: [1 2]
         --  indices1: [1 2 3]
         indices0   = U.packByTag indices' tags' 0
         indices1   = U.packByTag indices' tags' 1
         
         -- Copy source data into new ALT arrays.
         --  as:       [40 80]
         --  bs:       [50 60 70]
         as'        = bpermutePR as indices0
         bs'        = bpermutePR bs indices1

     in  PSum2 sel' as' bs'
  
  
  -- Extract several ranges of elements form some arrays of Sum2s.
  -- Example:
  --  arrs:    0: [L 20, R 30, L 40]
  --               ----------1 ----3
  --           1: [R 50, R 60, R 70, L 80, R 90, L 100]
  --               ----4 ----------------0 -----------2
  -- 
  --  Sel2
  --   0 TAGS:    [0     1     0]
  --     INDICES: [0     0     1]
  --               -------1 ----3
  --     ALTS0:   [20 40]
  --     ALTS1:   [30]
  --
  --   1 TAGS:    [1     1      1     0     1     0]
  --     INDICES: [0     1      2     0     3     1]
  --               ----4 --------------0   --------2
  --     ALTS0    [80 100]
  --     ALTS1    [50 60 70 90]
  --
  --  Goal: extract arrs ssegd
  --           => [R 60, R 70, L 80, L 20, R 30, R 90, L 100, L 40, R 50]
  --               ----------------0 ----------1 -----------2 ----3 ----4
  --
  --  Sel2:
  --     TAGS:    [1     1     0     0     1     1     0      0     1]
  --     INDICES: [0     1     0     1     2     3     2      3     4]   
  --     ALTS0:   [80 20 100 40]
  --     ALTS1:   [60 70 30  90 50]
  --
  --  ssegd:
  --   SRCIDS:  [1 0 1 0 1]
  --   STARTS:  [1 0 4 2 0]
  --  LENGTHS:  [3 2 2 1 1]
  -- 
  {-# INLINE_PDATA extractsPR #-}
  extractsPR (PSum2s sels (PInts tagss) pdatas0 pdatas1) ssegd
   = let                
         sources        = U.sourcesSSegd ssegd
         starts         = U.startsSSegd  ssegd
         lengths        = U.lengthsSSegd  ssegd

         -- Extract the tags of the result elements,
         --  and rebuild the result selector indices based on these tags.
         -- tags'       = [1     1     0     0     1     1     0      0     1]
         -- sel'        = [0     1     0     1     2     3     2      3     4]   
         tags'          = uextracts tagss sources starts lengths
         sel'           = U.tagsToSel2 tags'

         -- Extract the indices of the data elements we want.
         -- These are the indices of the elements in their source arrays.
         -- (result)      [R 60, R 70, L 80, L 20, R 30, R 90, L 100, L 40, R 50]
         --                ----------------0 ----------1 -----------2 ----3 ----4
         -- indices'    = [  1     2     0     0     0     3     1      1     0 ]
         indices'       = uextracts (V.map U.indicesSel2 sels) sources starts lengths

         -- Count the number of L and R elements for each segment,
         --  then scan them to produce the starting index of each segment in the
         --  result alt data.

         --  ALTS0:       [80  20  100  40    ]     (result)
         --                --0 --1 --2  --3 .4      (segs)
         --  lens0      = [1   1   1    1   0 ]
         --  indices0   = [0   1   2    3   4 ]
         
         --  ALTS1:       [60  70   30  90     50]  (result)
         --                ------0  --1 --2 .3 --4  (segs)
         --  lens1      = [2        1   1   0  1 ]
         --  indices1   = [0        2   3   3  4 ]
         
         lens0          = U.count_ss ssegd tagss 0
         indices0       = U.scan (+) 0 lens0

         lens1          = U.count_ss ssegd tagss 1
         indices1       = U.scan (+) 0 lens1

         -- For each segment in the result alt data, get its starting index in
         -- the original alt array.
         -- 
         -- TODO: We're doing this by getting the index of EVERY element in the
         --       original array, then just selecting the indices corresponding
         --       to the start of each segment. If the last segment has length 0,
         --       then we get an index overflow problem because the last element
         --       in the indices array doesn't point to real data. There might be
         --       a better way to do this that doesn't require copying all indices,
         --       and doesn't need a bounds check.
         -- 
         -- indices0    = [ 0 1 2 3 4 ] (from above)
         -- sel0        = [ 0 0 1 1 ]       -- here, we've only got starting indices
         -- sel0_len    = 4                 --  for 4 segs, but there are 5 segs in total.
         -- starts0     = [ 0 0 1 1 0 ]
         sel0           = U.packByTag indices' tags' 0
         sel0_len       = U.length sel0
         starts0        = U.map (\i -> if i >= sel0_len then 0 else sel0 U.!: i) indices0

         -- indices1    = [ 0 2 3 3 4 ] (from above)
         -- sel1        = [ 1 2 0 3 0 ]
         -- sel1_len    = 5
         -- starts1     = [ 1 0 3 3 0 ]
         sel1           = U.packByTag indices' tags' 1
         sel1_len       = U.length sel1
         starts1        = U.map (\i -> if i >= sel1_len then 0 else sel1 U.!: i) indices1

         -- Extract the final alts data:
         -- sources     = [ 1 0 1 0 1 ] (from above)
         -- starts0     = [ 0 0 1 1 0 ] (from above)
         -- starts1     = [ 1 0 3 3 0 ] (from above)
         -- lens0       = [ 1 1 1 1 0 ] (from above)
         -- lens1       = [ 2 1 1 0 1 ] (from above)

         -- (source data)
         --  0: ALTS0:   [20  40]
         --               --1 --3
         --     ALTS1:   [30]
         --               --1 .3             (no alt1 data for seg 3)
         -- 
         --  1: ALTS0:   [80  100]
         --               --0 ---2 .4        (no alt0 data for seg 4)
         --     ALTS1:   [50  60  70  90]
         --               --4 -----0  --2

         -- (result data)
         --  ALTS0:      [80  20  100  40 ]
         --  ALTS1:      [60  70   30  90 50]

         pdata0         = extractsPR pdatas0 $ U.mkSSegd starts0 sources (U.lengthsToSegd lens0)
         pdata1         = extractsPR pdatas1 $ U.mkSSegd starts1 sources (U.lengthsToSegd lens1)

     in {- trace (render $ vcat 
                        [ text "tags'       = " <> pprp tags'
                        , text ""
                        , text "lens0       = " <> pprp lens0
                        , text "selStarts0  = " <> pprp selStarts0
                        , text "sel0        = " <> pprp sel0
                        , text "starts0     = " <> pprp starts0
                        , text ""
                        , text "lens1       = " <> pprp lens1
                        , text "selStarts1  = " <> pprp selStarts1
                        , text "sel1        = " <> pprp sel1
                        , text ""
                        , text "sources     = " <> pprp sources
                        , text "selindices' = " <> pprp selIndices'
                        , text ""]) $ -}
           PSum2 sel' pdata0 pdata1


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR
        = nope "packByTag"
  
  {-# INLINE_PDATA combine2PR #-}
  combine2PR 
        = nope "combine2"


  -- Conversions --------------------------------
  -- TODO: fix rubbish via-lists filtering.   
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR vec
   = let tags   = V.convert $ V.map tagOfSum2 vec
         sel2   = U.tagsToSel2 tags
         xs'    = fromVectorPR $ V.fromList $ [x | Alt2_1 x <- V.toList vec]
         ys'    = fromVectorPR $ V.fromList $ [x | Alt2_2 x <- V.toList vec]
         
     in  PSum2 sel2 xs' ys'
        

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR pdata@(PSum2 sel _ _)
   = let len = U.length $ U.tagsSel2 sel
     in  if len == 0
          then V.empty
          else V.map (indexPR pdata) 
                $ V.enumFromTo 0 (len - 1)


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PSum2s V.empty emptydPR emptydPR emptydPR


  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PSum2 sel2 xs ys)
        = PSum2s (V.singleton sel2)
                 (singletondPR (PInt (U.tagsSel2 sel2)))
                 (singletondPR xs)
                 (singletondPR ys)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PSum2s sel2s _ _ _)
        = V.length sel2s


  {-# INLINE_PDATA indexdPR #-}
  indexdPR  (PSum2s sel2s _ xss yss) ix
        = PSum2  (V.unsafeIndex sel2s ix)
                 (indexdPR      xss   ix)
                 (indexdPR      yss   ix)


  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PSum2s sels1 tagss1 xss1 yss1)
            (PSum2s sels2 tagss2 xss2 yss2)
   = PSum2s (sels1  V.++        sels2)
            (tagss1 `appenddPR` tagss2)
            (xss1   `appenddPR` xss2)
            (yss1   `appenddPR` yss2)


  {-# INLINE_PDATA concatdPR #-}
  concatdPR
        = nope "concatdPR"
                

  -- TODO: fix rubbish via-lists conversion.
  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
   = let   (sels, pdatas1, pdatas2) 
                   = P.unzip3 
                   $ [ (sel, pdata1, pdata2) 
                                    | PSum2 sel pdata1 pdata2 <- V.toList vec]
     in    PSum2s  (V.fromList sels)
                   (PInts $ V.map U.tagsSel2 $ V.fromList sels)
                   (fromVectordPR $ V.fromList pdatas1)
                   (fromVectordPR $ V.fromList pdatas2)
                

  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PSum2s sels _ pdatas1 pdatas2)
   = let  vecs1 = toVectordPR pdatas1
          vecs2 = toVectordPR pdatas2
          
     in   V.zipWith3 PSum2 sels vecs1 vecs2


-- Pretty ---------------------------------------------------------------------
instance PprPhysical U.Sel2 where
 pprp sel2
  =   text "Sel2"
  $+$ (nest 4 $ vcat
       [ text "TAGS:   " <+> text (show $ U.toList $ U.tagsSel2 sel2)
       , text "INDICES:" <+> text (show $ U.toList $ U.indicesSel2 sel2)])


